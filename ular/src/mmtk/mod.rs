pub mod object_descriptor_store;
pub mod runtime;
pub mod stack_map;

use crate::mmtk::runtime::{
    is_mutator, mmtk_copy_object, mmtk_copy_object_to, mmtk_get_object_size,
    mmtk_pause_all_mutators, mmtk_register_roots, mmtk_resume_all_mutators, mmtk_scan_object,
    mmtk_spawn_gc_thread, number_of_mutators, with_mutator, with_mutators,
};
use mmtk::{
    util::{
        copy::{CopySemantics, GCWorkerCopyContext},
        Address, ObjectReference, VMMutatorThread, VMThread, VMWorkerThread,
    },
    vm::{
        slot::{SimpleSlot, UnimplementedMemorySlice},
        ActivePlan, Collection, GCThreadContext, ObjectModel, ReferenceGlue, RootsWorkFactory,
        Scanning, SlotVisitor, VMBinding, VMGlobalLogBitSpec, VMLocalForwardingBitsSpec,
        VMLocalForwardingPointerSpec, VMLocalLOSMarkNurserySpec, VMLocalMarkBitSpec,
    },
    Mutator,
};
use std::ptr::NonNull;

#[derive(Default)]
pub struct UlarVM;

impl VMBinding for UlarVM {
    type VMActivePlan = UlarActivePlan;
    type VMCollection = UlarCollection;
    type VMMemorySlice = UnimplementedMemorySlice;
    type VMObjectModel = UlarObjectModel;
    type VMReferenceGlue = UlarReferenceGlue;
    type VMScanning = UlarScanning;
    type VMSlot = SimpleSlot;

    // When creating object references, the minimum alignment MMTK will accept is the length of a word
    // (see the check in `ObjectReference::from_raw_address`). Despite this, `MIN_ALIGNMENT` is set to
    // 4 bytes (32 bits) by default.
    //
    // We set it to the correct value.
    const MIN_ALIGNMENT: usize = size_of::<usize>();
}

/// The purpose of this struct is to implement [ActivePlan] from MMTk.
///
/// This tells MMTk how to create and track mutator threads, threads that can manipulate objects. See
/// [crate::mmtk::runtime] to understand how this is implemented.
pub struct UlarActivePlan;

impl ActivePlan<UlarVM> for UlarActivePlan {
    fn is_mutator(thread: VMThread) -> bool {
        is_mutator(thread)
    }

    fn mutator(thread: VMMutatorThread) -> &'static mut Mutator<UlarVM> {
        with_mutator(thread, |mutator| {
            let mut mutator_pointer = NonNull::from(mutator);

            // SAFETY: This is totally unsafe and MMTk really shouldn't be so demanding in expecting a
            // `'static mut Mutator`, but it is what it is. Let's just hope that whoever is calling
            // this function knows what they're doing.
            unsafe { mutator_pointer.as_mut() }
        })
    }

    fn mutators<'a>() -> Box<dyn Iterator<Item = &'a mut Mutator<UlarVM>> + 'a> {
        // SAFETY: This is also totally unsafe, and I don't think it's possible to implement it safely
        with_mutators(|mutators| unsafe {
            std::mem::transmute::<
                Box<dyn Iterator<Item = &Mutator<UlarVM>>>,
                Box<dyn Iterator<Item = &'a mut Mutator<UlarVM>>>,
            >(mutators)
        })
    }

    fn number_of_mutators() -> usize {
        number_of_mutators()
    }
}

/// The purpose of this struct is to implement [Collection] from MMTk.
///
/// This tells MMTk how to pause and resume mutator threads—threads that can manipulate objects—for
/// stop-the-world garbage collection.
///
/// # Collection in Ular
///
/// Threads can't be stopped and started at will, and even if they could, they need to be stopped at
/// points where it's safe to to do so. To accomplish this, we employ a cooperative model of
/// thread management. That means that the garbage collection thread, which will call the methods in
/// [Collection], will signal to mutator threads that it wants them to stop, and those threads will
/// periodically wait for such a signal (so-called *ticking*) before stopping.
///
/// There are a couple performance considerations here:
/// 1. When we signal a thread to stop, we don't want that signaling to block other threads
/// 2. We want threads to tick at the right frequency:
///     - Clock cycles spent ticking could've been spent doing other work
///     - Threads that tick less frequently may wait unnecessarily longer before stopping
///
/// We manage consideration #1 through complicated locking implemented in [crate::mmtk::runtime].
///
/// We manage consideration #2 by ticking before every object is allocated. This choice was made
/// because:
/// - It's performed frequently enough that we shouldn't have to wait *too* long
/// - It's better than many of the obvious alternatives, e.g. ticking on every function call
/// - It's easy to implement
///
/// The main concern is that we spend a long time doing work that doesn't necessitate allocation. If
/// this proves to be an issue, we may reevaluate this strategy.
pub struct UlarCollection;

impl Collection<UlarVM> for UlarCollection {
    fn block_for_gc(_tls: VMMutatorThread) {
        std::thread::park();
    }

    fn resume_mutators(_tls: VMWorkerThread) {
        mmtk_resume_all_mutators();
    }

    fn spawn_gc_thread(_tls: VMThread, context: GCThreadContext<UlarVM>) {
        mmtk_spawn_gc_thread(context);
    }

    fn stop_all_mutators<A: FnMut(&'static mut Mutator<UlarVM>)>(
        _tls: VMWorkerThread,
        mutator_visitor: A,
    ) {
        mmtk_pause_all_mutators(mutator_visitor);
    }
}

/// The purpose of this struct is to implement [ObjectModel] from MMTk.
///
/// This tells MMTk about how objects are laid out and how to perform operations like copying on them.
///
/// # Ular's Object Model
///
/// ## Object Layout
///
/// In Ular, objects are structured like so:
///
/// | Section | Size | Description |
/// | ------- | ---- | ----------- |
/// | Object descriptor reference | 4 bytes | An index locating the object's descriptor in the object descriptor store
/// | Payload | ? | The object payload
///
/// For information on object descriptors and the object descriptor store, see the [UlarScanning].
///
/// Object references (of type [ObjectReference]) refer to objects, and store the address to the
/// block of memory allocated for the object, beginning with the object descriptor reference. For that
/// reason, [ref_to_object_start], which computes the address originally returned by MMTk when the
/// object was allocated (start address), returns the object reference provided to it, unmodified.
///
/// That also explains why:
/// - [get_reference_when_copied_to] returns the destination address provided to it
/// - [OBJECT_REF_OFFSET_LOWER_BOUND], the minimum difference between an object reference and
///   start address, is 0
///
/// ## Object Metadata
///
/// MMTk needs to store metadata about objects. This data can be stored in a header on the object,
/// or in a side location managed by MMTk. For convenience, we choose to store most metadata in
/// the side location, with one exception: the forwarding pointer.
///
/// This pointer is used by MMTk during a garbage collection cycle, when it's moving an object, to
/// point to the new object. Because it's only set on dying objects, it's safe to override the
/// object payload (which begins with the object descriptor reference) with it. For that reason,
/// we store it at the start of the object.
pub struct UlarObjectModel;

impl ObjectModel<UlarVM> for UlarObjectModel {
    // Global metadata
    const GLOBAL_LOG_BIT_SPEC: VMGlobalLogBitSpec = VMGlobalLogBitSpec::side_first();

    // Local metadata
    const LOCAL_FORWARDING_POINTER_SPEC: VMLocalForwardingPointerSpec =
        VMLocalForwardingPointerSpec::in_header(0);

    // The other metadata can be put in the side metadata.
    const LOCAL_FORWARDING_BITS_SPEC: VMLocalForwardingBitsSpec =
        VMLocalForwardingBitsSpec::side_first();

    const LOCAL_MARK_BIT_SPEC: VMLocalMarkBitSpec =
        VMLocalMarkBitSpec::side_after(Self::LOCAL_FORWARDING_BITS_SPEC.as_spec());

    const LOCAL_LOS_MARK_NURSERY_SPEC: VMLocalLOSMarkNurserySpec =
        VMLocalLOSMarkNurserySpec::side_after(Self::LOCAL_MARK_BIT_SPEC.as_spec());

    const OBJECT_REF_OFFSET_LOWER_BOUND: isize = 0;

    fn copy(
        from: ObjectReference,
        semantics: CopySemantics,
        copy_context: &mut GCWorkerCopyContext<UlarVM>,
    ) -> ObjectReference {
        mmtk_copy_object(from, semantics, copy_context)
    }

    fn copy_to(from: ObjectReference, _to: ObjectReference, region: Address) -> Address {
        // SAFETY: I assume MMTk will call `copy_to` with arguments satisfying the conditions laid out
        // in the documentation for `mmtk_copy_object_to`
        unsafe { mmtk_copy_object_to(from, region) }
    }

    fn get_current_size(object: ObjectReference) -> usize {
        mmtk_get_object_size(object)
    }

    fn get_size_when_copied(object: ObjectReference) -> usize {
        Self::get_current_size(object)
    }

    fn get_align_when_copied(object: ObjectReference) -> usize {
        mmtk_get_object_size(object)
    }

    fn get_align_offset_when_copied(_object: ObjectReference) -> usize {
        0
    }

    fn get_reference_when_copied_to(_from: ObjectReference, to: Address) -> ObjectReference {
        ObjectReference::from_raw_address(to).unwrap()
    }

    fn get_type_descriptor(_reference: ObjectReference) -> &'static [i8] {
        unimplemented!()
    }

    fn ref_to_object_start(object: ObjectReference) -> Address {
        object.to_raw_address()
    }

    fn ref_to_header(object: ObjectReference) -> Address {
        object.to_raw_address()
    }

    fn dump_object(_object: ObjectReference) {
        unimplemented!()
    }
}

/// The purpose of this struct is to implement [ReferenceGlue] from MMTk.
///
/// That trait is for weak references and finalizers, neither of which we support. For that reason,
/// its implementation of [ReferenceGlue] is mostly empty.
pub struct UlarReferenceGlue;

impl ReferenceGlue<UlarVM> for UlarReferenceGlue {
    type FinalizableType = ObjectReference;

    fn clear_referent(_object: ObjectReference) {}
    fn enqueue_references(_references: &[ObjectReference], _tls: VMWorkerThread) {}
    fn get_referent(_object: ObjectReference) -> Option<ObjectReference> {
        None
    }

    fn set_referent(_reference: ObjectReference, _referent: ObjectReference) {}
}

/// The purpose of this struct is to implement [Scanning] from MMTk.
///
/// This tells MMTk how to perform root scanning and object scanning, which are both part of the
/// mark phase of mark and sweep-style garbage collectors.
///
/// # Root Scanning in Ular
///
/// Root scanning refers to the discovery of root objects, which can be searched to discover all the
/// objects currently reachable by running Ular programs. Root objects include all the objects
/// referenced in:
/// - Global variables
/// - Local variables
/// - Function parameters
///
/// ## How It Works
///
/// Ular performs root scanning by unwinding the stack (using the libunwind bindings in
/// [crate::libunwind]) and consulting the *stack map* generated by LLVM to identify values on the
/// stack that contain object references. See [crate::mmtk::stack_map] to understand how the stack map
/// is structured.
///
/// # Object Scanning in Ular
///
/// Object scanning refers to the discovery of objects referenced by a given object.
///
/// For example, if we define two structs like this:
///
/// ```ular
/// struct Teacher {
///     age: u8;
/// }
///
/// struct Student {
///     age: u8;
///     teacher: Teacher;
///     grade: u8;
/// }
/// ```
///
/// we can say every `Student` points to a `Teacher`. The responsibility of [UlarScanning], namely
/// [UlarScanning::scan_objects], is to figure out, from an object reference which other objects are
/// referenced by that object.
///
/// ## How It Works
///
/// To minimize memory usage and avoid storing duplicated information, we store information about
/// *inner object references* (relationships like `Student -> Teacher`) in a global
/// *object descriptor store*. We call this information, which is generated for every struct, an
/// *object descriptor*.
///
/// Currently, we store:
/// - Inner references (expressed in terms of offsets from the object reference)
/// - Object size
/// - Object alignment
///
/// See [crate::mmtk::object_descriptor_store] for the specifics on how this works.
///
/// We store an *object descriptor reference*, which is really just a 32-bit index into the
/// object descriptor store, at the start of every object, just before the payload. That's how we're
/// able to determine the layout of an object from its reference alone. See [UlarObjectModel] for more
/// information on object layout.
pub struct UlarScanning;

impl Scanning<UlarVM> for UlarScanning {
    fn notify_initial_thread_scan_complete(_partial_scan: bool, _tls: VMWorkerThread) {}
    fn prepare_for_roots_re_scanning() {}
    fn scan_object<A: SlotVisitor<SimpleSlot>>(
        _tls: VMWorkerThread,
        object: ObjectReference,
        slot_visitor: &mut A,
    ) {
        mmtk_scan_object(object, slot_visitor);
    }

    fn scan_roots_in_mutator_thread(
        _tls: VMWorkerThread,
        _mutator: &'static mut Mutator<UlarVM>,
        _factory: impl RootsWorkFactory<SimpleSlot>,
    ) {
    }

    fn scan_vm_specific_roots(_tls: VMWorkerThread, factory: impl RootsWorkFactory<SimpleSlot>) {
        mmtk_register_roots(factory);
    }

    fn supports_return_barrier() -> bool {
        unimplemented!()
    }
}
