pub mod runtime;

use crate::mmtk::runtime::{is_mutator, number_of_mutators, with_mutator, with_mutators};
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

/// This is the offset from the allocation result to the object reference for the object. For bindings
/// that this offset is not a constant, you can implement the calculation in the method
/// `ref_to_object_start`, and remove this constant.
pub const OBJECT_REF_OFFSET: usize = 0;

/// This is the offset from the object reference to the object header. This value is used in
/// `ref_to_header` where MMTk loads header metadata from.
pub const OBJECT_HEADER_OFFSET: usize = 0;

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

pub struct UlarCollection;

impl Collection<UlarVM> for UlarCollection {
    fn block_for_gc(_tls: VMMutatorThread) {
        unimplemented!()
    }

    fn resume_mutators(_tls: VMWorkerThread) {
        unimplemented!()
    }

    fn spawn_gc_thread(_tls: VMThread, _ctx: GCThreadContext<UlarVM>) {
        unimplemented!()
    }

    fn stop_all_mutators<A: FnMut(&'static mut Mutator<UlarVM>)>(
        _tls: VMWorkerThread,
        _mutator_visitor: A,
    ) {
        unimplemented!()
    }
}

pub struct UlarObjectModel;

impl ObjectModel<UlarVM> for UlarObjectModel {
    // Global metadata

    const GLOBAL_LOG_BIT_SPEC: VMGlobalLogBitSpec = VMGlobalLogBitSpec::side_first();

    // Local metadata

    // Forwarding pointers have to be in the header. It is okay to overwrite the object payload with a forwarding pointer.
    // FIXME: The bit offset needs to be set properly.
    const LOCAL_FORWARDING_POINTER_SPEC: VMLocalForwardingPointerSpec =
        VMLocalForwardingPointerSpec::in_header(0);

    // The other metadata can be put in the side metadata.
    const LOCAL_FORWARDING_BITS_SPEC: VMLocalForwardingBitsSpec =
        VMLocalForwardingBitsSpec::side_first();

    const LOCAL_MARK_BIT_SPEC: VMLocalMarkBitSpec =
        VMLocalMarkBitSpec::side_after(Self::LOCAL_FORWARDING_BITS_SPEC.as_spec());

    const LOCAL_LOS_MARK_NURSERY_SPEC: VMLocalLOSMarkNurserySpec =
        VMLocalLOSMarkNurserySpec::side_after(Self::LOCAL_MARK_BIT_SPEC.as_spec());

    const OBJECT_REF_OFFSET_LOWER_BOUND: isize = OBJECT_REF_OFFSET as isize;

    fn copy(
        _from: ObjectReference,
        _semantics: CopySemantics,
        _copy_context: &mut GCWorkerCopyContext<UlarVM>,
    ) -> ObjectReference {
        unimplemented!()
    }

    fn copy_to(_from: ObjectReference, _to: ObjectReference, _region: Address) -> Address {
        unimplemented!()
    }

    fn get_current_size(_object: ObjectReference) -> usize {
        unimplemented!()
    }

    fn get_size_when_copied(object: ObjectReference) -> usize {
        Self::get_current_size(object)
    }

    fn get_align_when_copied(_object: ObjectReference) -> usize {
        unimplemented!()
    }

    fn get_align_offset_when_copied(_object: ObjectReference) -> usize {
        unimplemented!()
    }

    fn get_reference_when_copied_to(_from: ObjectReference, _to: Address) -> ObjectReference {
        unimplemented!()
    }

    fn get_type_descriptor(_reference: ObjectReference) -> &'static [i8] {
        unimplemented!()
    }

    fn ref_to_object_start(object: ObjectReference) -> Address {
        object.to_raw_address().sub(OBJECT_REF_OFFSET)
    }

    fn ref_to_header(object: ObjectReference) -> Address {
        object.to_raw_address().sub(OBJECT_HEADER_OFFSET)
    }

    fn dump_object(_object: ObjectReference) {
        unimplemented!()
    }
}

pub struct UlarReferenceGlue;

impl ReferenceGlue<UlarVM> for UlarReferenceGlue {
    type FinalizableType = ObjectReference;

    fn clear_referent(_object: ObjectReference) {
        unimplemented!()
    }

    fn enqueue_references(_references: &[ObjectReference], _tls: VMWorkerThread) {
        unimplemented!()
    }

    fn get_referent(_object: ObjectReference) -> Option<ObjectReference> {
        unimplemented!()
    }

    fn set_referent(_reference: ObjectReference, _referent: ObjectReference) {
        unimplemented!()
    }
}

pub struct UlarScanning;

impl Scanning<UlarVM> for UlarScanning {
    fn notify_initial_thread_scan_complete(_partial_scan: bool, _tls: VMWorkerThread) {
        unimplemented!()
    }

    fn prepare_for_roots_re_scanning() {
        unimplemented!()
    }

    fn scan_object<SV: SlotVisitor<SimpleSlot>>(
        _tls: VMWorkerThread,
        _object: ObjectReference,
        _slot_visitor: &mut SV,
    ) {
        unimplemented!()
    }

    fn scan_roots_in_mutator_thread(
        _tls: VMWorkerThread,
        _mutator: &'static mut Mutator<UlarVM>,
        _factory: impl RootsWorkFactory<SimpleSlot>,
    ) {
        unimplemented!()
    }

    fn scan_vm_specific_roots(_tls: VMWorkerThread, _factory: impl RootsWorkFactory<SimpleSlot>) {
        unimplemented!()
    }

    fn supports_return_barrier() -> bool {
        unimplemented!()
    }
}
