use crate::{
    arguments::GarbageCollectionPlan,
    libunwind,
    mmtk::{
        object_descriptor_store::{
            ObjectDescriptor, ObjectDescriptorReference, ObjectDescriptorStore,
        },
        stack_map::{IndexableStackMap, StackMapLocation},
        UlarActivePlan, UlarVM,
    },
};
use dashmap::DashMap;
use mmtk::{
    util::{
        constants::MIN_OBJECT_SIZE,
        copy::{CopySemantics, GCWorkerCopyContext},
        Address, ObjectReference, OpaquePointer, VMMutatorThread, VMThread,
    },
    vm::{
        slot::{SimpleSlot, Slot},
        ActivePlan, GCThreadContext, RootsWorkFactory, SlotVisitor, VMBinding,
    },
    AllocationSemantics, MMTKBuilder, Mutator, MMTK,
};
use std::{
    fmt::{Debug, Formatter},
    ptr::NonNull,
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Arc, LazyLock, Mutex, OnceLock, RwLock, Weak,
    },
    thread::Thread,
};

enum GarbageCollectionRoot {
    Direct(Address),
    Indirect(SimpleSlot),
}

impl GarbageCollectionRoot {
    fn object_reference(&self) -> Option<ObjectReference> {
        match self {
            Self::Direct(address) => ObjectReference::from_raw_address(*address),
            Self::Indirect(slot) => slot.load(),
        }
    }
}

struct SafepointState {
    mutator: Mutex<Option<VMMutatorThread>>,
    should_pause: AtomicBool,
    paused: AtomicBool,
    roots: Mutex<Vec<GarbageCollectionRoot>>,
}

impl SafepointState {
    fn new() -> Self {
        Self {
            mutator: Mutex::new(None),
            should_pause: AtomicBool::new(false),
            paused: AtomicBool::new(false),
            roots: Mutex::new(Vec::new()),
        }
    }
}

struct UlarMutator {
    mutator: Mutator<UlarVM>,
    thread: Thread,
}

// SAFETY: It's impossible to implement the interface demanded by `ActivePlan` without referencing
// `Mutator`s from multiple threads. They ought to be `Sync`.
unsafe impl Sync for UlarMutator {}

static MMTK_INSTANCE: OnceLock<MMTK<UlarVM>> = OnceLock::new();

/// A concurrent map of the current program's
/// [mutators](https://docs.mmtk.io/api/mmtk/plan/struct.Mutator.html).
///
/// The key of the map is the [VMMutatorThread] identifying the thread to which the mutator
/// corresponds. Normally, we'd use [std::thread::ThreadId], but we can't because it's opaque and
/// therefore can't be converted to and from [VMMutatorThread]. So instead, we use our own counter to
/// generate the keys for this map.
static MUTATORS: LazyLock<DashMap<usize, UlarMutator>> = LazyLock::new(DashMap::new);
static OBJECT_DESCRIPTOR_STORE: OnceLock<ObjectDescriptorStore> = OnceLock::new();
static SAFEPOINT_STATES: LazyLock<Mutex<Vec<Weak<SafepointState>>>> =
    LazyLock::new(|| Mutex::new(Vec::new()));

static STACK_MAP: OnceLock<RwLock<IndexableStackMap>> = OnceLock::new();

thread_local! {
    static SAFEPOINT_STATE: LazyLock<Arc<SafepointState>> = LazyLock::new(|| {
        let mut safepoint_states = SAFEPOINT_STATES.lock().unwrap();
        let current_state = Arc::new(SafepointState::new());

        safepoint_states.push(Arc::downgrade(&current_state));

        current_state
    });
}

pub(crate) struct ObjectDescriptorStoreAlreadySetError;

impl Debug for ObjectDescriptorStoreAlreadySetError {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "The object descriptor store was already set.")
    }
}

pub(crate) struct StackMapAlreadySetError;

impl Debug for StackMapAlreadySetError {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "The stack map was already set.")
    }
}

fn get_object_descriptor(object: ObjectReference) -> &'static ObjectDescriptor {
    let descriptor_reference = get_object_descriptor_reference(object);
    let object_descriptor_store = OBJECT_DESCRIPTOR_STORE
        .get()
        .expect("Expected the object descriptor store to be set");

    object_descriptor_store.get_descriptor(descriptor_reference)
}

fn get_object_descriptor_reference(object: ObjectReference) -> ObjectDescriptorReference {
    // SAFETY: All objects begin with a 32 bit object descriptor reference
    unsafe {
        object
            .to_header::<UlarVM>()
            .load::<ObjectDescriptorReference>()
    }
}

fn get_mmtk() -> &'static MMTK<UlarVM> {
    MMTK_INSTANCE
        .get()
        .expect("MMTK is not initialized. Please call `mmtk_init` first.")
}

pub(super) fn is_mutator(thread: VMThread) -> bool {
    MUTATORS.contains_key(&thread.0.to_address().as_usize())
}

pub(crate) extern "C" fn mmtk_bind_current_mutator() {
    mmtk_bind_mutator(std::thread::current());
}

pub(crate) fn mmtk_bind_mutator(thread: Thread) {
    static MMTK_THREAD_COUNTER: AtomicUsize = AtomicUsize::new(0);

    let mutator_key = MMTK_THREAD_COUNTER.fetch_add(1, Ordering::Relaxed);

    // SAFETY: The created address is never referenced
    let mmtk_thread = VMMutatorThread(VMThread(OpaquePointer::from_address(unsafe {
        Address::from_usize(mutator_key)
    })));

    // We modify `SAFEPOINT_STATE` before `MUTATORS` to avoid a race condition in which code that
    // operates on a mutator or all mutators expects a `SAFEPOINT_STATE` to be registered for that
    // mutator. In other words, a mutator being in `MUTATORS` is what registers it as a mutator, so
    // make sure all the other metadata is initialized before inserting it into `MUTATORS`.
    SAFEPOINT_STATE.with(|current_safepoint_state| {
        let _ = current_safepoint_state
            .mutator
            .lock()
            .unwrap()
            .insert(mmtk_thread);
    });

    let mutator = *mmtk::memory_manager::bind_mutator(get_mmtk(), mmtk_thread);

    MUTATORS.insert(mutator_key, UlarMutator { mutator, thread });
}

pub(crate) extern "C" fn mmtk_alloc(size: usize, align: usize) -> Address {
    mmtk_maybe_pause_at_safepoint();
    with_current_mutator(|mutator| {
        let adjusted_align = align.max(UlarVM::MIN_ALIGNMENT);

        // With bump allocators (like MMTK's NoGC plan), the allocation size must be a multiple of the
        // requested alignment
        let adjusted_size = size.max(MIN_OBJECT_SIZE).next_multiple_of(adjusted_align);
        let mut mutator_pointer = NonNull::from(mutator);

        // SAFETY: We're doing this to cast `mutator` from a `&Mutator<UlarVM>` to a
        // `&mut Mutator<UlarVM>`. Doing this in safe Rust would require lots of
        // complex synchronization (to avoid multiple mutable references to `&mut Ular<UlarVM>`),
        // which creates deadlocks in my experience. I suspect this is because MMTk relies on the
        // ability to hold multiple mutable references to the same mutator.
        let mutator_mut = unsafe { mutator_pointer.as_mut() };
        let result = mmtk::memory_manager::alloc(
            mutator_mut,
            adjusted_size,
            adjusted_align,
            0,
            AllocationSemantics::Default,
        );

        if let Some(object_address) = ObjectReference::from_raw_address(result) {
            mmtk::memory_manager::post_alloc(
                mutator_mut,
                object_address,
                adjusted_size,
                AllocationSemantics::Default,
            );
        }

        result
    })
}

pub(super) fn mmtk_copy_object(
    from: ObjectReference,
    semantics: CopySemantics,
    copy_context: &mut GCWorkerCopyContext<UlarVM>,
) -> ObjectReference {
    let descriptor = get_object_descriptor(from);
    let object_size = unsafe { descriptor.get_size(from) };
    let to_address = copy_context.alloc_copy(
        from,
        // SAFETY: We assume the object descriptor is valid
        object_size,
        descriptor.align(),
        0,
        semantics,
    );

    // SAFETY:
    // - `to_address` is guaranteed to be valid for writes of `descriptor.size` bytes
    // - `to_address` is guaranteed to be aligned to `descriptor.align` bytes
    // - `to_address` is guaranteed to not store an existing object, and therefore to not overlap with
    //   the object referenced by `from`
    unsafe { mmtk_copy_object_to(from, to_address) };

    let to = ObjectReference::from_raw_address(to_address).unwrap();

    copy_context.post_copy(to, object_size, semantics);

    to
}

/// Copy an object referenced by [from] to a region pointed to by [region].
///
/// # Safety
///
/// The following conditions must be satisfied:
/// - [region] must be valid for writes of the size of the object referenced by [from]
/// - [region] must be aligned to store objects of the type contained in [from]
/// - [region] must not overlap with the object referenced by [from]
pub(super) unsafe fn mmtk_copy_object_to(from: ObjectReference, region: Address) -> Address {
    let from_address = from.to_raw_address();
    let size = mmtk_get_object_size(from);

    unsafe {
        std::ptr::copy_nonoverlapping(from_address.to_ptr::<u8>(), region.to_mut_ptr::<u8>(), size)
    };

    from_address + size
}

pub(super) fn mmtk_get_object_size(object: ObjectReference) -> usize {
    // SAFETY: We assume the object descriptor is valid
    unsafe { get_object_descriptor(object).get_size(object) }
}

pub(crate) extern "C" fn mmtk_init(plan: GarbageCollectionPlan) {
    let mut mmtk_builder = MMTKBuilder::new();

    mmtk_builder.options.plan.set(plan.into());

    let mmtk = mmtk::memory_manager::mmtk_init(&mmtk_builder);

    if MMTK_INSTANCE.set(*mmtk).is_err() {
        panic!("MMTK is already initialized.");
    }
}

pub(crate) fn mmtk_maybe_pause_at_safepoint() {
    SAFEPOINT_STATE.with(|safepoint_state| {
        if safepoint_state.should_pause.load(Ordering::Relaxed) {
            mmtk_pause_at_safepoint();
        }
    })
}

pub fn mmtk_pause_at_safepoint() {
    SAFEPOINT_STATE.with(|safepoint_state| {
        scan_current_thread_roots();

        safepoint_state.paused.store(true, Ordering::Relaxed);

        std::thread::park();

        // When this statement is executed, we will have woken up
        safepoint_state.paused.store(false, Ordering::Relaxed);
    });
}

pub fn mmtk_register_roots(mut factory: impl RootsWorkFactory<SimpleSlot>) {
    let safepoint_states = SAFEPOINT_STATES.lock().unwrap();
    let mut direct_roots = Vec::new();
    let mut indirect_roots = Vec::new();

    for state in safepoint_states.iter() {
        if let Some(state) = state.upgrade() {
            let roots = state.roots.lock().unwrap();

            for root in roots.iter() {
                match root {
                    GarbageCollectionRoot::Direct(address) => {
                        if let Some(reference) = ObjectReference::from_raw_address(*address) {
                            direct_roots.push(reference);
                        }
                    }

                    GarbageCollectionRoot::Indirect(slot) => {
                        indirect_roots.push(*slot);
                    }
                };
            }
        }
    }

    factory.create_process_roots_work(indirect_roots);
    factory.create_process_pinning_roots_work(direct_roots);
}

pub fn mmtk_resume_all_mutators() {
    let safepoint_states = SAFEPOINT_STATES.lock().unwrap();

    for state in safepoint_states.iter() {
        if let Some(state) = state.upgrade() {
            state.should_pause.store(false, Ordering::Relaxed);
        }
    }

    for entry in MUTATORS.iter() {
        entry.value().thread.unpark();
    }
}

pub fn mmtk_scan_object<A: SlotVisitor<SimpleSlot>>(object: ObjectReference, slot_visitor: &mut A) {
    let descriptor = get_object_descriptor(object);

    for inner_reference in descriptor.inner_references() {
        let address = object.to_raw_address().add(inner_reference.offset);
        let slot = SimpleSlot::from_address(address);

        // Explanation for `.unwrap()`: inner references should always be non-null. If an
        // inner reference is null, something is wrong and we should know about it.
        let inner_descriptor_reference = get_object_descriptor_reference(slot.load().unwrap());

        if inner_descriptor_reference.is_allocated() {
            slot_visitor.visit_slot(slot);
        }
    }
}

pub(crate) fn mmtk_set_object_descriptor_store(
    store: ObjectDescriptorStore,
) -> Result<(), ObjectDescriptorStoreAlreadySetError> {
    OBJECT_DESCRIPTOR_STORE
        .set(store)
        .map_err(|_| ObjectDescriptorStoreAlreadySetError)
}

pub(super) fn mmtk_spawn_gc_thread(context: GCThreadContext<UlarVM>) {
    match context {
        GCThreadContext::Worker(worker) => std::thread::spawn(|| {
            let thread = worker.tls;

            worker.run(thread, get_mmtk());
        }),
    };
}

pub(crate) fn mmtk_set_stack_map(
    stack_map: IndexableStackMap,
) -> Result<(), StackMapAlreadySetError> {
    STACK_MAP
        .set(RwLock::new(stack_map))
        .map_err(|_| StackMapAlreadySetError)
}

pub fn mmtk_pause_all_mutators<A: FnMut(&'static mut Mutator<UlarVM>)>(mut mutator_visitor: A) {
    let safepoint_states = SAFEPOINT_STATES.lock().unwrap();

    for state in safepoint_states.iter() {
        // If `state.upgrade()` is `None`, then the safepoint state has been deallocated, meaning that
        // the thread to which it belongs has ended
        if let Some(state) = state.upgrade() {
            state.should_pause.store(true, Ordering::Relaxed);
        }
    }

    for state in safepoint_states.iter() {
        if let Some(state) = state.upgrade() {
            while !state.paused.load(Ordering::Relaxed) {
                std::hint::spin_loop();
            }

            // Release the lock quickly
            let mmtk_thread = {
                state.mutator
                    .lock()
                    .unwrap()
                    .unwrap_or_else(|| {
                        panic!("A thread wasn't bound. Please call `mmtk_bind_mutator` within every thread first.")
                    })
            };

            mutator_visitor(UlarActivePlan::mutator(mmtk_thread));
        }
    }
}

pub(super) fn number_of_mutators() -> usize {
    MUTATORS.len()
}

fn scan_current_thread_roots() {
    let stack_map = STACK_MAP
        .get()
        .expect("Expected the memory manager to set the stack map.")
        .read()
        .unwrap();

    SAFEPOINT_STATE.with(|safepoint_state| {
        let mut roots = safepoint_state.roots.lock().unwrap();

        roots.clear();

        let mut scan_at_function =
            |cursor: &mut libunwind::Cursor| -> Result<(), libunwind::Error> {
                let instruction_pointer = cursor.ip()? as u64;
                let record =
                    if let Some(record) = stack_map.records_by_address.get(&instruction_pointer) {
                        record
                    } else {
                        return Ok(());
                    };

                for location in &record.locations {
                    let potential_root = match location {
                        StackMapLocation::ConstIndex(i) => {
                            let constant_value = stack_map.constants[*i as usize].0 as usize;

                            // SAFETY: We assume the pointers inserted into the stack map are valid
                            GarbageCollectionRoot::Direct(unsafe {
                                Address::from_usize(constant_value)
                            })
                        }

                        StackMapLocation::Constant(value) => {
                            // SAFETY: We assume the pointers inserted into the stack map are valid
                            GarbageCollectionRoot::Direct(unsafe {
                                Address::from_usize(*value as usize)
                            })
                        }

                        StackMapLocation::Direct { register, offset } => {
                            let register_value = cursor.register(i32::from(*register))?;

                            // SAFETY: We assume the pointers inserted into the stack map are valid
                            GarbageCollectionRoot::Direct(unsafe {
                                Address::from_usize(register_value + *offset as usize)
                            })
                        }

                        StackMapLocation::Indirect { register, offset } => {
                            let register_value = cursor.register(i32::from(*register))?;
                            let pointer_pointer =
                                (register_value + *offset as usize) as *const *const usize;

                            GarbageCollectionRoot::Indirect(SimpleSlot::from_address(
                                Address::from_ptr(pointer_pointer),
                            ))
                        }

                        StackMapLocation::Register(i) => {
                            let register_value = cursor.register(i32::from(*i))?;

                            // SAFETY: We assume the pointers inserted into the stack map are valid
                            GarbageCollectionRoot::Direct(unsafe {
                                Address::from_usize(register_value)
                            })
                        }
                    };

                    let is_root = potential_root.object_reference().is_none_or(|object| {
                        get_object_descriptor_reference(object).is_allocated()
                    });

                    if is_root {
                        roots.push(potential_root);
                    }
                }

                Ok(())
            };

        libunwind::Cursor::local(|mut cursor| loop {
            scan_at_function(&mut cursor)?;

            if !cursor.step()? {
                break Ok(());
            }
        })
        .unwrap();
    });
}

fn with_current_mutator<A, B: FnOnce(&Mutator<UlarVM>) -> A>(callback: B) -> A {
    SAFEPOINT_STATE.with(|current_safepoint_state| {
        // Drop the lock before calling the callback
        let current_thread = {
            current_safepoint_state
                .mutator
                .lock()
                .unwrap()
                .unwrap_or_else(|| {
                    panic!("This thread isn't bound. Please call `mmtk_bind_mutator` first.")
                })
        };

        with_mutator(current_thread, callback)
    })
}

pub(super) fn with_mutator<A, B: FnOnce(&Mutator<UlarVM>) -> A>(
    thread: VMMutatorThread,
    callback: B,
) -> A {
    let mutator_key = thread.0 .0.to_address().as_usize();

    MUTATORS
        .get(&mutator_key)
        .map(|mutator| callback(&mutator.mutator))
        .unwrap_or_else(|| {
            panic!(
                "The thread identified by {} isn't bound. `mmtk_bind_mutator` should've been called.",
                mutator_key,
            )
        })
}

pub(super) fn with_mutators<A, B: FnOnce(Box<dyn Iterator<Item = &Mutator<UlarVM>> + '_>) -> A>(
    callback: B,
) -> A {
    callback(Box::new(
        MUTATORS.iter().map(|entry| &entry.value().mutator),
    ))
}
