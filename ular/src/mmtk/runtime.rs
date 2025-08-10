use crate::mmtk::UlarVM;
use dashmap::DashMap;
use mmtk::{
    util::{
        constants::MIN_OBJECT_SIZE, Address, ObjectReference, OpaquePointer, VMMutatorThread,
        VMThread,
    },
    vm::VMBinding,
    AllocationSemantics, MMTKBuilder, Mutator, MMTK,
};
use std::{
    cell::RefCell,
    ptr::NonNull,
    sync::{LazyLock, OnceLock},
};
use ular_scheduler::Worker;

struct UlarMutator(Mutator<UlarVM>);

// SAFETY: It's impossible to implement the interface demanded by `ActivePlan` without referencing
// `Mutator`s from multiple threads. They ought to be `Sync`.
unsafe impl Sync for UlarMutator {}

static MMTK_INSTANCE: OnceLock<MMTK<UlarVM>> = OnceLock::new();
static MUTATORS: LazyLock<DashMap<usize, UlarMutator>> = LazyLock::new(DashMap::new);

thread_local! {
    static CURRENT_THREAD: RefCell<Option<VMMutatorThread>> = const { RefCell::new(None) };
}

fn get_mmtk() -> &'static MMTK<UlarVM> {
    MMTK_INSTANCE
        .get()
        .expect("MMTK is not initialized. Please call `mmtk_init` first.")
}

pub fn is_mutator(thread: VMThread) -> bool {
    MUTATORS.contains_key(&thread.0.to_address().as_usize())
}

pub fn mmtk_bind_mutator(worker: &Worker) {
    let thread = VMMutatorThread(VMThread(OpaquePointer::from_address(Address::from_ref(
        worker,
    ))));

    let mutator = mmtk::memory_manager::bind_mutator(get_mmtk(), thread);
    let mutator_key = worker as *const Worker as usize;

    MUTATORS.insert(mutator_key, UlarMutator(*mutator));
    CURRENT_THREAD.with(|current_thread_cell| {
        current_thread_cell.replace(Some(thread));
    });
}

pub extern "C" fn mmtk_alloc(size: usize, align: usize) -> Address {
    with_current_mutator(|mutator| {
        let adjusted_size = size.max(MIN_OBJECT_SIZE);
        let adjusted_align = align.max(UlarVM::MIN_ALIGNMENT);
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

pub extern "C" fn mmtk_init() {
    let mut mmtk_builder = MMTKBuilder::new();

    // TODO: Use a real garbage collector
    mmtk_builder
        .options
        .plan
        .set(mmtk::util::options::PlanSelector::NoGC);

    let mmtk = mmtk::memory_manager::mmtk_init(&mmtk_builder);

    if MMTK_INSTANCE.set(*mmtk).is_err() {
        panic!("MMTK is already initialized.");
    }
}

pub fn number_of_mutators() -> usize {
    MUTATORS.len()
}

fn with_current_mutator<A, B: FnOnce(&Mutator<UlarVM>) -> A>(callback: B) -> A {
    CURRENT_THREAD.with(|current_thread_cell| match *current_thread_cell.borrow() {
        Some(current_thread) => with_mutator(current_thread, callback),
        None => panic!("This thread isn't bound. Please call `mmtk_bind_mutator` first."),
    })
}

pub fn with_mutator<A, B: FnOnce(&Mutator<UlarVM>) -> A>(
    thread: VMMutatorThread,
    callback: B,
) -> A {
    let mutator_key = thread.0 .0.to_address().as_usize();

    MUTATORS
        .get(&mutator_key)
        .map(|mutator| callback(&mutator.0))
        .unwrap_or_else(|| {
            panic!(
                "The thread identified by {} isn't bound. `mmtk_bind_mutator` should've been called.",
                mutator_key,
            )
        })
}

pub fn with_mutators<A, B: FnOnce(Box<dyn Iterator<Item = &Mutator<UlarVM>> + '_>) -> A>(
    callback: B,
) -> A {
    callback(Box::new(MUTATORS.iter().map(|entry| &entry.value().0)))
}
