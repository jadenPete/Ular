use crate::mmtk::UlarVM;
use mmtk::{
    util::{
        constants::MIN_OBJECT_SIZE, Address, ObjectReference, OpaquePointer, VMMutatorThread,
        VMThread,
    },
    vm::VMBinding,
    AllocationSemantics, MMTKBuilder, Mutator, MMTK,
};
use std::{cell::RefCell, sync::OnceLock};
use ular_scheduler::Worker;

static MMTK_INSTANCE: OnceLock<MMTK<UlarVM>> = OnceLock::new();

thread_local! {
    static MUTATOR: RefCell<Option<Mutator<UlarVM>>> = const { RefCell::new(None) };
}

fn get_mmtk() -> &'static MMTK<UlarVM> {
    MMTK_INSTANCE
        .get()
        .expect("MMTK is not initialized. Please call `mmtk_init` first.")
}

fn with_mutator<A, B: FnOnce(&mut Mutator<UlarVM>) -> A>(callback: B) -> A {
    MUTATOR.with(|mutator_cell| {
        callback(
            mutator_cell
                .borrow_mut()
                .as_mut()
                .expect("This thread isn't bound. Please call `mmtk_bind_mutator` first."),
        )
    })
}

pub fn mmtk_bind_mutator(worker: &Worker) {
    MUTATOR.with(|mutator_cell| {
        let thread = VMMutatorThread(VMThread(OpaquePointer::from_address(Address::from_ref(
            worker,
        ))));

        let mutator = mmtk::memory_manager::bind_mutator(get_mmtk(), thread);

        mutator_cell.replace(Some(*mutator));
    });
}

pub extern "C" fn mmtk_alloc(size: usize, align: usize) -> Address {
    with_mutator(|mutator| {
        let adjusted_size = size.max(MIN_OBJECT_SIZE);
        let adjusted_align = align.max(UlarVM::MIN_ALIGNMENT);
        let result = mmtk::memory_manager::alloc(
            mutator,
            adjusted_size,
            adjusted_align,
            0,
            AllocationSemantics::Default,
        );

        if let Some(object_address) = ObjectReference::from_raw_address(result) {
            mmtk::memory_manager::post_alloc(
                mutator,
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
