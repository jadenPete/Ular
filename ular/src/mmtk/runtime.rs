use crate::mmtk::{UlarActivePlan, UlarVM};
use dashmap::DashMap;
use mmtk::{
    util::{
        constants::MIN_OBJECT_SIZE, Address, ObjectReference, OpaquePointer, VMMutatorThread,
        VMThread,
    },
    vm::{ActivePlan, GCThreadContext, VMBinding},
    AllocationSemantics, MMTKBuilder, Mutator, MMTK,
};
use std::{
    ptr::NonNull,
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Arc, LazyLock, Mutex, OnceLock, Weak,
    },
    thread::Thread,
};

struct SafepointState {
    mutator: Mutex<Option<VMMutatorThread>>,
    should_pause: AtomicBool,
    paused: AtomicBool,
}

impl SafepointState {
    fn new() -> Self {
        Self {
            mutator: Mutex::new(None),
            should_pause: AtomicBool::new(false),
            paused: AtomicBool::new(false),
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
static SAFEPOINT_STATES: LazyLock<Mutex<Vec<Weak<SafepointState>>>> =
    LazyLock::new(|| Mutex::new(Vec::new()));

thread_local! {
    static SAFEPOINT_STATE: LazyLock<Arc<SafepointState>> = LazyLock::new(|| {
        let mut safepoint_states = SAFEPOINT_STATES.lock().unwrap();
        let current_state = Arc::new(SafepointState::new());

        safepoint_states.push(Arc::downgrade(&current_state));

        current_state
    });
}

fn get_mmtk() -> &'static MMTK<UlarVM> {
    MMTK_INSTANCE
        .get()
        .expect("MMTK is not initialized. Please call `mmtk_init` first.")
}

pub fn is_mutator(thread: VMThread) -> bool {
    MUTATORS.contains_key(&thread.0.to_address().as_usize())
}

pub extern "C" fn mmtk_bind_current_mutator() {
    mmtk_bind_mutator(std::thread::current());
}

pub fn mmtk_bind_mutator(thread: Thread) {
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

pub extern "C" fn mmtk_alloc(size: usize, align: usize) -> Address {
    mmtk_maybe_pause_at_safepoint();
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

fn mmtk_maybe_pause_at_safepoint() {
    SAFEPOINT_STATE.with(|safepoint_state| {
        if safepoint_state.should_pause.load(Ordering::Relaxed) {
            safepoint_state.paused.store(true, Ordering::Relaxed);

            std::thread::park();

            // When this statement is executed, we will have woken up
            safepoint_state.paused.store(false, Ordering::Relaxed);
        }
    })
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

pub fn mmtk_spawn_gc_thread(context: GCThreadContext<UlarVM>) {
    match context {
        GCThreadContext::Worker(worker) => std::thread::spawn(|| {
            let thread = worker.tls;

            worker.run(thread, get_mmtk());
        }),
    };
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

pub fn number_of_mutators() -> usize {
    MUTATORS.len()
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

pub fn with_mutator<A, B: FnOnce(&Mutator<UlarVM>) -> A>(
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

pub fn with_mutators<A, B: FnOnce(Box<dyn Iterator<Item = &Mutator<UlarVM>> + '_>) -> A>(
    callback: B,
) -> A {
    callback(Box::new(
        MUTATORS.iter().map(|entry| &entry.value().mutator),
    ))
}
