mod job_link;
pub mod option;

use crate::job_link::JobLink;
use crate::option::FfiOption;
use intrusive_collections::{LinkedList, UnsafeRef, intrusive_adapter};
use nonzero_ext::nonzero;
use std::{
    collections::BTreeMap,
    marker::PhantomData,
    mem::MaybeUninit,
    num::NonZero,
    ptr::NonNull,
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, AtomicUsize, Ordering},
    },
    thread::JoinHandle,
    time::Duration,
};

pub struct Configuration {
    pub thread_count: NonZero<usize>,
    pub heartbeat_interval: Duration,
}

impl Default for Configuration {
    fn default() -> Self {
        Self {
            thread_count: NonZero::new(num_cpus::get()).unwrap_or(nonzero!(1_usize)),
            heartbeat_interval: Duration::from_micros(100),
        }
    }
}

struct WorkerContext {
    is_stopping: AtomicBool,
    heartbeat_index: AtomicUsize,
    heartbeat_values: Mutex<Vec<Arc<AtomicBool>>>,
    shared_jobs: Mutex<BTreeMap<usize, &'static mut Job<'static>>>,
}

impl WorkerContext {
    fn new() -> Self {
        Self {
            is_stopping: AtomicBool::new(false),
            heartbeat_index: AtomicUsize::new(0),
            heartbeat_values: Mutex::new(Vec::new()),
            shared_jobs: Mutex::new(BTreeMap::new()),
        }
    }
}

pub struct WorkerPool {
    context: Arc<WorkerContext>,
    worker_threads: Vec<JoinHandle<()>>,
    heartbeat_thread: JoinHandle<()>,
}

impl WorkerPool {
    pub fn join(self) -> std::thread::Result<()> {
        self.context.is_stopping.store(true, Ordering::Relaxed);

        for thread in self.worker_threads {
            thread.join()?;
        }

        self.heartbeat_thread.join()?;

        Ok(())
    }

    pub fn new(configuration: Configuration) -> Self {
        let context = Arc::new(WorkerContext::new());
        let thread_count = configuration.thread_count.into();
        let worker_threads = (0..thread_count)
            .map(|_| {
                let cloned_context = Arc::clone(&context);
                let heartbeat_value = Arc::new(AtomicBool::new(false));

                std::thread::spawn(move || {
                    cloned_context
                        .heartbeat_values
                        .lock()
                        .unwrap()
                        .push(Arc::clone(&heartbeat_value));

                    let mut worker = Worker::new(cloned_context, heartbeat_value);

                    worker.execute_background_thread()
                })
            })
            .collect();

        let cloned_context = Arc::clone(&context);
        let heartbeat_thread = std::thread::spawn(move || {
            execute_heartbeat_thread(&configuration, &cloned_context);
        });

        Self {
            context,
            worker_threads,
            heartbeat_thread,
        }
    }

    pub fn worker(&self) -> Worker {
        let heartbeat_value = Arc::new(AtomicBool::new(false));

        self.context
            .heartbeat_values
            .lock()
            .unwrap()
            .push(Arc::clone(&heartbeat_value));

        Worker::new(Arc::clone(&self.context), heartbeat_value)
    }
}

pub struct Worker {
    context: Arc<WorkerContext>,
    heartbeat_value: Arc<AtomicBool>,
    job_queue: LinkedList<JobAdapter>,
    shared_job_key: Option<usize>,
}

impl Worker {
    #[inline]
    pub fn call<'a, Context, Result>(
        &mut self,
        function: extern "C" fn(&mut Worker, &'a Context) -> Result,
        context: &'a Context,
    ) -> Result {
        self.tick();

        function(self, context)
    }

    fn execute_background_thread(&mut self) {
        while !self.context.is_stopping.load(Ordering::Relaxed) {
            self.execute_shared_job();
        }
    }

    fn execute_shared_job(&mut self) {
        let mut shared_jobs = self.context.shared_jobs.lock().unwrap();

        if let Some((_, job)) = shared_jobs.pop_first() {
            // Drop `shared_jobs` in case the job indirectly uses it
            drop(shared_jobs);

            // SAFETY: Popping the job from `shared_jobs` ensures that it's only executed once. Also,
            // the job wouldn't have been shared if it hadn't been queued first.
            unsafe {
                job.execute(self);
            }
        }
    }

    /// Queues a job.
    ///
    /// # Safety
    ///
    /// After calling fork, you must call [Worker::try_join] before the `job` reference is dropped.
    /// Otherwise, the worker's job queue will contain a pointer to a job that's no longer valid.
    #[inline]
    pub unsafe extern "C" fn fork<'a, Context, Result>(
        &mut self,
        job: &mut Job<'a, Context, Result>,
        function: extern "C" fn(&mut Worker, &Context) -> ValueBuffer<Result>,
        context: &'a Context,
    ) {
        job.function = Some(function);
        job.context = Some(context);

        // SAFETY: The contract for calling `fork` ensures that `job` is removed from the job queue
        // before it's dropped. Also, although we're casting `job` to a different type and ignoring
        // its `Context` and `Result` types, this is safe because those type parameters are only used
        // internally by the job.
        self.job_queue.push_back(unsafe {
            UnsafeRef::from_raw(job as *const Job<Context, Result> as *const Job)
        });
    }

    #[cold]
    fn join_executing<Context, Result>(
        &mut self,
        job: &Job<Context, Result>,
    ) -> FfiOption<ValueBuffer<Result>> {
        let mut shared_jobs = self.context.shared_jobs.lock().unwrap();

        if let Some(key) = self.shared_job_key {
            if shared_jobs.get(&key).is_some_and(|shared_job| {
                std::ptr::eq(
                    *shared_job,
                    job as *const Job<Context, Result> as *const Job,
                )
            }) {
                shared_jobs.remove(&key);

                return FfiOption::None;
            }
        }

        drop(shared_jobs);

        while !job.done.load(Ordering::Relaxed) {
            self.execute_shared_job();
        }

        // SAFETY: `job.result` is always defined when `job.done` is true. This is because `job.done`
        // is initially set to false and `Job::execute`, the only function that writes to
        // `job.result`, always writes to `job.result` before setting `job.done` to true
        FfiOption::Some(unsafe { job.result.assume_init_read() })
    }

    fn heartbeat(&mut self) {
        let mut shared_jobs = self.context.shared_jobs.lock().unwrap();
        let already_sharing_job = self
            .shared_job_key
            .is_some_and(|key| shared_jobs.contains_key(&key));

        if !already_sharing_job {
            self.shared_job_key = self.job_queue.pop_front().map(|job| {
                let key = self.context.heartbeat_index.fetch_add(1, Ordering::Relaxed);

                // SAFETY: `pop_front` should return a non-null and valid reference to the first
                // element in the job queue
                shared_jobs.insert(key, unsafe {
                    NonNull::new_unchecked(UnsafeRef::<Job>::into_raw(job)).as_mut()
                });

                key
            });
        }

        self.heartbeat_value.store(false, Ordering::Relaxed);
    }

    #[inline]
    pub extern "C" fn tick(&mut self) {
        if self.heartbeat_value.load(Ordering::Relaxed) {
            self.heartbeat();
        }
    }

    /// Waits for the result of [Worker::fork].
    ///
    /// If the job executed in another thread, returns [Some] with its result. Otherwise, returns
    /// [None] with the expectation that the job is executed manually in the same thread.
    ///
    /// # Safety
    ///
    /// If [Worker::fork] was called with `job`, it must've been called with the same worker this
    /// function is executed with.
    #[inline]
    pub unsafe extern "C" fn try_join<Context, Result>(
        &mut self,
        job: &Job<Context, Result>,
    ) -> FfiOption<ValueBuffer<Result>> {
        match job.state() {
            JobState::Pending => FfiOption::None,
            JobState::Queued => {
                // SAFETY: That the job is queued and the contract for calling `try_join` guarantee
                // that `job` is a part of this worker's job queue
                unsafe {
                    self.job_queue
                        .cursor_mut_from_ptr(job as *const Job<Context, Result> as *const Job)
                        .remove();
                }

                FfiOption::None
            }

            JobState::Executing => self.join_executing(job),
        }
    }

    fn new(context: Arc<WorkerContext>, heartbeat_value: Arc<AtomicBool>) -> Self {
        Self {
            context,
            heartbeat_value,
            job_queue: LinkedList::new(JobAdapter::new()),
            shared_job_key: None,
        }
    }
}

#[repr(C)]
pub struct Job<'a, Context = (), Result = ()> {
    done: AtomicBool,
    link: JobLink,
    function: Option<extern "C" fn(&mut Worker, &'a Context) -> ValueBuffer<Result>>,
    context: Option<&'a Context>,
    result: MaybeUninit<ValueBuffer<Result>>,
}

impl<Context, Result> Job<'_, Context, Result> {
    /// Executes the job.
    ///
    /// # Safety
    ///
    /// [Job::execute] should only be called once on a given job. Calling [execute] multiple times on
    /// a given job is unsafe because it consumes the job's context. [execute] should only be called
    /// if a job has been queued. Otherwise, its function will be null and we'll be calling a null
    /// function pointer, which is undefined behavior.
    ///
    /// We could check these things before executing the job, but that would incur an overhead.
    unsafe fn execute(&mut self, worker: &mut Worker) {
        // SAFETY: The contract for calling `execute` states that it should only be called if the job
        // has been queued. `Worker::fork` is responsible for queuing the job and initializes
        // `self.function`.
        let function = unsafe { self.function.unwrap_unchecked() };

        // SAFETY: The contract for calling `execute` states that it should only be called if the job
        // has been queued. The `fork` function queues the job and initializes `self.context`.
        let context = unsafe { self.context.unwrap_unchecked() };

        self.result.write(worker.call(function, context));
        self.done.store(true, Ordering::Relaxed);
    }

    #[inline]
    pub extern "C" fn new() -> Self {
        Self {
            done: AtomicBool::new(false),
            link: JobLink::new(),
            function: None,
            context: None,
            result: MaybeUninit::uninit(),
        }
    }

    fn state(&self) -> JobState {
        match self.function {
            None => JobState::Pending,
            Some(_) if self.link.is_linked() => JobState::Queued,
            Some(_) => JobState::Executing,
        }
    }
}

impl Default for Job<'_, (), ()> {
    fn default() -> Self {
        Self::new()
    }
}

intrusive_adapter!(JobAdapter = UnsafeRef<Job<'static>>: Job { link: JobLink });

enum JobState {
    Pending,
    Queued,
    Executing,
}

/// The largest type in Ular (u64) is 8 bytes long
pub const VALUE_BUFFER_WORD_SIZE: usize = 1;
pub const VALUE_BUFFER_SIZE: usize = VALUE_BUFFER_WORD_SIZE * 8;

#[repr(C)]
pub struct ValueBuffer<A> {
    buffer: [u64; VALUE_BUFFER_WORD_SIZE],
    buffer_type: PhantomData<A>,
}

impl<A> ValueBuffer<A> {
    // Assert at compile time that the size and alignment of `A` are within the limits of this type
    const _SIZE_OK: () = assert!(size_of::<A>() <= VALUE_BUFFER_SIZE);
    const _ALIGNMENT_OK: () = assert!(align_of::<A>() <= align_of::<u64>());

    #[inline]
    pub fn get(&self) -> &A {
        // SAFETY: We know from the implementation of `new` that `self.buffer` is initialized with a
        // valid value of type `A`
        unsafe { std::mem::transmute(&self.buffer) }
    }

    #[inline]
    pub fn new(value: A) -> Self {
        let mut buffer = MaybeUninit::uninit();
        let buffer_pointer = buffer.as_mut_ptr() as *mut A;

        // SAFETY: `buffer_pointer` is valid and aligned because we checked above that:
        // 1. A value of type `A` fits into `buffer`
        // 2. The alignment of `A` is no more than the alignment of `[u64; VALUE_BUFFER_WORD_SIZE]`
        //    (the alignment of `u64`)
        unsafe {
            buffer_pointer.write(value);
        }

        Self {
            // SAFETY: We just initialized `buffer`
            buffer: unsafe { buffer.assume_init() },
            buffer_type: PhantomData,
        }
    }
}

fn execute_heartbeat_thread(configuration: &Configuration, context: &WorkerContext) {
    while !context.is_stopping.load(Ordering::Relaxed) {
        std::thread::sleep(configuration.heartbeat_interval);

        for heartbeat_value in context.heartbeat_values.lock().unwrap().iter() {
            heartbeat_value.store(true, Ordering::Relaxed);
        }
    }
}
