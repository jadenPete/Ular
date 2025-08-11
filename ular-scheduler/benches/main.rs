use chili::Scope;
use divan::Bencher;
use nonzero_ext::nonzero;
use std::{
    fmt::{Display, Formatter},
    num::NonZero,
    time::Duration,
};
use ular_scheduler::{
    Configuration, Job, StandardThreadSpawner, ValueBuffer, Worker, WorkerPool, option::FfiOption,
};

#[derive(Clone, Copy)]
struct Benchmark {
    layers: usize,
    expected_result: u64,
}

impl Display for Benchmark {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        self.layers.fmt(formatter)
    }
}

struct Node {
    value: u64,
    left: Option<Box<Node>>,
    right: Option<Box<Node>>,
}

impl Node {
    fn tree(layers: usize) -> Self {
        Self {
            value: 1,
            left: (layers != 1).then(|| Box::new(Self::tree(layers - 1))),
            right: (layers != 1).then(|| Box::new(Self::tree(layers - 1))),
        }
    }
}

#[divan::bench(args = benchmarks(), threads = false)]
fn benchmark_chili(bencher: Bencher, benchmark: Benchmark) {
    fn sum(node: &Node, scope: &mut Scope) -> u64 {
        let (left_sum, right_sum) = scope.join(
            |scope| node.left.as_ref().map(|left| sum(left, scope)).unwrap_or(0),
            |scope| {
                node.right
                    .as_ref()
                    .map(|right| sum(right, scope))
                    .unwrap_or(0)
            },
        );

        node.value + left_sum + right_sum
    }

    let tree = Node::tree(benchmark.layers);
    let mut scope = Scope::global();

    bencher.bench_local(|| {
        assert_eq!(sum(&tree, &mut scope), benchmark.expected_result);
    });
}

#[divan::bench(args = benchmarks(), threads = false)]
fn benchmark_heartbeat(bencher: Bencher, benchmark: Benchmark) {
    let thread_pool = WorkerPool::new::<StandardThreadSpawner>(Configuration {
        thread_count: NonZero::new(num_cpus::get()).unwrap_or(nonzero!(1_usize)),
        heartbeat_interval: Duration::from_micros(100),
    });

    extern "C" fn node_left_sum(worker: &mut Worker, node: &Node) -> u64 {
        node.left
            .as_ref()
            .map(|left| *sum(worker, left).get())
            .unwrap_or(0)
    }

    extern "C" fn node_left_sum_buffered(worker: &mut Worker, node: &Node) -> ValueBuffer<u64> {
        node.left
            .as_ref()
            .map(|left| sum(worker, left))
            .unwrap_or(ValueBuffer::new(0))
    }

    extern "C" fn node_right_sum(worker: &mut Worker, node: &Node) -> u64 {
        node.right
            .as_ref()
            .map(|right| *sum(worker, right).get())
            .unwrap_or(0)
    }

    fn sum(worker: &mut Worker, node: &Node) -> ValueBuffer<u64> {
        let mut result = node.value;
        let mut job = Job::new();

        // SAFETY: We call `try_join` below
        unsafe {
            worker.fork(&mut job, node_left_sum_buffered, node);
        }

        result += worker.call(node_right_sum, node);

        // SAFETY: We call `try_join` with the same worker we called `fork` with
        result += match unsafe { worker.try_join(&job) } {
            FfiOption::Some(result) => *result.get(),
            FfiOption::None => worker.call(node_left_sum, node),
        };

        ValueBuffer::new(result)
    }

    let tree = Node::tree(benchmark.layers);

    bencher.bench_local(|| {
        assert_eq!(
            *sum(&mut thread_pool.worker(), &tree).get(),
            benchmark.expected_result
        );
    });

    thread_pool.join().unwrap();
}

#[divan::bench(args = benchmarks(), threads = false)]
fn benchmark_sequential(bencher: Bencher, benchmark: Benchmark) {
    fn sum(node: &Node) -> u64 {
        node.value
            + node.left.as_ref().map(|node| sum(node)).unwrap_or(0)
            + node.right.as_ref().map(|node| sum(node)).unwrap_or(0)
    }

    let tree = Node::tree(benchmark.layers);

    bencher.bench_local(|| {
        assert_eq!(sum(&tree), benchmark.expected_result);
    });
}

fn benchmarks() -> Vec<Benchmark> {
    [10, 20]
        .map(|i| Benchmark {
            layers: i,
            expected_result: (1 << i) - 1,
        })
        .to_vec()
}

fn main() {
    divan::main()
}
