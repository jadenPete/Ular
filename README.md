# Ular

Ular is functional, strongly typed programming language with automatic, ultra-low-overhead
parallelism. It's compiled so its performance can be competitive with that of systems languages like
C, Rust, and Go. However, that compilation happens
[just-in-time](https://en.wikipedia.org/wiki/Just-in-time_compilation) so you can execute Ular
programs (with the file extension `*.ul`) directly.

## Features

- First-class functions
- Basic data types (`i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `bool`, `unit`)
- Structural types (structs)
- Arithmetic, logical, and comparison operators
- If expressions

## Planned Features

- Anonymous functions
  - Tail call optimization
  - Parametric polymorphism (generics)
- Other complex data types (strings, tuples, and sum types)
- Type classes (similar to Rust's `trait`s)
- Pattern matching
- Garbage collection
- An effect system

## How to Use

You can run an Ular program like this:
```
$ cargo run < program.ul
```

where `program.ul` contains your program.

Here's an example program:
```
println_i32(42);
```

Take a look at [ular/tests](ular/tests) for various example programs.

## Automatic Parallelism

Similar to languages like [Bend](https://github.com/HigherOrderCO/bend), Ular is
*automatically parallel*. This means that you write code as you normally would, and Ular figures out
which expressions can be evaluated in parallel and what dependencies there are between them. To
avoid the overhead that typically comes with massively parallel applications, Ular uses
[Heartbeat Scheduling](https://www.andrew.cmu.edu/user/mrainey/heartbeat/heartbeat.html), which
evaluates tasks sequentially where it makes sense to do so. In actuality, the vast majority of code
is executed sequentially as plain function calls. Ular's scheduler
(located in [ular-scheduler](ular-scheduler)) is heavily inspired by the
[spice](https://github.com/judofyr/spice) library.

### Benchmarking

Ular's scheduler can be benchmarked like so:

```sh
$ cd ular-scheduler
$ cargo bench
```

This benchmark generates a full binary tree of varying depth and computes the sum of every node in
the tree using the scheduler. This task is also performed sequentially and with
[chili](https://github.com/dragostis/chili), a Rust port of the spice library, for comparison.

Here's what the results look like on my Thinkpad E15 Gen 3 with an AMD Ryzen 7 5700U:
```
main                     fastest       │ slowest       │ median        │ mean          │ samples │ iters
├─ benchmark_chili                     │               │               │               │         │
│  ├─ 10                 3.225 µs      │ 15.55 µs      │ 3.305 µs      │ 3.534 µs      │ 100     │ 100
│  ╰─ 20                 1.211 ms      │ 2.559 ms      │ 1.488 ms      │ 1.591 ms      │ 100     │ 100
├─ benchmark_heartbeat                 │               │               │               │         │
│  ├─ 10                 6.13 µs       │ 31.73 µs      │ 8.535 µs      │ 9.705 µs      │ 100     │ 100
│  ╰─ 20                 1.481 ms      │ 4.783 ms      │ 1.867 ms      │ 1.955 ms      │ 100     │ 100
╰─ benchmark_sequential                │               │               │               │         │
   ├─ 10                 1.562 µs      │ 6.857 µs      │ 1.567 µs      │ 1.64 µs       │ 100     │ 200
   ╰─ 20                 7.111 ms      │ 13.99 ms      │ 8.875 ms      │ 9.12 ms       │ 100     │ 100
```

# Functional

Ular is a [functional programming language](https://en.wikipedia.org/wiki/Functional_programming),
which means that variables can't be modified—only declared. Ular can't be described as
*purely functional* because expressions can have side effects. For example, the side effect of the
expression `println_i32(42)` is to print `42`.

This decision is a strategic balance between parallelism and ease of use. It's a lot easier to
execute functional code in parallel than it is to execute imperative code. That being said, the
entire purpose of programs is to perform side effects, and languages like Haskell make this more
difficult than it has to be, for better or for worse.

## Dependencies Between Side Effects

Currently, Ular doesn't understand that a program like:
```
println_i32(42);
println_bool(true);
```

ought to be executed sequentially. In the future, an
[effect system](https://en.wikipedia.org/wiki/Effect_system) will be embedded directly into the
language to complement Ular's type system with an understanding of the dependencies between
side effects. For now, you're recommended to use *sequential blocks*:
```
seq {
    println_i32(42);
    println_bool(true);
};
```
