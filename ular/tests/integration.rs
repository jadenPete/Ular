mod common;

use crate::common::evaluate_program;

#[test]
fn fibonacci_numbers() -> anyhow::Result<()> {
    let output = evaluate_program(
        "\
fn fibonacci_underlying(n: u64, a: u64, b: u64): u64 {
    if n == 0u64 {
        a
    } else {
        fibonacci_underlying(n - 1, b, a + b)
    }
}

fn fibonacci(n: u64): u64 {
    fibonacci_underlying(n, 0, 1)
}

seq {
    println_u64(fibonacci(0));
    println_u64(fibonacci(1));
    println_u64(fibonacci(7));
    println_u64(fibonacci(75));
};
",
        true,
    )?;

    assert_eq!(
        output,
        "\
0
1
13
2111485077978050
"
    );

    Ok(())
}
