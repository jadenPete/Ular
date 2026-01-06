use ular_test_common::evaluate_program;

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

fibonacci_0 = fibonacci(0);
fibonacci_1 = fibonacci(1);
fibonacci_7 = fibonacci(7);
fibonacci_75 = fibonacci(75);

seq {
    println_u64(fibonacci_0);
    println_u64(fibonacci_1);
    println_u64(fibonacci_7);
    println_u64(fibonacci_75);
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

#[test]
fn triangular_numbers() -> anyhow::Result<()> {
    let output = evaluate_program(
        "\
fn triangular_number(n: i32): i32 {
    result = triangular_number_inner(n, 0);

    fn triangular_number_inner(n: i32, accumulated: i32): i32 {
        if n == 0 {
            accumulated
        } else {
            triangular_number_inner(n - 1, accumulated + n)
        }
    }

    result
}

triangle_0 = triangular_number(0);
triangle_10 = triangular_number(10);
triangle_100 = triangular_number(100);

seq {
    println_i32(triangle_0);
    println_i32(triangle_10);
    println_i32(triangle_100);
};
",
        true,
    )?;

    assert_eq!(
        output,
        "\
0
55
5050
"
    );

    Ok(())
}
