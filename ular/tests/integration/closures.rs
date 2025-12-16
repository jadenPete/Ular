use ular_test_common::evaluate_program;

#[test]
fn test_calling_closure() -> anyhow::Result<()> {
    let output = evaluate_program(
        r#"say_hello = fn () {
    println_str("Hello, world!");
};

say_hello();
"#,
        true,
    )?;

    assert_eq!(output, "Hello, world!\n");

    Ok(())
}

#[test]
fn test_closure_arguments() -> anyhow::Result<()> {
    let output = evaluate_program(
        r#"say = fn (message: str) {
    println_str(message);
};

say("Hello, world!");
"#,
        true,
    )?;

    assert_eq!(output, "Hello, world!\n");

    Ok(())
}

#[test]
fn test_closure_argument_type_inference() -> anyhow::Result<()> {
    let output = evaluate_program(
        r#"fn say_hello(say: str => unit) {
    say("Hello, world!");
}

say_hello(fn (message): unit { println_str(message) });
"#,
        true,
    )?;

    assert_eq!(output, "Hello, world!\n");

    Ok(())
}

#[test]
fn test_closure_failed_type_inference() -> anyhow::Result<()> {
    let output = evaluate_program(r#"fn (message) { println_str(message) };"#, false)?;

    assert_eq!(output, "\
Error: Cannot infer the types of this closure's parameters. Please explicitly annotate every parameter's type.

These parameters are missing type annotations: `message`.

 1 │ fn (message) { println_str(message) };
   │ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

");

    Ok(())
}

#[test]
fn test_closure_missing_return_type() -> anyhow::Result<()> {
    let output = evaluate_program(r#"fn (value: i32): i32 {};"#, false)?;

    assert_eq!(
        output,
        "\
Error: Expected closure to return a value of type `i32`.

 1 │ fn (value: i32): i32 {};
   │ ^^^^^^^^^^^^^^^^^^^^^^^

"
    );

    Ok(())
}

#[test]
fn test_closure_return_type_inference() -> anyhow::Result<()> {
    let output = evaluate_program(
        r#"fn apply_transformation(transformation: i32 => i32, n: i32): i32 {
    transformation(n)
}

println_i32(apply_transformation(fn (i) { i + 1 }, 42));
"#,
        true,
    )?;

    assert_eq!(output, "43\n");

    Ok(())
}

#[test]
fn test_closure_return_value() -> anyhow::Result<()> {
    let output = evaluate_program(
        r#"add_one = fn (n: i32) { n + 1 };

println_i32(add_one(42));
"#,
        true,
    )?;

    assert_eq!(output, "43\n");

    Ok(())
}

#[test]
fn test_closure_calling_closure() -> anyhow::Result<()> {
    let output = evaluate_program(
        r#"evaluate = fn (closure: i32 => i32, value: i32) {
    closure(value)
};

println_i32(evaluate(fn (i) { i * 2 }, 21));
"#,
        true,
    )?;

    assert_eq!(output, "42\n");

    Ok(())
}

#[test]
fn test_closure_context() -> anyhow::Result<()> {
    let output1 = evaluate_program(
        r#"message = "Hello, world!";

say_hello = fn () {
    println_str(message);
};

say_hello();
"#,
        true,
    )?;

    assert_eq!(output1, "Hello, world!\n");

    let output2 = evaluate_program(
        r#"message = "Hello, world!";

outer = fn () {
    inner = fn () {
        println_str(message);
    };
    
    inner();
};

outer();
"#,
        true,
    )?;

    assert_eq!(output2, "Hello, world!\n");

    Ok(())
}

#[test]
fn test_returning_closure() -> anyhow::Result<()> {
    let output = evaluate_program(
        r#"message = "Hello, world!";

outer = fn () {
    inner = fn () {
        println_str(message);
    };
    
    inner
};

outer()();
"#,
        true,
    )?;

    assert_eq!(output, "Hello, world!\n");

    Ok(())
}
