mod common;

use crate::common::evaluate_program;

#[test]
fn function_arguments_are_typechecked() -> anyhow::Result<()> {
    evaluate_program(
        "\
fn increment(x: i32): i32 {
    x + 1
}

println_i32(increment(42));
",
        true,
    )?;

    let output1 = evaluate_program(
        "\
fn increment(x: i32): i32 {
    x + 1
}

increment(42u64);
",
        false,
    )?;

    assert_eq!(
        output1,
        "\
Error: Expected a value of type `i32`, but got one of type `u64`.

 3 │ }
 4 │ 
 5 │ increment(42u64);
   │           ^^^^^

",
    );

    let output2 = evaluate_program("println_i32(0u64);", false)?;

    assert_eq!(
        output2,
        "\
Error: Expected a value of type `i32`, but got one of type `u64`.

 1 │ println_i32(0u64);
   │             ^^^^

",
    );

    Ok(())
}

#[test]
fn if_expression_conditioned_typechecked() -> anyhow::Result<()> {
    evaluate_program("if true {};", true)?;
    evaluate_program("if false {};", true)?;

    let output = evaluate_program("if 0 {};", false)?;

    assert_eq!(
        output,
        "\
Error: Expected a value of type `bool`, but got one of type `i32`.

 1 │ if 0 {};
   │    ^

",
    );

    Ok(())
}

#[test]
fn if_expression_else_branch_typechecked() -> anyhow::Result<()> {
    evaluate_program("if true { 42 } else { 21 };", true)?;
    evaluate_program("if true { 42 } else {};", true)?;

    let output1 = evaluate_program("if true { 42 } else { 21u64 };", false)?;

    assert_eq!(
        output1,
        "\
Error: Expected a value of type `i32`, but got one of type `u64`.

 1 │ if true { 42 } else { 21u64 };
   │                     ^^^^^^^^^

",
    );

    evaluate_program("println_i32(if true { 42 } else { 21 });", true)?;

    let output2 = evaluate_program("println_i32(if true { 42 } else {});", false)?;

    assert_eq!(
        output2,
        "\
Error: Expected a value of type `i32`, but got one of type `unit`.

 1 │ println_i32(if true { 42 } else {});
   │             ^^^^^^^^^^^^^^^^^^^^^^

",
    );

    let output3 = evaluate_program("println_i32(if true { 42 } else { 21u64 });", false)?;

    assert_eq!(
        output3,
        "\
Error: Expected a value of type `i32`, but got one of type `u64`.

 1 │ println_i32(if true { 42 } else { 21u64 });
   │                                 ^^^^^^^^^

",
    );

    Ok(())
}

#[test]
fn arithmetic_infix_operation_typechecked() -> anyhow::Result<()> {
    let output1 = evaluate_program("0 + 1u64;", false)?;

    assert_eq!(
        output1,
        "\
Error: Expected a value of type `i32`, but got one of type `u64`.

 1 │ 0 + 1u64;
   │     ^^^^

",
    );

    evaluate_program("0u64 + 1u64;", true)?;

    Ok(())
}

#[test]
fn logical_infix_operation_typechecked() -> anyhow::Result<()> {
    let output1 = evaluate_program("true && 0;", false)?;

    assert_eq!(
        output1,
        "\
Error: Expected a value of type `bool`, but got one of type `i32`.

 1 │ true && 0;
   │         ^

",
    );

    let output2 = evaluate_program("1 && false;", false)?;

    assert_eq!(
        output2,
        "\
Error: Expected a value of type `bool`, but got one of type `i32`.

 1 │ 1 && false;
   │ ^

",
    );

    let output3 = evaluate_program("true || 0;", false)?;

    assert_eq!(
        output3,
        "\
Error: Expected a value of type `bool`, but got one of type `i32`.

 1 │ true || 0;
   │         ^

",
    );

    let output4 = evaluate_program("1 || false;", false)?;

    assert_eq!(
        output4,
        "\
Error: Expected a value of type `bool`, but got one of type `i32`.

 1 │ 1 || false;
   │ ^

",
    );

    evaluate_program("true && false;", true)?;

    Ok(())
}

#[test]
fn arithmetic_prefix_operation_typechecked() -> anyhow::Result<()> {
    let output = evaluate_program("-true;", false)?;

    assert_eq!(
        output,
        "\
Error: Expected a numeric value, but got one of type `bool`.

 1 │ -true;
   │  ^^^^

"
    );

    evaluate_program("-42;", true)?;

    Ok(())
}

#[test]
fn logical_prefix_operation_typechecked() -> anyhow::Result<()> {
    let output = evaluate_program("!42;", false)?;

    assert_eq!(
        output,
        "\
Error: Expected a value of type `bool`, but got one of type `i32`.

 1 │ !42;
   │  ^^

",
    );

    evaluate_program("!true;", true)?;

    Ok(())
}
