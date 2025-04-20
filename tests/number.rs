mod common;

use crate::common::evaluate_program;

#[test]
fn addition_works() -> anyhow::Result<()> {
    /*
     * Add:
     * - The additive identity
     * - A small number
     * - A large number
     */
    let output = evaluate_program(
        &format!(
            "\
println_i32({min} + 0);
println_i32({min} + 1);
println_i32({min} + {max});
",
            min = i32::MIN,
            max = i32::MAX,
        ),
        true,
    )?;

    assert_eq!(
        output,
        format!(
            "\
{}
{}
-1
",
            i32::MIN,
            i32::MIN + 1,
        )
    );

    Ok(())
}

#[test]
fn subtraction_works() -> anyhow::Result<()> {
    // Subtract combinations of a negative number, the additive identity, and a positive number
    let output = evaluate_program(
        "\
println_i32(-1 - -1);
println_i32(-1 - 0);
println_i32(-1 - 1);
println_i32(0 - -1);
println_i32(0 - 0);
println_i32(0 - 1);
println_i32(1 - -1);
println_i32(1 - 0);
println_i32(1 - 1);
",
        true,
    )?;

    assert_eq!(
        output,
        "\
0
-1
-2
1
0
-1
2
1
0
"
    );

    Ok(())
}

#[test]
fn multiplication_works() -> anyhow::Result<()> {
    /*
     * Multiply by:
     * 1. A negative number
     * 2. Zero
     * 3. The multiplicative identity
     * 2. A positive number
     */
    let output = evaluate_program(
        "\
println_i32(-2 * -2);
println_i32(-2 * 0);
println_i32(-2 * 1);
println_i32(-2 * 2);
",
        true,
    )?;

    assert_eq!(
        output,
        "\
4
0
-2
-4
"
    );

    Ok(())
}

#[test]
fn division_works() -> anyhow::Result<()> {
    /*
     * Divide negative and positive numbers by:
     * - A negative number larger in absolute value
     * - A negative number smaller in absolute value
     * - The multiplicative identity
     * - A positive number smaller in absolute value
     * - A positive number greater in absolute value
     */

    let output = evaluate_program(
        "\
println_i32(-4 / -8);
println_i32(-4 / -2);
println_i32(-4 / 1);
println_i32(-4 / 2);
println_i32(-4 / 8);
println_i32(4 / -8);
println_i32(4 / -2);
println_i32(4 / 1);
println_i32(4 / 2);
println_i32(4 / 8);
",
        true,
    )?;

    assert_eq!(
        output,
        "\
0
2
-4
-2
0
0
-2
4
2
0
"
    );

    Ok(())
}

#[test]
fn divide_by_zero() -> anyhow::Result<()> {
    let output = evaluate_program("1 / 0;", false)?;

    assert_eq!(output, "Attempted to divide 1 by zero.\n");

    Ok(())
}

#[test]
fn out_of_range_checking() -> anyhow::Result<()> {
    let output = evaluate_program("256u8;", false)?;

    assert_eq!(output, "Literal out of range for type `u8`.\n");

    Ok(())
}

#[test]
fn heterogenous_arithmetic_prohibited() -> anyhow::Result<()> {
    let output = evaluate_program("4u16 / 2u8;", false)?;

    assert_eq!(
        output,
        "Expected a value of type `u16`, but got one of type `u8`.\n"
    );

    Ok(())
}
