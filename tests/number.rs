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
println_number({min} + 0);
println_number({min} + 1);
println_number({min} + {max});
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
println_number(-1 - -1);
println_number(-1 - 0);
println_number(-1 - 1);
println_number(0 - -1);
println_number(0 - 0);
println_number(0 - 1);
println_number(1 - -1);
println_number(1 - 0);
println_number(1 - 1);
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
println_number(-2 * -2);
println_number(-2 * 0);
println_number(-2 * 1);
println_number(-2 * 2);
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
println_number(-4 / -8);
println_number(-4 / -2);
println_number(-4 / 1);
println_number(-4 / 2);
println_number(-4 / 8);
println_number(4 / -8);
println_number(4 / -2);
println_number(4 / 1);
println_number(4 / 2);
println_number(4 / 8);
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
    evaluate_program("1 / 0;", false)?;

    Ok(())
}
