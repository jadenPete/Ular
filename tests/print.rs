mod common;

use crate::common::evaluate_program;

#[test]
fn println_bool_works() -> anyhow::Result<()> {
    let output = evaluate_program(
        "\
println_bool(true);
println_bool(false);
",
        true,
    )?;

    assert_eq!(output, "true\nfalse\n");

    Ok(())
}

#[test]
fn println_number_works() -> anyhow::Result<()> {
    let output = evaluate_program(
        &format!(
            "\
println_number({});
println_number(0);
println_number({});
",
            i32::MIN,
            i32::MAX,
        ),
        true,
    )?;

    assert_eq!(output, format!("{}\n0\n{}\n", i32::MIN, i32::MAX));

    Ok(())
}
