mod common;

use crate::common::evaluate_program;

#[test]
fn logical_and_works() -> anyhow::Result<()> {
    let output = evaluate_program(
        "\
println_bool(false && false);
println_bool(false && true);
println_bool(true && false);
println_bool(true && true);
",
        true,
    )?;

    assert_eq!(output, "false\nfalse\nfalse\ntrue\n");

    Ok(())
}

#[test]
fn logical_or_works() -> anyhow::Result<()> {
    let output = evaluate_program(
        "\
println_bool(false || false);
println_bool(false || true);
println_bool(true || false);
println_bool(true || true);
",
        true,
    )?;

    assert_eq!(output, "false\ntrue\ntrue\ntrue\n");

    Ok(())
}
