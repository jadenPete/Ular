mod common;

use crate::common::evaluate_program;

#[test]
fn branching_works() -> anyhow::Result<()> {
    let output = evaluate_program("println_i32(if true { 1 } else { 2 });", true)?;

    assert_eq!(output, "1\n");

    Ok(())
}

#[test]
fn multiple_statements() -> anyhow::Result<()> {
    let output = evaluate_program(
        "\
if false {
	println_i32(1);
	println_i32(2);
	println_i32(3);
} else {
	println_i32(4);
	println_i32(5);
	println_i32(6);
};
",
        true,
    )?;

    assert_eq!(output, "4\n5\n6\n");

    Ok(())
}
