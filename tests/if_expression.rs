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

#[test]
fn else_if_works() -> anyhow::Result<()> {
    let output1 = evaluate_program(
        "\
if true {
    println_i32(1);
} else if true {
    println_i32(2);
};
",
        true,
    )?;

    assert_eq!(output1, "1\n");

    let output2 = evaluate_program(
        "\
if false {
    println_i32(1);
} else if true {
    println_i32(2);
};
",
        true,
    )?;

    assert_eq!(output2, "2\n");

    let output3 = evaluate_program(
        "\
println_i32(if false {
	1
} else if false {
	2
} else if false {
    3
} else if true {
    4
} else {
    5
});
",
        true,
    )?;

    assert_eq!(output3, "4\n");

    let output4 = evaluate_program(
        "\
println_i32(if false {
	1
} else if false {
	2
} else if false {
    3
} else if false {
    4
} else {
    5
});
",
        true,
    )?;

    assert_eq!(output4, "5\n");

    Ok(())
}
