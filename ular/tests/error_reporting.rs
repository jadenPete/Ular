mod common;

use crate::common::evaluate_program;

#[test]
fn tabs_are_supported() -> anyhow::Result<()> {
    let output = evaluate_program(
        "\
struct Point {
	x: i32;
	y: i32;
}

point = Point {
	x: 1,
	y: 2
};

seq {
	println_i32(point.x);
	println_i32(point.z);
};
",
        false,
    )?;

    assert_eq!(
        output,
        "\
Error: Type `Point` has no field named `z`.

 11 │ seq {
 12 │ 	println_i32(point.x);
 13 │ 	println_i32(point.z);
    │ 	                  ^
 14 │ };

"
    );

    Ok(())
}
