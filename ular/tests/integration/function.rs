use crate::common::evaluate_program;

#[test]
fn functions_work() -> anyhow::Result<()> {
    let output = evaluate_program(
        "\
fn print_42() {
	println_i32(42);
}

print_42();
",
        true,
    )?;

    assert_eq!(output, "42\n");

    Ok(())
}

#[test]
fn function_arguments_and_return_values_work() -> anyhow::Result<()> {
    let output = evaluate_program(
        "\
fn add_u8(x: u8, y: u8): u8 {
	x + y
}

fn add_u16(x: u16, y: u16): u16 {
	x + y
}

fn add_u32(x: u32, y: u32): u32 {
	x + y
}

fn add_u64(x: u64, y: u64): u64 {
	x + y
}

fn add_i8(x: i8, y: i8): i8 {
	x + y
}

fn add_i16(x: i16, y: i16): i16 {
	x + y
}

fn add_i32(x: i32, y: i32): i32 {
	x + y
}

fn add_i64(x: i64, y: i64): i64 {
	x + y
}

seq {
    println_u8(add_u8(1, 1));
    println_u16(add_u16(1, 2));
    println_u32(add_u32(2, 3));
    println_u64(add_u64(3, 5));
    println_i8(add_i8(-1, -1));
    println_i16(add_i16(-1, -2));
    println_i32(add_i32(-2, -3));
    println_i64(add_i64(-3, -5));
};
",
        true,
    )?;

    assert_eq!(
        output,
        "\
2
3
5
8
-2
-3
-5
-8
",
    );

    Ok(())
}

#[test]
fn functions_calling_functions_works() -> anyhow::Result<()> {
    let output = evaluate_program(
        "\
fn identity(x: i32): i32 {
	x
}

fn println_i32_proxy(x: i32) {
	println_i32(identity(x));
}

println_i32_proxy(42);
",
        true,
    )?;

    assert_eq!(output, "42\n");

    Ok(())
}

#[test]
fn recursive_functions_work() -> anyhow::Result<()> {
    let output = evaluate_program(
        "\
fn complicated_not(boolean: bool): bool {
	if boolean {
		!complicated_not(!boolean)
	} else {
		!boolean
	}
}

println_bool(complicated_not(true));
",
        true,
    )?;

    assert_eq!(output, "false\n");

    Ok(())
}

#[test]
fn higher_order_functions_work() -> anyhow::Result<()> {
    let output1 = evaluate_program(
        "\
fn print_0() {
	println_i32(0);
}

fn print_1() {
	println_i32(1);
}

fn print_number(is_odd: bool): () => unit {
	if is_odd {
		print_1
	} else {
		print_0
	}
}

seq {
    print_number(false)();
    print_number(true)();
};
",
        true,
    )?;

    assert_eq!(output1, "0\n1\n");

    let output2 = evaluate_program(
        "\
fn identity(x: i32): i32 {
    x
}

fn increment(x: i32): i32 {
    x + 1
}

fn conditional_incrementer(add_one: bool): i32 => i32 {
    if add_one {
        increment
    } else {
        identity
    }
}

seq {
    println_i32(conditional_incrementer(false)(42));
    println_i32(conditional_incrementer(true)(42));
};
",
        true,
    )?;

    assert_eq!(output2, "42\n43\n");

    Ok(())
}

#[test]
fn closures_unsupported() -> anyhow::Result<()> {
    let output = evaluate_program(
        "\
forty_two = 42;

fn print_42() {
    println_i32(forty_two);
}

print_42();
",
        false,
    )?;

    assert_eq!(
        output,
        "\
Error: Unknown value `forty_two`.

 2 │ 
 3 │ fn print_42() {
 4 │     println_i32(forty_two);
   │                 ^^^^^^^^^
 5 │ }
 6 │ 

",
    );

    Ok(())
}

#[test]
fn nested_functions_unsupported() -> anyhow::Result<()> {
    let output = evaluate_program(
        "\
fn outer() {
    fn inner() {
        println_i32(42);
    }

    inner();
}

outer();
",
        false,
    )?;

    assert_eq!(
        output,
        "\
Error: Nested functions aren't currently supported.

 1 │ fn outer() {
 2 │     fn inner() {
   │     ^^^^^^^^^^^^
 3 │         println_i32(42);
 4 │     }
 5 │ 
 6 │     inner();

",
    );

    Ok(())
}

#[test]
fn function_return_values_typechecked() -> anyhow::Result<()> {
    let output1 = evaluate_program(
        "\
fn the_meaning_of_life_the_universe_and_everything(): i32 {
    42u8
}
",
        false,
    )?;

    assert_eq!(
        output1,
        "\
Error: Expected a value of type `i32`, but got one of type `u8`.

 1 │ fn the_meaning_of_life_the_universe_and_everything(): i32 {
 2 │     42u8
   │     ^^^^
 3 │ }

",
    );

    let output2 = evaluate_program(
        "fn the_meaning_of_life_the_universe_and_everything(): i32 {}",
        false,
    )?;

    assert_eq!(
        output2,
        "\
Error: Expected `the_meaning_of_life_the_universe_and_everything` to return a value of type `i32`.

 1 │ fn the_meaning_of_life_the_universe_and_everything(): i32 {}
   │ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

"
    );

    Ok(())
}
