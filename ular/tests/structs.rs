mod common;

use crate::common::evaluate_program;

#[test]
fn struct_definition_works() -> anyhow::Result<()> {
    evaluate_program(
        "\
struct Point {
    x: i32,
    y: i32
}
",
        true,
    )?;

    Ok(())
}

#[test]
fn empty_struct_not_allowed() -> anyhow::Result<()> {
    evaluate_program("struct Foo {}", false)?;

    Ok(())
}

#[test]
fn struct_application_typechecked() -> anyhow::Result<()> {
    let output1 = evaluate_program(
        "\
Point {
    x: 0,
    y: 1
};
",
        false,
    )?;

    assert_eq!(
        output1,
        "\
Error: Unknown type `Point`.

 1 │ Point {
   │ ^^^^^
 2 │     x: 0,
 3 │     y: 1

",
    );

    let output2 = evaluate_program(
        "\
struct Point {
    x: i32,
    y: i32
}

Point {
    x: 0
};
",
        false,
    )?;

    assert_eq!(
        output2,
        "\
Error: Missing field when constructing a value of type `Point`: `y`

 4 │ }
 5 │ 
 6 │ Point {
   │ ^^^^^^^
 7 │     x: 0
 8 │ };

",
    );

    let output3 = evaluate_program(
        "\
struct Point {
    x: i32,
    y: i32,
    z: i32
}

Point {
    x: 0
};
",
        false,
    )?;

    assert_eq!(
        output3,
        "\
Error: Missing fields when constructing a value of type `Point`: `y`, `z`

 5 │ }
 6 │ 
 7 │ Point {
   │ ^^^^^^^
 8 │     x: 0
 9 │ };

",
    );

    let output4 = evaluate_program(
        "\
struct Point {
    x: i32,
    y: i32
}

Point {
    x: 0,
    y: 1,
    z: 0
};
",
        false,
    )?;

    assert_eq!(
        output4,
        "\
Error: Type `Point` has no field named `z`.

  7 │     x: 0,
  8 │     y: 1,
  9 │     z: 0
    │     ^
 10 │ };

",
    );

    Ok(())
}

#[test]
fn struct_application_works() -> anyhow::Result<()> {
    evaluate_program(
        "\
struct Person {
    age: u8
}

Person {
    age: 21
};
",
        true,
    )?;

    Ok(())
}

#[test]
fn select_typechecked() -> anyhow::Result<()> {
    let output1 = evaluate_program("0.a;", false)?;

    assert_eq!(
        output1,
        "\
Error: Type `i32` has no field named `a`.

 1 │ 0.a;
   │ ^^^

",
    );

    let output2 = evaluate_program(
        "\
struct Person {
    age: u8
}

Person {
    age: 21
}.name;
",
        false,
    )?;

    assert_eq!(
        output2,
        "\
Error: Type `Person` has no field named `name`.

 3 │ }
 4 │ 
 5 │ Person {
   │ ^^^^^^^^
 6 │     age: 21
 7 │ }.name;

",
    );

    Ok(())
}

#[test]
fn select_works() -> anyhow::Result<()> {
    let output = evaluate_program(
        "\
struct Point {
    x: i32,
    y: i32
}

point = Point {
    x: 0,
    y: 1
};

seq {
    println_i32(point.x);
    println_i32(point.y);
};
",
        true,
    )?;

    assert_eq!(output, "0\n1\n");

    Ok(())
}

#[test]
fn nested_structs_supported() -> anyhow::Result<()> {
    let output = evaluate_program(
        "\
fn increment(n: i32): i32 {
    struct Wrapper {
        value: i32
    }

    wrapper = Wrapper {
        value: n + 1
    };

    wrapper.value
}

println_i32(increment(1));
",
        true,
    )?;

    assert_eq!(output, "2\n");

    Ok(())
}

#[test]
fn structs_cannot_evade_scope() -> anyhow::Result<()> {
    let output = evaluate_program(
        "\
fn flip(n: u8): u8 {
    wrapper = if n > 0u8 {
        struct Wrapper {
            value: u8
        }

        Wrapper {
            value: 0
        }
    } else {
        struct Wrapper {
            value: u8
        }

        Wrapper {
            value: 1
        }
    };

    wrapper.value
}
",
        false,
    )?;

    assert_eq!(
        output,
        "\
Error: A value of type `Wrapper` evades the scope of `Wrapper`.

 18 │     };
 19 │ 
 20 │     wrapper.value
    │     ^^^^^^^^^^^^^
 21 │ }

",
    );

    Ok(())
}
