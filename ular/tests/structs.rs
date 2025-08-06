mod common;

use crate::common::evaluate_program;

#[test]
fn struct_definition_works() -> anyhow::Result<()> {
    evaluate_program(
        "\
struct Point {
    x: i32;
    y: i32;
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
    x: i32;
    y: i32;
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
    x: i32;
    y: i32;
    z: i32;
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
    x: i32;
    y: i32;
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
    age: u8;
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
   │   ^

",
    );

    let output2 = evaluate_program(
        "\
struct Person {
    age: u8;
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

 5 │ Person {
 6 │     age: 21
 7 │ }.name;
   │   ^^^^

",
    );

    Ok(())
}

#[test]
fn select_works() -> anyhow::Result<()> {
    let output = evaluate_program(
        "\
struct Point {
    x: i32;
    y: i32;
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
        value: i32;
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
            value: u8;
        }

        Wrapper {
            value: 0
        }
    } else {
        struct Wrapper {
            value: u8;
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

#[test]
fn identifier_types_validated() -> anyhow::Result<()> {
    let output1 = evaluate_program(
        "\
struct Person {
    profession: Profession;
}
",
        false,
    )?;

    assert_eq!(output1, "Error: Unknown type `Profession`.\n");

    let output2 = evaluate_program("fn coolest_person(): Person {}", false)?;

    assert_eq!(output2, "Error: Unknown type `Person`.\n");

    Ok(())
}

#[test]
fn identifier_types_work() -> anyhow::Result<()> {
    let output = evaluate_program(
        "\
struct Point {
    x: i32;
    y: i32;
}

fn get_origin(): Point {
    Point {
        x: 0,
        y: 0
    }
}

origin = get_origin();

seq {
    println_i32(origin.x);
    println_i32(origin.y);
};
",
        true,
    )?;

    assert_eq!(output, "0\n0\n");

    Ok(())
}

#[test]
fn struct_methods_work() -> anyhow::Result<()> {
    let output = evaluate_program(
        "\
struct Point {
    x: i32;
    y: i32;

    fn add(self: Point, other: Point): Point {
        Point {
            x: self.x + other.x,
            y: self.y + other.y
        }
    }
}

point1 = Point { x: 1, y: 2 };
point2 = Point::add(point1, Point { x: -2, y: 5 });

seq {
    println_i32(point2.x);
    println_i32(point2.y);
};
",
        true,
    )?;

    assert_eq!(output, "-1\n7\n");

    Ok(())
}

#[test]
fn struct_static_methods_work() -> anyhow::Result<()> {
    let output = evaluate_program(
        "\
struct Point {
    x: i32;
    y: i32;

    fn get_origin(): Point {
        Point { x: 0, y: 0 }
    }
}

origin = Point::get_origin();

seq {
    println_i32(origin.x);
    println_i32(origin.y);
};
",
        true,
    )?;

    assert_eq!(output, "0\n0\n");

    Ok(())
}

#[test]
fn struct_method_self_parameters_work() -> anyhow::Result<()> {
    let output = evaluate_program(
        "\
struct Person {
    age: u8;

    fn is_adult(self): bool {
        self.age >= 18u8
    }
}

seq {
    println_bool(Person::is_adult(Person { age: 17 }));
    println_bool(Person::is_adult(Person { age: 22 }));
};
",
        true,
    )?;

    assert_eq!(output, "false\ntrue\n");

    Ok(())
}

#[test]
fn nonexistent_struct_methods_cannot_be_called() -> anyhow::Result<()> {
    let output = evaluate_program(
        "\
struct Person {
    age: u8;
}

seq {
    println_bool(Person::is_awesome(Person { age: 22 }));
};
",
        false,
    )?;

    assert_eq!(
        output,
        "\
Error: Type `Person` has no method named `is_awesome`.

 4 │ 
 5 │ seq {
 6 │     println_bool(Person::is_awesome(Person { age: 22 }));
   │                          ^^^^^^^^^^
 7 │ };

",
    );

    Ok(())
}

#[test]
fn struct_method_self_parameters_must_be_first() -> anyhow::Result<()> {
    evaluate_program(
        "\
struct Person {
    age: u8;

    fn is_elder(other: Person, self): bool {
        self.age > other.age
    }
}
",
        false,
    )?;

    Ok(())
}

#[test]
fn structs_cannot_have_duplicate_fields() -> anyhow::Result<()> {
    let output = evaluate_program(
        "\
struct Person {
	age: u8;
	age: u8;
}
",
        false,
    )?;

    assert_eq!(
        output,
        "\
Error: A field with name `age` was already defined.

 1 │ struct Person {
 2 │ 	age: u8;
 3 │ 	age: u8;
   │ 	^^^^^^^
 4 │ }

",
    );

    Ok(())
}

#[test]
fn structs_cannot_have_duplicate_methods() -> anyhow::Result<()> {
    let output = evaluate_program(
        "\
struct Person {
	age: u8;

	fn is_adult(self): bool {
		self.age >= 18u8
	}

	fn is_adult(self): bool {
		self.age >= 18u8
	}
}
",
        false,
    )?;

    assert_eq!(
        output,
        "\
Error: A method with name `is_adult` was already defined.

  6 │ 	}
  7 │ 
  8 │ 	fn is_adult(self): bool {
    │ 	^^^^^^^^^^^^^^^^^^^^^^^^^
  9 │ 		self.age >= 18u8
 10 │ 	}
 11 │ }

",
    );

    Ok(())
}
