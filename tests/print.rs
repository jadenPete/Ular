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
fn println_i8_works() -> anyhow::Result<()> {
    let output = evaluate_program(
        &format!(
            "\
println_i8({});
println_i8(0);
println_i8({});
",
            i8::MIN,
            i8::MAX,
        ),
        true,
    )?;

    assert_eq!(output, format!("{}\n0\n{}\n", i8::MIN, i8::MAX));

    Ok(())
}

#[test]
fn println_i16_works() -> anyhow::Result<()> {
    let output = evaluate_program(
        &format!(
            "\
println_i16({});
println_i16(0);
println_i16({});
",
            i16::MIN,
            i16::MAX,
        ),
        true,
    )?;

    assert_eq!(output, format!("{}\n0\n{}\n", i16::MIN, i16::MAX));

    Ok(())
}

#[test]
fn println_i32_works() -> anyhow::Result<()> {
    let output = evaluate_program(
        &format!(
            "\
println_i32({});
println_i32(0);
println_i32({});
",
            i32::MIN,
            i32::MAX,
        ),
        true,
    )?;

    assert_eq!(output, format!("{}\n0\n{}\n", i32::MIN, i32::MAX));

    Ok(())
}

#[test]
fn println_i64_works() -> anyhow::Result<()> {
    let output = evaluate_program(
        &format!(
            "\
println_i64({});
println_i64(0);
println_i64({});
",
            i64::MIN,
            i64::MAX,
        ),
        true,
    )?;

    assert_eq!(output, format!("{}\n0\n{}\n", i64::MIN, i64::MAX));

    Ok(())
}

#[test]
fn println_u8_works() -> anyhow::Result<()> {
    let output = evaluate_program(
        &format!(
            "\
println_u8(0);
println_u8({});
",
            u8::MAX,
        ),
        true,
    )?;

    assert_eq!(output, format!("0\n{}\n", u8::MAX));

    Ok(())
}

#[test]
fn println_u16_works() -> anyhow::Result<()> {
    let output = evaluate_program(
        &format!(
            "\
println_u16(0);
println_u16({});
",
            u16::MAX,
        ),
        true,
    )?;

    assert_eq!(output, format!("0\n{}\n", u16::MAX));

    Ok(())
}

#[test]
fn println_u32_works() -> anyhow::Result<()> {
    let output = evaluate_program(
        &format!(
            "\
println_u32(0);
println_u32({});
",
            u32::MAX,
        ),
        true,
    )?;

    assert_eq!(output, format!("0\n{}\n", u32::MAX));

    Ok(())
}

#[test]
fn println_u64_works() -> anyhow::Result<()> {
    let output = evaluate_program(
        &format!(
            "\
println_u64(0);
println_u64({});
",
            u64::MAX,
        ),
        true,
    )?;

    assert_eq!(output, format!("0\n{}\n", u64::MAX));

    Ok(())
}
