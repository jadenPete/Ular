use ular_test_common::evaluate_program;

#[test]
fn test_string_literal() -> anyhow::Result<()> {
    let output = evaluate_program(
        r#"message = "Hello, world!";

println_str(message);
"#,
        true,
    )?;

    assert_eq!(output, "Hello, world!\n");

    Ok(())
}

#[test]
fn test_escaped_quote() -> anyhow::Result<()> {
    let output = evaluate_program(r#"println_str("\"");"#, true)?;

    assert_eq!(output, "\"\n");

    Ok(())
}

#[test]
fn test_escaped_backslash() -> anyhow::Result<()> {
    let output = evaluate_program(r#"println_str("\\");"#, true)?;

    assert_eq!(output, "\\\n");

    Ok(())
}

#[test]
fn test_escaped_newline() -> anyhow::Result<()> {
    let output = evaluate_program(r#"println_str("\n");"#, true)?;

    assert_eq!(output, "\n\n");

    Ok(())
}

#[test]
fn test_escaped_carriage_return() -> anyhow::Result<()> {
    let output1 = evaluate_program(r#"println_str("\r");"#, true)?;

    assert_eq!(output1, "\r\n");

    let output2 = evaluate_program(r#"println_str("\r\n");"#, true)?;

    assert_eq!(output2, "\r\n\n");

    Ok(())
}

#[test]
fn test_escaped_tab() -> anyhow::Result<()> {
    let output = evaluate_program(r#"println_str("\t");"#, true)?;

    assert_eq!(output, "\t\n");

    Ok(())
}

#[test]
fn test_line_continuation() -> anyhow::Result<()> {
    let output = evaluate_program(
        r#"println_str(
    "\
Hello, \
world!
"
);
"#,
        true,
    )?;

    assert_eq!(output, "Hello, world!\n\n");

    Ok(())
}

#[test]
fn test_invalid_escape() -> anyhow::Result<()> {
    evaluate_program(r#"println_str("\a");"#, false)?;

    Ok(())
}

#[test]
fn test_explicit_string_type() -> anyhow::Result<()> {
    let output = evaluate_program(
        r#"fn println_str_alias(string: str) {
    println_str(string);
}

println_str_alias("Hello, world!");
"#,
        true,
    )?;

    assert_eq!(output, "Hello, world!\n");

    Ok(())
}
