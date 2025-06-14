use anyhow::anyhow;
use std::{
    io::Write,
    process::{Command, Stdio},
};

pub fn evaluate_program(program: &str, expect_success: bool) -> anyhow::Result<String> {
    let ular_executable = std::env::current_exe()?
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("ular");

    let mut process = Command::new(ular_executable)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;

    process
        .stdin
        .as_mut()
        .unwrap()
        .write_all(program.as_bytes())?;

    let output = process.wait_with_output()?;

    if output.status.success() == expect_success {
        Ok(String::from_utf8(output.stdout)?)
    } else {
        std::io::stdout().write_all(&output.stdout)?;

        let expected = if expect_success {
            "a successful code"
        } else {
            "a failure code"
        };

        Err(match output.status.code() {
            Some(code) => anyhow!(
                "ular returned a status code of {}, but expected {}.",
                code,
                expected
            ),

            None => anyhow!("ular was unexpectedly terminated."),
        })
    }
}
