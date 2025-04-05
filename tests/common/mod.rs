use anyhow::anyhow;
use std::{
    io::Write,
    process::{Command, Stdio},
};

pub fn evaluate_program(program: &str) -> anyhow::Result<String> {
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

    if output.status.success() {
        Ok(String::from_utf8(output.stdout)?)
    } else {
        std::io::stdout().write_all(&output.stdout)?;

        Err(match output.status.code() {
            Some(code) => anyhow!("ular returned a status code of {}.", code),
            None => anyhow!("ular was unexpectedly terminated."),
        })
    }
}
