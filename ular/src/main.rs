use clap::Parser;
use inkwell::context::Context;
use log::LevelFilter;
use simplelog::{ColorChoice, ConfigBuilder, TermLogger, TerminalMode};
use std::collections::HashMap;
use std::{io::Read, process::ExitCode};
use ular::arguments::Arguments;
use ular::error_reporting::report_error;
use ular::run_phases;

fn main() -> std::io::Result<ExitCode> {
    let arguments = Arguments::parse();

    TermLogger::init(
        if cfg!(debug_assertions) {
            LevelFilter::Info
        } else {
            LevelFilter::Warn
        },
        ConfigBuilder::new()
            .set_target_level(LevelFilter::Off)
            .set_thread_level(LevelFilter::Off)
            .set_time_level(LevelFilter::Off)
            .build(),
        TerminalMode::Stderr,
        ColorChoice::Auto,
    )
    .unwrap();

    let mut buffer = String::new();

    std::io::stdin().read_to_string(&mut buffer)?;

    let context = Context::create();
    let return_code = run_phases(&context, &buffer, &arguments, HashMap::new())
        .unwrap_or_else(|error| report_error(error, &buffer));

    Ok(ExitCode::from(return_code))
}
