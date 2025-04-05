mod error;
mod jit_compiler;
mod lexer;
mod parser;
mod phase;
mod simplifier;
mod typechecker;

use crate::{
    jit_compiler::compile_and_execute_program,
    lexer::{token::Tokens, LexerPhase},
    parser::ParserPhase,
    phase::Phase,
    simplifier::SimplifierPhase,
    typechecker::TypecheckerPhase,
};

use clap::Parser;
use log::LevelFilter;
use simplelog::{ColorChoice, ConfigBuilder, TermLogger, TerminalMode};
use std::{fmt::Display, io::Read};

#[derive(Parser)]
struct Arguments {
    /// The name of a phase whose output to print. This can be supplied multiple times. Accepted values are "lexer", "parser", "simplifier", "typechecker", or "jit_compiler"
    #[arg(long)]
    debug_phase: Vec<String>,
}

fn main() -> std::io::Result<()> {
    let arguments = Arguments::parse();

    TermLogger::init(
        LevelFilter::Debug,
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

    let debug_phases = arguments.debug_phase.as_slice();
    let tokens = unwrap_phase_result(LexerPhase.execute_and_debug(&buffer, debug_phases));
    let ast = unwrap_phase_result(ParserPhase.execute_and_debug(
        Tokens {
            tokens: tokens.as_slice(),
            source: &buffer,
        },
        debug_phases,
    ));

    let simplified_ast = SimplifierPhase
        .execute_and_debug(&ast, debug_phases)
        .unwrap();

    let typed_ast =
        unwrap_phase_result(TypecheckerPhase.execute_and_debug(&simplified_ast, debug_phases));

    if let Err(error) = compile_and_execute_program(
        &typed_ast,
        arguments
            .debug_phase
            .iter()
            .any(|name| name == "jit_compiler"),
    ) {
        eprintln!("{}", error);

        std::process::exit(1);
    }

    Ok(())
}

fn unwrap_phase_result<Output, Error: Display>(result: Result<Output, Error>) -> Output {
    result.unwrap_or_else(|error| {
        eprintln!("{}", error);

        std::process::exit(1)
    })
}
