mod data_structures;
mod dependency_analyzer;
mod error_reporting;
mod jit_compiler;
mod lexer;
mod mmtk;
mod parser;
mod phase;
mod simplifier;
mod typechecker;

use crate::{
    dependency_analyzer::AnalyzerPhase,
    jit_compiler::compile_and_execute_program,
    lexer::{token::Tokens, LexerPhase},
    parser::ParserPhase,
    phase::Phase,
    simplifier::SimplifierPhase,
    typechecker::TypecheckerPhase,
};
use clap::Parser;
use error_reporting::{report_error, CompilationError};
use log::LevelFilter;
use simplelog::{ColorChoice, ConfigBuilder, TermLogger, TerminalMode};
use std::{fmt::Display, io::Read, process::ExitCode};

#[derive(Parser)]
struct Arguments {
    /// The name of a phase whose output to print. This can be supplied multiple times. Accepted values are "lexer", "parser", "simplifier", "typechecker", or "jit_compiler"
    #[arg(long)]
    debug_phase: Vec<String>,
}

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

    let debug_phases = arguments.debug_phase.as_slice();
    let tokens = unwrap_lexer_or_parser_result(LexerPhase.execute_and_debug(&buffer, debug_phases));
    let ast = unwrap_lexer_or_parser_result(ParserPhase.execute_and_debug(
        Tokens {
            tokens: tokens.as_slice(),
            source: &buffer,
        },
        debug_phases,
    ));

    let simplified_ast = SimplifierPhase
        .execute_and_debug(&ast, debug_phases)
        .unwrap();

    let typed_ast = unwrap_phase_result(
        TypecheckerPhase.execute_and_debug(&simplified_ast, debug_phases),
        &buffer,
    );

    let analyzed_ast = unwrap_phase_result(
        AnalyzerPhase.execute_and_debug(&typed_ast, debug_phases),
        &buffer,
    );

    let return_code = unwrap_phase_result(
        compile_and_execute_program(
            &analyzed_ast,
            arguments
                .debug_phase
                .iter()
                .any(|name| name == "jit_compiler"),
        ),
        &buffer,
    );

    Ok(ExitCode::from(return_code))
}

fn unwrap_lexer_or_parser_result<Output, Error: Display>(result: Result<Output, Error>) -> Output {
    result.unwrap_or_else(|error| {
        println!("{}", error);

        std::process::exit(1)
    })
}

fn unwrap_phase_result<A>(result: Result<A, CompilationError>, source: &str) -> A {
    result.unwrap_or_else(|error| report_error(error, source))
}
