pub mod arguments;
mod data_structures;
mod dependency_analyzer;
pub mod error_reporting;
mod jit_compiler;
mod lexer;
mod mmtk;
mod parser;
mod phase;
mod simplifier;
mod typechecker;

use inkwell::context::Context;

use crate::{
    arguments::Arguments,
    dependency_analyzer::AnalyzerPhase,
    error_reporting::CompilationError,
    jit_compiler::{ExecutorPhase, JitCompilerPhase},
    lexer::{token::Tokens, LexerPhase},
    parser::ParserPhase,
    phase::Phase,
    simplifier::SimplifierPhase,
    typechecker::TypecheckerPhase,
};

pub fn run_phases(
    context: &Context,
    source: &str,
    arguments: &Arguments,
) -> Result<u8, CompilationError> {
    let tokens = LexerPhase.execute_and_debug(source, arguments)?;
    let program = ParserPhase.execute_and_debug(
        Tokens {
            tokens: &tokens,
            source,
        },
        arguments,
    )?;

    let simplified_program = SimplifierPhase.execute_and_debug(&program, arguments)?;
    let typed_program = TypecheckerPhase.execute_and_debug(&simplified_program, arguments)?;
    let analyzed_program = AnalyzerPhase.execute_and_debug(&typed_program, arguments)?;
    let jit_compiler_phase = JitCompilerPhase {
        context,
        print_stack_map: arguments.print_stack_map,
    };

    let compiled_program = jit_compiler_phase.execute_and_debug(&analyzed_program, arguments)?;

    ExecutorPhase.execute_and_debug(&compiled_program, arguments)
}
