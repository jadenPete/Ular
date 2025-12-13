pub mod arguments;
mod data_structures;
mod dependency_analyzer;
pub mod error_reporting;
pub mod jit_compiler;
mod lexer;
mod libunwind;
pub mod mmtk;
pub mod parser;
mod phase;
mod simplifier;
mod typechecker;

use crate::{
    arguments::Arguments,
    dependency_analyzer::AnalyzerPhase,
    error_reporting::CompilationError,
    jit_compiler::{module::built_in_values::BuiltInValue, ExecutorPhase, JitCompilerPhase},
    lexer::{token::Tokens, LexerPhase},
    parser::{type_::Type, ParserPhase},
    phase::Phase,
    simplifier::SimplifierPhase,
    typechecker::TypecheckerPhase,
};
use inkwell::context::Context;
use std::collections::HashMap;

pub struct AdditionalValue<'a> {
    pub built_in_value: Box<dyn BuiltInValue<'a> + 'a>,
    pub type_: Type,
}

pub fn run_phases<'a>(
    context: &'a Context,
    source: &str,
    arguments: &Arguments,
    additional_values: HashMap<String, AdditionalValue<'a>>,
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
    let additional_value_types: HashMap<String, Type> = additional_values
        .iter()
        .map(|(key, value)| (key.clone(), value.type_.clone()))
        .collect();

    let typechecker_phase = TypecheckerPhase {
        additional_values: additional_value_types.clone(),
    };

    let typed_program = typechecker_phase.execute_and_debug(&simplified_program, arguments)?;
    let analyzer_phase = AnalyzerPhase {
        additional_values: additional_value_types,
    };

    let analyzed_program = analyzer_phase.execute_and_debug(&typed_program, arguments)?;
    let jit_compiler_phase = JitCompilerPhase {
        context,
        garbage_collection_plan: arguments.gc_plan,
        print_stack_map: arguments.print_stack_map,
        additional_values: additional_values
            .iter()
            .map(|(key, value)| (key.clone(), value.built_in_value.clone()))
            .collect(),
    };

    let compiled_program = jit_compiler_phase.execute_and_debug(&analyzed_program, arguments)?;

    ExecutorPhase.execute_and_debug(&compiled_program, arguments)
}
