use crate::{
    error_reporting::{CompilationError, CompilationErrorMessage, InternalError},
    jit_compiler::{built_in_values::BuiltInValues, module::UlarModule, value::UlarValue},
    parser::program::{Identifier, Node},
};
use inkwell::{builder::Builder, context::Context, execution_engine::ExecutionEngine};
use std::{cmp::Eq, collections::HashMap};

pub struct JitCompilerScope<'a, 'context> {
    parent: Option<&'a JitCompilerScope<'a, 'context>>,
    next_local_name: LocalName,
    variable_values: HashMap<String, UlarValue<'context>>,
}

impl<'a, 'context> JitCompilerScope<'a, 'context> {
    pub fn new(parent: Option<&'a JitCompilerScope<'a, 'context>>) -> Self {
        let next_local_name = match parent {
            Some(parent) => parent.next_local_name,
            None => LocalName(0),
        };

        Self {
            parent,
            next_local_name,
            variable_values: HashMap::new(),
        }
    }

    pub fn declare_variable(
        &mut self,
        name: &Identifier,
        value: UlarValue<'context>,
    ) -> Result<(), CompilationError> {
        match self.variable_values.insert(name.value.clone(), value) {
            Some(_) => Err(CompilationError {
                message: CompilationErrorMessage::InternalError(
                    InternalError::JitCompilerVariableAlreadyDefined {
                        name: name.value.clone(),
                    },
                ),

                position: Some(name.get_position()),
            }),

            None => Ok(()),
        }
    }

    pub fn get_local_name(&mut self) -> LocalName {
        let result = self.next_local_name;

        self.next_local_name = LocalName(self.next_local_name.0 + 1);

        result
    }

    pub fn get_variable_value(
        &self,
        variable_name: &str,
        local_name: LocalName,
        context: &'context Context,
        builder: &Builder<'context>,
        built_in_values: &mut BuiltInValues<'context>,
        execution_engine: &ExecutionEngine<'context>,
        module: &mut UlarModule<'context>,
    ) -> Option<UlarValue<'context>> {
        self.variable_values
            .get(variable_name)
            .copied()
            .or_else(|| {
                self.parent.and_then(|parent| {
                    parent.get_variable_value(
                        variable_name,
                        local_name,
                        context,
                        builder,
                        built_in_values,
                        execution_engine,
                        module,
                    )
                })
            })
            .or_else(|| {
                built_in_values.get(
                    variable_name,
                    local_name,
                    context,
                    builder,
                    execution_engine,
                    module,
                )
            })
    }

    pub fn has_parent(&self) -> bool {
        self.parent.is_some()
    }

    pub fn with_child<A: FnOnce(&mut JitCompilerScope<'_, 'context>) -> B, B>(
        &mut self,
        callback: A,
    ) -> B {
        let mut child = JitCompilerScope::new(Some(self));
        let result = callback(&mut child);

        self.next_local_name = child.next_local_name;

        result
    }
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct LocalName(u32);

impl ToString for LocalName {
    fn to_string(&self) -> String {
        self.0.to_string()
    }
}
