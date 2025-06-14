use crate::{
    dependency_analyzer::analyzed_program::AnalyzedExpressionRef,
    error_reporting::{CompilationError, CompilationErrorMessage},
    parser::program::{Identifier, Node},
    typechecker::{
        built_in_values::BuiltInValues,
        typed_program::{Typed, TypedIdentifier},
    },
};
use std::collections::HashMap;

pub struct AnalyzerScope<'a> {
    built_in_values: &'a BuiltInValues,
    parent: Option<&'a AnalyzerScope<'a>>,
    variables: HashMap<String, AnalyzedExpressionRef>,
}

impl<'a> AnalyzerScope<'a> {
    pub fn declare_variable(
        &mut self,
        name: &Identifier,
        reference: AnalyzedExpressionRef,
    ) -> Result<(), CompilationError> {
        match self.variables.insert(name.value.clone(), reference) {
            Some(_) => Err(CompilationError {
                message: CompilationErrorMessage::VariableAlreadyDefined {
                    name: name.value.clone(),
                },

                position: Some(name.get_position()),
            }),

            None => Ok(()),
        }
    }

    pub fn get_variable(&self, name: &TypedIdentifier) -> Option<AnalyzedExpressionRef> {
        self.variables
            .get(&name.underlying.value)
            .map(|reference| reference.with_position(name.get_position()))
            .or_else(|| self.parent.and_then(|parent| parent.get_variable(name)))
            .or_else(|| {
                self.built_in_values
                    .get_value_type(&name.underlying.value)
                    .map(|_| AnalyzedExpressionRef::BuiltIn {
                        name: name.underlying.value.clone(),
                        type_: name.get_type(),
                        position: name.get_position(),
                    })
            })
    }

    pub fn has_parent(&self) -> bool {
        self.parent.is_some()
    }

    pub fn with_parent(parent: &'a AnalyzerScope<'a>) -> Self {
        Self {
            built_in_values: parent.built_in_values,
            parent: Some(parent),
            variables: HashMap::new(),
        }
    }

    pub fn without_parent(built_in_values: &'a BuiltInValues) -> Self {
        Self {
            built_in_values,
            parent: None,
            variables: HashMap::new(),
        }
    }
}
