use crate::{
    data_structures::number_map::NumberMap,
    dependency_analyzer::analyzed_program::{
        AnalyzedExpressionRef, AnalyzedFunctionType, AnalyzedStructDefinition, AnalyzedType,
    },
    error_reporting::{CompilationError, CompilationErrorMessage, InternalError},
    parser::{
        program::{Identifier, Node},
        type_::{FunctionType, Type},
    },
    typechecker::{built_in_values::BuiltInValues, typed_program::TypedIdentifier},
};
use std::collections::HashMap;

pub struct AnalyzerScope<'a> {
    built_in_values: &'a BuiltInValues,
    parent: Option<&'a AnalyzerScope<'a>>,
    struct_indices: HashMap<String, usize>,
    variables: HashMap<String, AnalyzedExpressionRef>,
}

impl<'a> AnalyzerScope<'a> {
    pub fn analyze_function_type(
        &self,
        function_type: &FunctionType,
    ) -> Result<AnalyzedFunctionType, CompilationError> {
        Ok(AnalyzedFunctionType {
            parameters: function_type
                .parameters
                .iter()
                .map(|parameter_type| self.analyze_type(parameter_type))
                .collect::<Result<_, _>>()?,

            return_type: Box::new(self.analyze_type(&function_type.return_type)?),
        })
    }

    pub fn analyze_type(&self, type_: &Type) -> Result<AnalyzedType, CompilationError> {
        Ok(match type_ {
            Type::Bool => AnalyzedType::Bool,
            Type::Function(function_type) => {
                AnalyzedType::Function(self.analyze_function_type(function_type)?)
            }

            Type::Identifier(value) => {
                let i = self
                    .get_struct_index(value)
                    .ok_or_else(|| CompilationError {
                        message: CompilationErrorMessage::InternalError(
                            InternalError::UnknownType {
                                name: value.clone(),
                            },
                        ),
                        position: None,
                    })?;

                AnalyzedType::Struct(i)
            }

            Type::Numeric(numeric_type) => AnalyzedType::Numeric(*numeric_type),
            Type::Unit => AnalyzedType::Unit,
        })
    }

    pub fn declare_struct(&mut self, name: String, context: &mut AnalyzerScopeContext) -> usize {
        let i = context.struct_count;

        context.struct_count += 1;

        self.struct_indices.insert(name, i);

        i
    }

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

    pub fn define_struct(
        &mut self,
        i: usize,
        definition: AnalyzedStructDefinition,
        context: &mut AnalyzerScopeContext,
    ) {
        context.structs.insert(i, definition);
    }

    pub fn get_struct_index(&self, name: &str) -> Option<usize> {
        self.struct_indices
            .get(name)
            .copied()
            .or_else(|| self.parent.and_then(|parent| parent.get_struct_index(name)))
    }

    pub fn get_variable(
        &self,
        name: &TypedIdentifier,
    ) -> Result<Option<AnalyzedExpressionRef>, CompilationError> {
        if let Some(reference) = self.variables.get(&name.underlying.value) {
            return Ok(Some(reference.with_position(name.get_position())));
        }

        if let Some(parent) = self.parent {
            if let Some(result) = parent.get_variable(name)? {
                return Ok(Some(result));
            }
        }

        Ok(
            match self.built_in_values.get_value_type(&name.underlying.value) {
                Some(_) => Some(AnalyzedExpressionRef::BuiltIn {
                    name: name.underlying.value.clone(),
                    type_: self.analyze_type(&name.type_)?,
                    position: name.get_position(),
                }),

                None => None,
            },
        )
    }

    pub fn has_parent(&self) -> bool {
        self.parent.is_some()
    }

    pub fn with_parent(parent: &'a AnalyzerScope<'a>) -> Self {
        Self {
            built_in_values: parent.built_in_values,
            parent: Some(parent),
            struct_indices: HashMap::new(),
            variables: HashMap::new(),
        }
    }

    pub fn without_parent(built_in_values: &'a BuiltInValues) -> Self {
        Self {
            built_in_values,
            parent: None,
            struct_indices: HashMap::new(),
            variables: HashMap::new(),
        }
    }
}

pub struct AnalyzerScopeContext {
    struct_count: usize,
    structs: NumberMap<AnalyzedStructDefinition>,
}

impl AnalyzerScopeContext {
    pub fn into_structs(self) -> Result<Vec<AnalyzedStructDefinition>, CompilationError> {
        self.structs
            .into_contiguous_values(self.struct_count)
            .map_err(|error| CompilationError {
                message: CompilationErrorMessage::InternalError(
                    InternalError::AnalyzerStructNotDefined { index: error.index },
                ),
                position: None,
            })
    }

    pub fn new() -> Self {
        Self {
            struct_count: 0,
            structs: NumberMap::new(0),
        }
    }
}
