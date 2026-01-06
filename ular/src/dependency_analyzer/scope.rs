use crate::{
    data_structures::graph::DirectedGraph,
    dependency_analyzer::{
        analyzed_program::{
            AnalyzedExpression, AnalyzedExpressionRef, AnalyzedFunctionDefinition,
            AnalyzedFunctionType, AnalyzedProgram, AnalyzedStructDefinition, AnalyzedType,
        },
        DefinitionMap,
    },
    error_reporting::{CompilationError, CompilationErrorMessage, InternalError, Position},
    parser::{
        program::{Identifier, Node},
        type_::{FunctionType, Type},
    },
    phase::built_in_values::{BuiltInPath, BuiltInPathBuf},
    typechecker::{built_in_values::TypecheckerBuiltInValues, typed_program::TypedIdentifier},
};
use std::collections::HashMap;

pub(super) struct AnalyzerScope<'a> {
    built_in_values: &'a TypecheckerBuiltInValues,
    parent: Option<&'a AnalyzerScope<'a>>,
    struct_indices: HashMap<String, usize>,
    variables: HashMap<String, AnalyzedExpressionRef>,
}

impl<'a> AnalyzerScope<'a> {
    pub(super) fn analyze_function_type(
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

    pub(super) fn analyze_type(&self, type_: &Type) -> Result<AnalyzedType, CompilationError> {
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
            Type::Str => AnalyzedType::Str,
            Type::Unit => AnalyzedType::Unit,
        })
    }

    pub(super) fn declare_struct(
        &mut self,
        name: String,
        context: &mut AnalyzerScopeContext,
    ) -> usize {
        let i = context.structs.reserve_definition();

        self.struct_indices.insert(name, i);

        i
    }

    pub(super) fn declare_variable(
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

    pub(super) fn define_struct(
        &mut self,
        i: usize,
        definition: AnalyzedStructDefinition,
        context: &mut AnalyzerScopeContext,
    ) {
        context.structs.set_definition(i, definition);
    }

    pub(super) fn get_struct_index(&self, name: &str) -> Option<usize> {
        self.struct_indices
            .get(name)
            .copied()
            .or_else(|| self.parent.and_then(|parent| parent.get_struct_index(name)))
    }

    pub(super) fn get_variable(
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
            match self
                .built_in_values
                .get(&BuiltInPath::Identifier(&name.underlying.value))
            {
                Some(_) => Some(AnalyzedExpressionRef::BuiltIn {
                    path: BuiltInPathBuf::Identifier(name.underlying.value.clone()),
                    type_: self.analyze_type(&name.type_)?,
                    position: name.get_position(),
                }),

                None => None,
            },
        )
    }

    pub(super) fn with_parent(parent: &'a AnalyzerScope<'a>) -> Self {
        Self {
            built_in_values: parent.built_in_values,
            parent: Some(parent),
            struct_indices: HashMap::new(),
            variables: HashMap::new(),
        }
    }

    pub(super) fn without_parent(built_in_values: &'a TypecheckerBuiltInValues) -> Self {
        Self {
            built_in_values,
            parent: None,
            struct_indices: HashMap::new(),
            variables: HashMap::new(),
        }
    }
}

pub(super) struct AnalyzerScopeContext {
    functions: DefinitionMap<AnalyzedFunctionDefinition>,
    string_literals: Vec<String>,
    structs: DefinitionMap<AnalyzedStructDefinition>,
}

impl AnalyzerScopeContext {
    pub(super) fn add_string_literal(&mut self, string_literal: String) -> usize {
        let i = self.string_literals.len();

        self.string_literals.push(string_literal);

        i
    }

    pub(super) fn functions_mut(&mut self) -> &mut DefinitionMap<AnalyzedFunctionDefinition> {
        &mut self.functions
    }

    pub(super) fn into_program(
        self,
        expression_graph: DirectedGraph<AnalyzedExpression>,
        position: Position,
    ) -> Result<AnalyzedProgram, CompilationError> {
        let structs = self.structs.into_struct_vec()?;
        let functions = self.functions.into_function_vec()?;

        Ok(AnalyzedProgram {
            string_literals: self.string_literals,
            structs,
            functions,
            expression_graph,
            position,
        })
    }

    pub(super) fn new() -> Self {
        Self {
            functions: DefinitionMap::new(),
            string_literals: Vec::new(),
            structs: DefinitionMap::new(),
        }
    }
}
