use crate::{
    error_reporting::{CompilationError, CompilationErrorMessage},
    parser::{
        program::{Identifier, Node},
        type_::Type,
    },
    simplifier::simple_program::SimpleStructDefinition,
};
use std::collections::HashMap;

pub(super) struct IndexableStructDefinition<'a> {
    pub(super) underlying: &'a SimpleStructDefinition,
    pub(super) field_indices: HashMap<&'a str, usize>,
    pub(super) method_indices: HashMap<&'a str, usize>,
}

#[derive(Eq, PartialEq)]
pub(super) enum TypecheckerScopeKind {
    Function,
    Other,
}

#[derive(Eq, PartialEq)]
pub(super) enum TypecheckerVariableKind {
    Function,
    Value,
}

pub(super) struct TypecheckerScope<'a> {
    parent: Option<&'a TypecheckerScope<'a>>,
    kind: TypecheckerScopeKind,
    structs: HashMap<&'a str, IndexableStructDefinition<'a>>,
    variables: HashMap<String, Variable>,
}

impl<'a> TypecheckerScope<'a> {
    pub(super) fn declare_and_validate_struct(
        &mut self,
        definition: &'a SimpleStructDefinition,
    ) -> Result<(), CompilationError> {
        let mut field_indices = HashMap::<&str, usize>::new();

        for (i, field) in definition.fields.iter().enumerate() {
            if field_indices.contains_key::<str>(&field.name.value) {
                return Err(CompilationError {
                    message: CompilationErrorMessage::StructFieldAlreadyDefined {
                        field_name: field.name.value.clone(),
                    },

                    position: Some(field.get_position()),
                });
            }

            field_indices.insert(&field.name.value, i);
        }

        let mut method_indices = HashMap::<&str, usize>::new();

        for (i, method) in definition.methods.iter().enumerate() {
            if method_indices.contains_key::<str>(&method.name.value) {
                return Err(CompilationError {
                    message: CompilationErrorMessage::StructMethodAlreadyDefined {
                        method_name: method.name.value.clone(),
                    },

                    position: Some(method.get_position()),
                });
            }

            method_indices.insert(&method.name.value, i);
        }

        let indexable = IndexableStructDefinition {
            underlying: definition,
            field_indices,
            method_indices,
        };

        match self.structs.insert(&definition.name.value, indexable) {
            Some(_) => Err(CompilationError {
                message: CompilationErrorMessage::StructAlreadyDefined {
                    name: definition.name.value.clone(),
                },

                position: Some(definition.name.get_position()),
            }),

            None => Ok(()),
        }
    }

    pub(super) fn declare_variable(
        &mut self,
        name: &Identifier,
        kind: TypecheckerVariableKind,
        type_: Type,
    ) -> Result<(), CompilationError> {
        match self
            .variables
            .insert(name.value.clone(), Variable { kind, type_ })
        {
            Some(_) => Err(CompilationError {
                message: CompilationErrorMessage::VariableAlreadyDefined {
                    name: name.value.clone(),
                },

                position: Some(name.get_position()),
            }),

            None => Ok(()),
        }
    }

    fn get_function_type(&self, name: &str) -> Option<Type> {
        self.variables
            .get(name)
            .filter(|variable| variable.kind == TypecheckerVariableKind::Function)
            .map(|variable| variable.type_.clone())
            .or_else(|| {
                self.parent
                    .and_then(|parent| parent.get_function_type(name))
            })
    }

    pub(super) fn get_struct_definition(&self, name: &str) -> Option<&IndexableStructDefinition> {
        self.structs.get(name).or_else(|| {
            self.parent
                .and_then(|parent| parent.get_struct_definition(name))
        })
    }

    pub(super) fn get_variable_type(&self, name: &str) -> Option<Type> {
        self.variables
            .get(name)
            .map(|variable| variable.type_.clone())
            .or_else(|| {
                self.parent.and_then(|parent| {
                    if self.kind == TypecheckerScopeKind::Function {
                        parent.get_function_type(name)
                    } else {
                        parent.get_variable_type(name)
                    }
                })
            })
    }

    pub(super) fn new(kind: TypecheckerScopeKind) -> Self {
        Self {
            parent: None,
            kind,
            structs: HashMap::new(),
            variables: HashMap::new(),
        }
    }

    pub(super) fn new_child(&self, kind: TypecheckerScopeKind) -> TypecheckerScope<'_> {
        TypecheckerScope {
            parent: Some(self),
            kind,
            structs: HashMap::new(),
            variables: HashMap::new(),
        }
    }
}

struct Variable {
    kind: TypecheckerVariableKind,
    type_: Type,
}
