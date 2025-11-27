use crate::{
    error_reporting::{CompilationError, CompilationErrorMessage},
    parser::{
        program::{Identifier, Node},
        type_::Type,
    },
    simplifier::simple_program::SimpleStructDefinition,
};
use std::collections::HashMap;

pub struct IndexableStructDefinition<'a> {
    pub underlying: &'a SimpleStructDefinition,
    pub field_indices: HashMap<&'a str, usize>,
    pub method_indices: HashMap<&'a str, usize>,
}

pub struct TypecheckerScope<'a> {
    parent: Option<&'a TypecheckerScope<'a>>,
    structs: HashMap<&'a str, IndexableStructDefinition<'a>>,
    variable_types: HashMap<String, Type>,
}

impl<'a> TypecheckerScope<'a> {
    pub fn with_parent(parent: &'a TypecheckerScope<'a>) -> Self {
        Self {
            parent: Some(parent),
            structs: HashMap::new(),
            variable_types: HashMap::new(),
        }
    }

    pub fn without_parent() -> Self {
        Self {
            parent: None,
            structs: HashMap::new(),
            variable_types: HashMap::new(),
        }
    }

    pub fn declare_and_validate_struct(
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

    pub fn declare_variable(
        &mut self,
        name: &Identifier,
        type_: Type,
    ) -> Result<(), CompilationError> {
        match self.variable_types.insert(name.value.clone(), type_) {
            Some(_) => Err(CompilationError {
                message: CompilationErrorMessage::VariableAlreadyDefined {
                    name: name.value.clone(),
                },

                position: Some(name.get_position()),
            }),

            None => Ok(()),
        }
    }

    pub fn get_struct_definition(&self, name: &str) -> Option<&IndexableStructDefinition> {
        self.structs.get(name).or_else(|| {
            self.parent
                .and_then(|parent| parent.get_struct_definition(name))
        })
    }

    pub fn get_variable_type(&self, name: &str) -> Option<Type> {
        self.variable_types.get(name).cloned().or_else(|| {
            self.parent
                .and_then(|parent| parent.get_variable_type(name))
        })
    }
}
