use crate::{
    error_reporting::{CompilationError, CompilationErrorMessage},
    parser::{
        program::{Identifier, Node},
        type_::Type,
    },
    simplifier::simple_program::SimpleStructDefinition,
    typechecker::built_in_values::BuiltInValues,
};
use std::collections::HashMap;

pub struct IndexableStructDefinition<'a> {
    pub underlying: &'a SimpleStructDefinition,
    pub field_indices: HashMap<&'a str, usize>,
    pub method_indices: HashMap<&'a str, usize>,
}

pub struct TypecheckerScope<'a> {
    built_in_values: &'a BuiltInValues,
    parent: Option<&'a TypecheckerScope<'a>>,
    structs: HashMap<&'a str, IndexableStructDefinition<'a>>,
    variable_types: HashMap<String, Type>,
}

impl<'a> TypecheckerScope<'a> {
    pub fn with_parent(parent: &'a TypecheckerScope<'a>) -> Self {
        Self {
            built_in_values: parent.built_in_values,
            parent: Some(parent),
            structs: HashMap::new(),
            variable_types: HashMap::new(),
        }
    }

    pub fn without_parent(built_in_values: &'a BuiltInValues) -> Self {
        Self {
            built_in_values,
            parent: None,
            structs: HashMap::new(),
            variable_types: HashMap::new(),
        }
    }

    pub fn declare_struct(
        &mut self,
        definition: &'a SimpleStructDefinition,
    ) -> Result<(), CompilationError> {
        let indexable = IndexableStructDefinition {
            underlying: definition,
            field_indices: definition
                .fields
                .iter()
                .enumerate()
                .map::<(&str, usize), _>(|(i, field)| (&field.name.value, i))
                .collect(),

            method_indices: definition
                .methods
                .iter()
                .enumerate()
                .map::<(&str, usize), _>(|(i, method)| (&method.name.value, i))
                .collect(),
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
        self.variable_types
            .get(name)
            .cloned()
            .or_else(|| {
                self.parent
                    .and_then(|parent| parent.get_variable_type(name))
            })
            .or_else(|| self.built_in_values.get_value_type(name))
    }
}
