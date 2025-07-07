use crate::{
    error_reporting::{CompilationError, CompilationErrorMessage},
    parser::{
        program::{Identifier, Node, StructDefinition},
        type_::Type,
    },
    typechecker::built_in_values::BuiltInValues,
};
use std::collections::HashMap;

pub struct TypecheckerScope<'a> {
    built_in_values: &'a BuiltInValues,
    parent: Option<&'a TypecheckerScope<'a>>,
    structs: HashMap<String, StructDefinition>,
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

    pub fn declare_struct(&mut self, definition: StructDefinition) -> Result<(), CompilationError> {
        match self
            .structs
            .insert(definition.name.value.clone(), definition)
        {
            Some(existing) => Err(CompilationError {
                message: CompilationErrorMessage::StructAlreadyDefined {
                    name: existing.name.value.clone(),
                },

                position: Some(existing.name.get_position()),
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

    pub fn get_struct_definition(&self, name: &str) -> Option<&StructDefinition> {
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
