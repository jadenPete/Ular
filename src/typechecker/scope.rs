use crate::{
    parser::program::Identifier,
    typechecker::{
        built_in_values::BuiltInValues,
        typed_program::{TypedExpression, TypedIdentifier},
    },
};
use std::collections::HashMap;

pub struct TypecheckerScope<'a> {
    built_in_values: &'a BuiltInValues,
    parent: Option<&'a TypecheckerScope<'a>>,
    variable_values: HashMap<String, TypedExpression>,
}

impl<'a> TypecheckerScope<'a> {
    pub fn with_parent(parent: &'a TypecheckerScope<'a>) -> Self {
        Self {
            built_in_values: parent.built_in_values,
            parent: Some(parent),
            variable_values: HashMap::new(),
        }
    }

    pub fn without_parent(built_in_values: &'a BuiltInValues) -> Self {
        Self {
            built_in_values,
            parent: None,
            variable_values: HashMap::new(),
        }
    }

    pub fn declare_variable(&mut self, name: String, value: TypedExpression) -> bool {
        let result = self.variable_values.contains_key(&name);

        if !result {
            self.variable_values.insert(name, value);
        }

        result
    }

    pub fn get_variable_value(&self, name: &str) -> Option<TypedExpression> {
        self.variable_values
            .get(name)
            .map(|value| value.clone())
            .or_else(|| {
                self.parent
                    .and_then(|parent| parent.get_variable_value(name))
            })
            .or_else(|| {
                self.built_in_values.get_value_type(name).map(|type_| {
                    TypedExpression::Identifier(TypedIdentifier {
                        underlying: Identifier(String::from(name)),
                        type_,
                    })
                })
            })
    }
}
