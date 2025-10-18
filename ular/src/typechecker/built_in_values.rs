use crate::parser::type_::{FunctionType, NumericType, Type};
use std::collections::HashMap;

pub struct BuiltInValues {
    value_types: HashMap<String, Type>,
}

impl BuiltInValues {
    pub fn get_value_type(&self, name: &str) -> Option<Type> {
        self.value_types.get(name).cloned()
    }

    pub fn new(additional_values: HashMap<String, Type>) -> Self {
        let mut value_types = additional_values;

        value_types.insert(
            String::from("println_bool"),
            Type::Function(FunctionType {
                parameters: vec![Type::Bool],
                return_type: Box::new(Type::Unit),
            }),
        );

        for numeric_type in enum_iterator::all::<NumericType>() {
            value_types.insert(
                format!("println_{}", numeric_type),
                Type::Function(FunctionType {
                    parameters: vec![Type::Numeric(numeric_type)],
                    return_type: Box::new(Type::Unit),
                }),
            );
        }

        value_types.insert(String::from("true"), Type::Bool);
        value_types.insert(String::from("false"), Type::Bool);

        Self { value_types }
    }
}
