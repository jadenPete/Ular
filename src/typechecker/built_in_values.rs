use crate::typechecker::type_::{FunctionType, Type};
use std::collections::HashMap;

pub struct BuiltInValues {
    value_types: HashMap<String, Type>,
}

impl BuiltInValues {
    pub fn get_value_type(&self, name: &str) -> Option<Type> {
        self.value_types.get(name).map(|value| value.clone())
    }

    pub fn new() -> Self {
        let mut value_types = HashMap::new();

        value_types.insert(
            String::from("println_bool"),
            Type::Function(FunctionType {
                parameters: vec![Type::Boolean],
                result: Box::new(Type::Unit),
            }),
        );

        value_types.insert(
            String::from("println_number"),
            Type::Function(FunctionType {
                parameters: vec![Type::Number],
                result: Box::new(Type::Unit),
            }),
        );

        value_types.insert(String::from("true"), Type::Boolean);
        value_types.insert(String::from("false"), Type::Boolean);

        Self { value_types }
    }
}
