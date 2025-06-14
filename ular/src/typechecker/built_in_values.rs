use crate::parser::type_::{FunctionType, NumericType, Type};
use std::{collections::HashMap, sync::OnceLock};

static GLOBAL_BUILT_IN_VALUES: OnceLock<BuiltInValues> = OnceLock::new();

pub struct BuiltInValues {
    value_types: HashMap<String, Type>,
}

impl BuiltInValues {
    pub fn get_value_type(&self, name: &str) -> Option<Type> {
        self.value_types.get(name).map(|value| value.clone())
    }

    pub fn global() -> &'static Self {
        GLOBAL_BUILT_IN_VALUES.get_or_init(Self::new)
    }

    fn new() -> Self {
        let mut value_types = HashMap::new();

        value_types.insert(
            String::from("println_bool"),
            Type::Function(FunctionType {
                parameters: vec![Type::Bool],
                return_type: Box::new(Type::Unit),
            }),
        );

        for numeric_type in enum_iterator::all::<NumericType>() {
            value_types.insert(
                String::from(format!("println_{}", numeric_type)),
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
