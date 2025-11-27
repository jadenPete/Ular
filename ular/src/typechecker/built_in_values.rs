use crate::{
    parser::type_::{FunctionType, NumericType, Type},
    phase::built_in_values::{BuiltInValueProducer, BuiltInValues},
};

pub struct TypecheckerBuiltInValueProducer;

impl BuiltInValueProducer for TypecheckerBuiltInValueProducer {
    type Value = Type;

    fn get_println_bool(&self, _name: &str) -> Self::Value {
        Type::Function(FunctionType {
            parameters: vec![Type::Bool],
            return_type: Box::new(Type::Unit),
        })
    }

    fn get_println_numeric(&self, _name: &str, numeric_type: NumericType) -> Self::Value {
        Type::Function(FunctionType {
            parameters: vec![Type::Numeric(numeric_type)],
            return_type: Box::new(Type::Unit),
        })
    }

    fn get_println_str(&self, _name: &str) -> Self::Value {
        Type::Function(FunctionType {
            parameters: vec![Type::Str],
            return_type: Box::new(Type::Unit),
        })
    }

    fn get_true(&self, _name: &str) -> Self::Value {
        Type::Bool
    }

    fn get_false(&self, _name: &str) -> Self::Value {
        Type::Bool
    }
}

pub type TypecheckerBuiltInValues = BuiltInValues<TypecheckerBuiltInValueProducer>;
