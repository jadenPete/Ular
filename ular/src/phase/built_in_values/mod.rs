use crate::parser::type_::NumericType;
use dashmap::Equivalent;
use std::hash::Hash;

#[derive(Eq, Hash, PartialEq)]
pub(crate) enum BuiltInPath<'a> {
    Identifier(&'a str),
    Method(&'a str, &'a str),
}

impl Equivalent<BuiltInPathBuf> for BuiltInPath<'_> {
    fn equivalent(&self, key: &BuiltInPathBuf) -> bool {
        match (self, key) {
            (BuiltInPath::Identifier(value1), BuiltInPathBuf::Identifier(value2)) => {
                value1 == value2
            }

            (
                BuiltInPath::Method(left_hand_side1, right_hand_side1),
                BuiltInPathBuf::Method(left_hand_side2, right_hand_side2),
            ) => left_hand_side1 == left_hand_side2 && right_hand_side1 == right_hand_side2,

            _ => false,
        }
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub(crate) enum BuiltInPathBuf {
    Identifier(String),
    Method(String, String),
}

pub(crate) trait BuiltInValueProducer {
    type Value;

    fn get_println_bool(&self, name: &str) -> Self::Value;
    fn get_println_numeric(&self, name: &str, numeric_type: NumericType) -> Self::Value;
    fn get_println_str(&self, name: &str) -> Self::Value;
    fn get_true(&self, name: &str) -> Self::Value;
    fn get_false(&self, name: &str) -> Self::Value;
}

pub(crate) struct BuiltInValues<A: BuiltInValueProducer> {
    values_by_path: hashbrown::HashMap<BuiltInPathBuf, A::Value>,
}

impl<A: BuiltInValueProducer> BuiltInValues<A> {
    pub(crate) fn get<Path: Equivalent<BuiltInPathBuf> + Hash>(
        &self,
        path: &Path,
    ) -> Option<&A::Value> {
        self.values_by_path.get(path)
    }

    pub(crate) fn get_mut<Path: Equivalent<BuiltInPathBuf> + Hash>(
        &mut self,
        path: &Path,
    ) -> Option<&mut A::Value> {
        self.values_by_path.get_mut(path)
    }

    pub(crate) fn new<AdditionalValues: IntoIterator<Item = (String, A::Value)>>(
        built_in_values: A,
        additional_values: AdditionalValues,
    ) -> Self {
        let mut values_by_path = hashbrown::HashMap::new();

        values_by_path.insert(
            BuiltInPathBuf::Identifier("println_bool".to_owned()),
            built_in_values.get_println_bool("println_bool"),
        );

        for numeric_type in enum_iterator::all::<NumericType>() {
            let name = format!("println_{}", numeric_type);
            let value = built_in_values.get_println_numeric(&name, numeric_type);

            values_by_path.insert(BuiltInPathBuf::Identifier(name), value);
        }

        values_by_path.insert(
            BuiltInPathBuf::Identifier("println_str".to_owned()),
            built_in_values.get_println_str("println_str"),
        );

        values_by_path.insert(
            BuiltInPathBuf::Identifier("true".to_owned()),
            built_in_values.get_true("true"),
        );

        values_by_path.insert(
            BuiltInPathBuf::Identifier("false".to_owned()),
            built_in_values.get_false("false"),
        );

        for (name, value) in additional_values {
            values_by_path.insert(BuiltInPathBuf::Identifier(name), value);
        }

        Self { values_by_path }
    }
}
