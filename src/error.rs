use std::fmt::Display;

pub enum CompilationError {
    TypeMismatch {
        expected: String,
        actual: String,
    },

    VariableAlreadyDefined(String),
    UnknownValue(String),
    ValueNotCallable(String),
    IncorrectNumberOfArguments {
        expected: usize,
        actual: usize,
    },

    JitCompilerTypeMismatch {
        expected_type: String,
        actual_value: String,
    },

    UnitPassedAsValue,
}

impl Display for CompilationError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TypeMismatch { expected, actual } => write!(
                formatter,
                "Expected a value of type {}, but got one of type {}.",
                expected, actual
            ),

            Self::VariableAlreadyDefined(name) => write!(
                formatter,
                "A variable with name {} is already defined.",
                name
            ),

            Self::UnknownValue(name) => write!(formatter, "Unknown value: {}", name),
            Self::ValueNotCallable(name) => write!(formatter, "Value not callable: {}", name),
            Self::IncorrectNumberOfArguments { expected, actual } => write!(
                formatter,
                "Expected {} arguments, but got {}.",
                expected, actual
            ),

            Self::JitCompilerTypeMismatch {
                expected_type,
                actual_value,
            } => write!(
                formatter,
                "Internal error: The JIT compiler expected a value of type {}, but got {}",
                expected_type, actual_value
            ),

            Self::UnitPassedAsValue => write!(
                formatter,
                "Unit was passed as a value. Currently, unit isn't a value"
            ),
        }
    }
}
