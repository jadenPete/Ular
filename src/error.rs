use std::fmt::{Display, Formatter};

pub enum CompilationError {
    InternalError(InternalError),
    TypeMismatch { expected: String, actual: String },
    VariableAlreadyDefined(String),
    UnknownValue(String),
    ValueNotCallable(String),
    IncorrectNumberOfArguments { expected: usize, actual: usize },
    UnitPassedAsValue,
    NumberOutOfRange { type_: String },
    ExpectedNumericType { actual_type: String },
    NestedFunctionsNotSupported,
}

impl Display for CompilationError {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InternalError(internal_error) => {
                write!(formatter, "Internal error: {}", internal_error)
            }

            Self::TypeMismatch { expected, actual } => write!(
                formatter,
                "Expected a value of type `{}`, but got one of type `{}`.",
                expected, actual
            ),

            Self::VariableAlreadyDefined(name) => write!(
                formatter,
                "A variable with name `{}` is already defined.",
                name
            ),

            Self::UnknownValue(name) => write!(formatter, "Unknown value: `{}`.", name),
            Self::ValueNotCallable(name) => write!(formatter, "`{}` isn't callable.", name),
            Self::IncorrectNumberOfArguments { expected, actual } => write!(
                formatter,
                "Expected {} arguments, but got {}.",
                expected, actual
            ),

            Self::UnitPassedAsValue => write!(
                formatter,
                "Unit was passed as a value. Currently, unit isn't a value"
            ),

            Self::NumberOutOfRange { type_ } => {
                write!(formatter, "Literal out of range for type `{}`.", type_)
            }

            Self::ExpectedNumericType { actual_type } => write!(
                formatter,
                "Expected a numerically typed value (`i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, or `u64`), but got one of type `{}`.",
                actual_type
            ),

            Self::NestedFunctionsNotSupported => {
                write!(formatter, "Nested functions aren't currently supported.")
            },
        }
    }
}

pub enum InternalError {
    JitCompilerTypeMismatch {
        expected_type: String,
        actual_value: String,
    },

    JitCompilerExpectedNumericType {
        actual_type: String,
    },

    JitCompilerFunctionNotHoisted,
}

impl Display for InternalError {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::JitCompilerTypeMismatch {
                expected_type,
                actual_value,
            } => write!(
                formatter,
                "The JIT compiler expected a value of type `{}`, but got `{}`",
                expected_type, actual_value
            ),

            Self::JitCompilerExpectedNumericType { actual_type } => write!(
                formatter,
                "The JIT compiler expected a numerically typed value, but got one of type `{}`.",
                actual_type
            ),

            Self::JitCompilerFunctionNotHoisted => write!(
                formatter,
                "The JIT compiler attempted to compile a function that wasn't hoisted beforehand."
            ),
        }
    }
}
