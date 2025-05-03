use crate::{
    error::{CompilationError, InternalError},
    jit_compiler::scope::LocalName,
    parser::type_::Type,
};

use either::Either::{Left, Right};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    types::FunctionType,
    values::{BasicValue, BasicValueEnum, CallSiteValue, FunctionValue, IntValue, PointerValue},
};

#[derive(Clone, Copy, Debug)]
pub enum UlarFunction<'a> {
    DirectReference(FunctionValue<'a>),
    IndirectReference {
        pointer: PointerValue<'a>,
        type_: FunctionType<'a>,
    },
}

impl<'a> UlarFunction<'a> {
    pub fn get_pointer_value(&self) -> PointerValue<'a> {
        match self {
            Self::DirectReference(function) => function.as_global_value().as_pointer_value(),
            Self::IndirectReference { pointer, .. } => *pointer,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum UlarValue<'a> {
    Function(UlarFunction<'a>),
    Int(IntValue<'a>),
    Unit,
}

impl<'a> UlarValue<'a> {
    pub fn build_phi(
        context: &'a Context,
        builder: &Builder<'a>,
        type_: &Type,
        incoming: &[(UlarValue<'a>, BasicBlock<'a>)],
        name: LocalName,
    ) -> Result<UlarValue<'a>, CompilationError> {
        match type_.inkwell_type(context) {
            Some(phi_type) => {
                let phi = builder.build_phi(phi_type, &name.to_string()).unwrap();

                for (value, block) in incoming {
                    let basic_value: BasicValueEnum<'a> = (*value).try_into()?;

                    phi.add_incoming(&[(&basic_value, *block)]);
                }

                Self::from_basic_value(context, type_, phi.as_basic_value())
            }

            None => Ok(UlarValue::Unit),
        }
    }

    pub fn from_basic_value<A: BasicValue<'a>>(
        context: &'a Context,
        type_: &Type,
        value: A,
    ) -> Result<Self, CompilationError> {
        let basic_value_enum = value.as_basic_value_enum();

        match type_ {
            Type::Bool | Type::Numeric(_) => Ok(UlarValue::Int(basic_value_enum.into_int_value())),
            Type::Function(function_type) => Ok(Self::Function(UlarFunction::IndirectReference {
                pointer: basic_value_enum.into_pointer_value(),
                type_: function_type.inkwell_type(context)?,
            })),

            Type::Unit => Err(CompilationError::UnitPassedAsValue),
        }
    }

    pub fn from_call_site_value(
        context: &'a Context,
        type_: &Type,
        call_site_value: CallSiteValue<'a>,
    ) -> Result<Self, CompilationError> {
        match call_site_value.try_as_basic_value() {
            Left(basic_value) => Self::from_basic_value(context, type_, basic_value),
            Right(_) => Ok(Self::Unit),
        }
    }
}

impl<'a> From<IntValue<'a>> for UlarValue<'a> {
    fn from(value: IntValue<'a>) -> Self {
        Self::Int(value)
    }
}

impl<'a> TryFrom<UlarValue<'a>> for BasicValueEnum<'a> {
    type Error = CompilationError;

    fn try_from(value: UlarValue<'a>) -> Result<Self, Self::Error> {
        match value {
            UlarValue::Function(function) => {
                Ok(BasicValueEnum::PointerValue(function.get_pointer_value()))
            }

            UlarValue::Int(int_value) => Ok(BasicValueEnum::IntValue(int_value)),
            UlarValue::Unit => Err(CompilationError::InternalError(
                InternalError::JitCompilerTypeMismatch {
                    expected_type: String::from("BasicValue"),
                    actual_value: format!("{:?}", value),
                },
            )),
        }
    }
}

impl<'a> TryFrom<UlarValue<'a>> for IntValue<'a> {
    type Error = CompilationError;

    fn try_from(value: UlarValue<'a>) -> Result<Self, Self::Error> {
        match value {
            UlarValue::Int(int_value) => Ok(int_value),
            _ => Err(CompilationError::InternalError(
                InternalError::JitCompilerTypeMismatch {
                    expected_type: String::from("IntValue"),
                    actual_value: format!("{:?}", value),
                },
            )),
        }
    }
}

impl<'a> TryFrom<UlarValue<'a>> for UlarFunction<'a> {
    type Error = CompilationError;

    fn try_from(value: UlarValue<'a>) -> Result<Self, Self::Error> {
        match value {
            UlarValue::Function(function) => Ok(function),
            _ => Err(CompilationError::InternalError(
                InternalError::JitCompilerTypeMismatch {
                    expected_type: String::from("PointerValue"),
                    actual_value: format!("{:?}", value),
                },
            )),
        }
    }
}
