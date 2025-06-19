use crate::{
    error_reporting::{CompilationError, CompilationErrorMessage, InternalError, Position},
    jit_compiler::scope::LocalName,
    parser::type_::Type,
};

use either::Either::{Left, Right};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    types::FunctionType,
    values::{
        BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallSiteValue, FunctionValue, IntValue,
        PointerValue,
    },
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
    pub fn build_call(
        &self,
        context: &'a Context,
        builder: &Builder<'a>,
        arguments: &[UlarValue<'a>],
        name: LocalName,
        type_: &Type,
        position: Position,
    ) -> Result<UlarValue<'a>, CompilationError> {
        let mut non_unit_arguments = Vec::with_capacity(arguments.len());

        for argument in arguments {
            non_unit_arguments.push(BasicValueEnum::try_from(*argument)?.into());
        }

        let result = match self {
            Self::DirectReference(function) => builder
                .build_call(*function, &non_unit_arguments, &name.to_string())
                .unwrap(),

            Self::IndirectReference { pointer, type_ } => builder
                .build_indirect_call(*type_, *pointer, &non_unit_arguments, &name.to_string())
                .unwrap(),
        };

        UlarValue::from_call_site_value(context, type_, result, position)
    }

    pub fn get_pointer_value(&self) -> PointerValue<'a> {
        match self {
            Self::DirectReference(function) => function.as_global_value().as_pointer_value(),
            Self::IndirectReference { pointer, .. } => *pointer,
        }
    }

    pub fn get_type(&self) -> FunctionType<'a> {
        match self {
            Self::DirectReference(function) => function.get_type(),
            Self::IndirectReference { type_, .. } => *type_,
        }
    }
}

impl<'a> TryFrom<UlarFunction<'a>> for FunctionValue<'a> {
    type Error = CompilationError;

    fn try_from(function: UlarFunction<'a>) -> Result<Self, Self::Error> {
        match function {
            UlarFunction::DirectReference(result) => Ok(result),
            UlarFunction::IndirectReference { .. } => Err(CompilationError {
                message: CompilationErrorMessage::InternalError(
                    InternalError::JitCompilerTypeMismatch {
                        expected_type: String::from("FunctionValue"),
                        actual_value: format!("{:?}", function),
                    },
                ),
                position: None,
            }),
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
        position: Position,
    ) -> Result<UlarValue<'a>, CompilationError> {
        match type_.inkwell_type(context) {
            Some(phi_type) => {
                let phi = builder.build_phi(phi_type, &name.to_string()).unwrap();

                for (value, block) in incoming {
                    let basic_value: BasicValueEnum<'a> = (*value).try_into()?;

                    phi.add_incoming(&[(&basic_value, *block)]);
                }

                Self::from_basic_value(context, type_, phi.as_basic_value(), position)
            }

            None => Ok(UlarValue::Unit),
        }
    }

    pub fn from_basic_value<A: BasicValue<'a>>(
        context: &'a Context,
        type_: &Type,
        value: A,
        value_position: Position,
    ) -> Result<Self, CompilationError> {
        let basic_value_enum = value.as_basic_value_enum();

        match type_ {
            Type::Bool | Type::Numeric(_) => Ok(UlarValue::Int(basic_value_enum.into_int_value())),
            Type::Function(function_type) => Ok(Self::Function(UlarFunction::IndirectReference {
                pointer: basic_value_enum.into_pointer_value(),
                type_: function_type
                    .inkwell_type(context)
                    .ok_or(CompilationError {
                        message: CompilationErrorMessage::UnitPassedAsValue,
                        position: Some(value_position),
                    })?,
            })),

            Type::Unit => Err(CompilationError {
                message: CompilationErrorMessage::UnitPassedAsValue,
                position: Some(value_position),
            }),
        }
    }

    pub fn from_call_site_value(
        context: &'a Context,
        type_: &Type,
        value: CallSiteValue<'a>,
        value_position: Position,
    ) -> Result<Self, CompilationError> {
        match value.try_as_basic_value() {
            Left(basic_value) => {
                Self::from_basic_value(context, type_, basic_value, value_position)
            }

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
            UlarValue::Unit => Err(CompilationError {
                message: CompilationErrorMessage::InternalError(
                    InternalError::JitCompilerTypeMismatch {
                        expected_type: String::from("BasicValue"),
                        actual_value: format!("{:?}", value),
                    },
                ),

                position: None,
            }),
        }
    }
}

impl<'a> TryFrom<UlarValue<'a>> for BasicMetadataValueEnum<'a> {
    type Error = CompilationError;

    fn try_from(value: UlarValue<'a>) -> Result<Self, Self::Error> {
        Ok(BasicValueEnum::try_from(value)?.into())
    }
}

impl<'a> TryFrom<UlarValue<'a>> for IntValue<'a> {
    type Error = CompilationError;

    fn try_from(value: UlarValue<'a>) -> Result<Self, Self::Error> {
        match value {
            UlarValue::Int(int_value) => Ok(int_value),
            _ => Err(CompilationError {
                message: CompilationErrorMessage::InternalError(
                    InternalError::JitCompilerTypeMismatch {
                        expected_type: String::from("IntValue"),
                        actual_value: format!("{:?}", value),
                    },
                ),

                position: None,
            }),
        }
    }
}

impl<'a> TryFrom<UlarValue<'a>> for UlarFunction<'a> {
    type Error = CompilationError;

    fn try_from(value: UlarValue<'a>) -> Result<Self, Self::Error> {
        match value {
            UlarValue::Function(function) => Ok(function),
            _ => Err(CompilationError {
                message: CompilationErrorMessage::InternalError(
                    InternalError::JitCompilerTypeMismatch {
                        expected_type: String::from("PointerValue"),
                        actual_value: format!("{:?}", value),
                    },
                ),

                position: None,
            }),
        }
    }
}
