use enum_iterator::Sequence;
use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum, IntType},
    AddressSpace,
};

use std::fmt::{Debug, Display, Formatter};

#[derive(Clone, PartialEq)]
pub enum Type {
    Bool,
    Function(FunctionType),
    Numeric(NumericType),
    Unit,
}

impl Type {
    pub fn inkwell_type<'a>(&self, context: &'a Context) -> Option<BasicTypeEnum<'a>> {
        match self {
            Self::Bool => Some(BasicTypeEnum::IntType(context.i8_type())),
            Self::Function(_) => Some(BasicTypeEnum::PointerType(
                context.ptr_type(AddressSpace::default()),
            )),

            Self::Numeric(type_) => Some(BasicTypeEnum::IntType(type_.inkwell_type(context))),
            Self::Unit => None,
        }
    }
}

impl Display for Type {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{:?}", self)
    }
}

impl Debug for Type {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool => write!(formatter, "bool"),
            Self::Function(function) => {
                let mut debug_tuple = formatter.debug_tuple("");

                for parameter in &function.parameters {
                    debug_tuple.field(&parameter);
                }

                debug_tuple.finish()?;

                write!(formatter, " => {:?}", function.return_type)
            }

            Self::Numeric(numeric_type) => write!(formatter, "{}", numeric_type),
            Self::Unit => write!(formatter, "unit"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionType {
    pub parameters: Vec<Type>,
    pub return_type: Box<Type>,
}

impl FunctionType {
    pub fn inkwell_type<'a>(
        &self,
        context: &'a Context,
    ) -> Option<inkwell::types::FunctionType<'a>> {
        let mut parameter_types = Vec::with_capacity(self.parameters.len());

        for parameter in &self.parameters {
            parameter_types.push(parameter.inkwell_type(context)?.into());
        }

        Some(match self.return_type.inkwell_type(context) {
            Some(return_type) => return_type.fn_type(parameter_types.as_slice(), false),
            None => context
                .void_type()
                .fn_type(parameter_types.as_slice(), false),
        })
    }
}

#[derive(Clone, Copy, PartialEq, Sequence)]
pub enum NumericType {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
}

impl NumericType {
    pub fn maximum(self) -> i128 {
        match self {
            Self::I8 => i8::MAX.into(),
            Self::I16 => i16::MAX.into(),
            Self::I32 => i32::MAX.into(),
            Self::I64 => i32::MAX.into(),
            Self::U8 => u8::MAX.into(),
            Self::U16 => u16::MAX.into(),
            Self::U32 => u32::MAX.into(),
            Self::U64 => u64::MAX.into(),
        }
    }

    pub fn minimum(self) -> i128 {
        match self {
            Self::I8 => i8::MIN.into(),
            Self::I16 => i16::MIN.into(),
            Self::I32 => i32::MIN.into(),
            Self::I64 => i32::MIN.into(),
            Self::U8 => u8::MIN.into(),
            Self::U16 => u16::MIN.into(),
            Self::U32 => u32::MIN.into(),
            Self::U64 => u64::MIN.into(),
        }
    }

    pub fn is_signed(self) -> bool {
        match self {
            Self::I8 => true,
            Self::I16 => true,
            Self::I32 => true,
            Self::I64 => true,
            Self::U8 => false,
            Self::U16 => false,
            Self::U32 => false,
            Self::U64 => false,
        }
    }

    pub fn is_valid(self, value: i128) -> bool {
        match self {
            Self::I8 => i8::try_from(value).is_ok(),
            Self::I16 => i16::try_from(value).is_ok(),
            Self::I32 => i32::try_from(value).is_ok(),
            Self::I64 => i64::try_from(value).is_ok(),
            Self::U8 => u8::try_from(value).is_ok(),
            Self::U16 => u16::try_from(value).is_ok(),
            Self::U32 => u32::try_from(value).is_ok(),
            Self::U64 => u64::try_from(value).is_ok(),
        }
    }

    pub fn inkwell_type<'a>(self, context: &'a Context) -> IntType<'a> {
        match self {
            Self::I8 | Self::U8 => context.i8_type(),
            Self::I16 | Self::U16 => context.i16_type(),
            Self::I32 | Self::U32 => context.i32_type(),
            Self::I64 | Self::U64 => context.i64_type(),
        }
    }
}

impl Debug for NumericType {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::I8 => write!(formatter, "i8"),
            Self::I16 => write!(formatter, "i16"),
            Self::I32 => write!(formatter, "i32"),
            Self::I64 => write!(formatter, "i64"),
            Self::U8 => write!(formatter, "u8"),
            Self::U16 => write!(formatter, "u16"),
            Self::U32 => write!(formatter, "u32"),
            Self::U64 => write!(formatter, "u64"),
        }
    }
}

impl Display for NumericType {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{:?}", self)
    }
}
