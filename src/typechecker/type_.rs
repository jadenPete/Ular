use crate::error::CompilationError;
use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum},
    AddressSpace,
};
use std::fmt::{Debug, Display};

#[derive(Clone, PartialEq)]
pub enum Type {
    Boolean,
    Function(FunctionType),
    Number,
    Unit,
}

impl Type {
    pub fn inkwell_type<'a>(&self, context: &'a Context) -> Option<BasicTypeEnum<'a>> {
        match self {
            Self::Boolean => Some(BasicTypeEnum::IntType(context.i8_type())),
            Self::Function(_) => Some(BasicTypeEnum::PointerType(
                context.ptr_type(AddressSpace::default()),
            )),

            Self::Number => Some(BasicTypeEnum::IntType(context.i32_type())),
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
            Self::Boolean => write!(formatter, "bool"),
            Self::Function(function) => {
                let mut debug_tuple = formatter.debug_tuple("");

                for parameter in &function.parameters {
                    debug_tuple.field(&parameter);
                }

                debug_tuple.finish()?;

                write!(formatter, " -> {:?}", function.result)
            }

            Self::Number => write!(formatter, "number"),
            Self::Unit => write!(formatter, "unit"),
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct FunctionType {
    pub parameters: Vec<Type>,
    pub result: Box<Type>,
}

impl FunctionType {
    pub fn inkwell_type<'a>(
        &self,
        context: &'a Context,
    ) -> Result<inkwell::types::FunctionType<'a>, CompilationError> {
        let mut parameter_types = Vec::with_capacity(self.parameters.len());

        for parameter in &self.parameters {
            parameter_types.push(
                parameter
                    .inkwell_type(context)
                    .ok_or(CompilationError::UnitPassedAsValue)?
                    .into(),
            );
        }

        Ok(match self.result.inkwell_type(context) {
            Some(return_type) => return_type.fn_type(parameter_types.as_slice(), false),
            None => context
                .void_type()
                .fn_type(parameter_types.as_slice(), false),
        })
    }
}
