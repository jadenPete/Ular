use crate::{
    data_structures::graph::DirectedGraph,
    error_reporting::Position,
    parser::{
        program::{Identifier, InfixOperator, Node},
        type_::{FunctionType, NumericType, Type},
    },
    simplifier::simple_program::SimplePrefixOperator,
};
use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum, StructType},
    AddressSpace,
};
use std::fmt::{Debug, Display};
use ular_derive::{AnalyzerTyped, Node};

#[derive(Clone, Debug)]
pub enum AnalyzedType {
    Bool,
    Struct(usize),
    Function(AnalyzedFunctionType),
    Numeric(NumericType),
    Unit,
}

impl AnalyzedType {
    pub fn debug(&self, struct_types: &[(&AnalyzedStructDefinition, StructType)]) -> impl Debug {
        self.debug_display_underlying(struct_types)
    }

    fn debug_display_underlying(
        &self,
        struct_types: &[(&AnalyzedStructDefinition, StructType)],
    ) -> Type {
        match self {
            Self::Bool => Type::Bool,
            Self::Struct(i) => Type::Identifier(struct_types[*i].0.name.value.clone()),
            Self::Function(function_type) => Type::Function(FunctionType {
                parameters: function_type
                    .parameters
                    .iter()
                    .map(|parameter_type| parameter_type.debug_display_underlying(struct_types))
                    .collect(),

                return_type: Box::new(
                    function_type
                        .return_type
                        .debug_display_underlying(struct_types),
                ),
            }),

            Self::Numeric(numeric_type) => Type::Numeric(*numeric_type),
            Self::Unit => Type::Unit,
        }
    }

    pub fn display(
        &self,
        struct_types: &[(&AnalyzedStructDefinition, StructType)],
    ) -> impl Display {
        self.debug_display_underlying(struct_types)
    }

    pub fn inkwell_type<'a>(&self, context: &'a Context) -> Option<BasicTypeEnum<'a>> {
        match self {
            Self::Bool => Some(BasicTypeEnum::IntType(context.i8_type())),
            Self::Struct(_) | Self::Function(_) => Some(BasicTypeEnum::PointerType(
                context.ptr_type(AddressSpace::default()),
            )),

            Self::Numeric(type_) => Some(BasicTypeEnum::IntType(type_.inkwell_type(context))),
            Self::Unit => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct AnalyzedFunctionType {
    pub parameters: Vec<AnalyzedType>,
    pub return_type: Box<AnalyzedType>,
}

impl AnalyzedFunctionType {
    pub fn inkwell_type<'a>(
        &self,
        context: &'a Context,
    ) -> Option<inkwell::types::FunctionType<'a>> {
        let mut parameter_types = Vec::with_capacity(self.parameters.len() + 1);

        // Add an extra parameter for the worker pointer
        parameter_types.push(context.ptr_type(AddressSpace::default()).into());

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

pub trait AnalyzerTyped {
    fn get_type(&self) -> AnalyzedType;
}

#[derive(Debug, Node)]
pub struct AnalyzedProgram {
    pub structs: Vec<AnalyzedStructDefinition>,
    pub functions: Vec<AnalyzedFunctionDefinition>,
    pub expression_graph: DirectedGraph<AnalyzedExpression>,
    pub position: Position,
}

#[derive(Debug, Node)]
pub struct AnalyzedStructDefinition {
    pub name: Identifier,
    pub fields: Vec<AnalyzedStructDefinitionField>,
    pub position: Position,
}

#[derive(Debug)]
pub struct AnalyzedStructDefinitionField {
    pub name: Identifier,
    pub type_: AnalyzedType,
}

#[derive(Debug, Node)]
pub struct AnalyzedFunctionDefinition {
    pub name: Identifier,
    pub parameters: Vec<AnalyzedParameter>,
    pub body: AnalyzedBlock,
    pub type_: AnalyzedFunctionType,
    pub position: Position,
}

#[derive(Debug)]
pub struct AnalyzedParameter {
    #[allow(dead_code)]
    pub name: String,
    pub type_: AnalyzedType,
    pub position: Position,
}

#[derive(AnalyzerTyped, Debug, Node)]
pub enum AnalyzedExpression {
    If(AnalyzedIf),
    InfixOperation(AnalyzedInfixOperation),
    Select(AnalyzedSelect),
    Call(AnalyzedCall),
    StructApplication(AnalyzedStructApplication),
    PrefixOperation(AnalyzedPrefixOperation),
}

#[derive(AnalyzerTyped, Debug, Node)]
pub struct AnalyzedIf {
    pub condition: AnalyzedExpressionRef,
    pub then_block: AnalyzedBlock,
    pub else_block: AnalyzedBlock,
    pub type_: AnalyzedType,
    pub position: Position,
}

#[derive(Debug)]
pub struct AnalyzedBlock {
    pub expression_graph: DirectedGraph<AnalyzedExpression>,
    pub result: Option<AnalyzedExpressionRef>,
}

#[derive(AnalyzerTyped, Debug, Node)]
pub struct AnalyzedInfixOperation {
    pub left: AnalyzedExpressionRef,
    pub operator: InfixOperator,
    pub right: AnalyzedExpressionRef,
    pub type_: AnalyzedType,
    pub position: Position,
}

#[derive(AnalyzerTyped, Debug, Node)]
pub struct AnalyzedPrefixOperation {
    pub operator: SimplePrefixOperator,
    pub expression: AnalyzedExpressionRef,
    pub type_: AnalyzedType,
    pub position: Position,
}

#[derive(AnalyzerTyped, Debug, Node)]
pub struct AnalyzedSelect {
    pub left_hand_side: AnalyzedExpressionRef,
    pub field_index: usize,
    pub type_: AnalyzedType,
    pub position: Position,
}

#[derive(AnalyzerTyped, Debug, Node)]
pub struct AnalyzedCall {
    pub function: AnalyzedExpressionRef,
    pub arguments: Vec<AnalyzedExpressionRef>,
    pub type_: AnalyzedType,
    pub position: Position,
}

#[derive(AnalyzerTyped, Debug, Node)]
pub struct AnalyzedStructApplication {
    pub struct_index: usize,
    pub fields: Vec<AnalyzedStructApplicationField>,
    pub type_: AnalyzedType,
    pub position: Position,
}

#[derive(Debug)]
pub struct AnalyzedStructApplicationField {
    pub name: String,
    pub value: AnalyzedExpressionRef,
}

#[derive(AnalyzerTyped, Debug, Clone, Node)]
pub enum AnalyzedExpressionRef {
    BuiltIn {
        name: String,
        type_: AnalyzedType,
        position: Position,
    },

    Expression {
        index: usize,
        type_: AnalyzedType,
        position: Position,
    },

    Function {
        index: usize,
        type_: AnalyzedType,
        position: Position,
    },

    Number(AnalyzedNumber),
    Parameter {
        index: usize,
        type_: AnalyzedType,
        position: Position,
    },

    Unit(AnalyzedUnit),
}

#[derive(Clone, Debug, Node)]
pub struct AnalyzedNumber {
    pub value: i128,
    pub type_: NumericType,
    pub position: Position,
}

impl AnalyzerTyped for AnalyzedNumber {
    fn get_type(&self) -> AnalyzedType {
        AnalyzedType::Numeric(self.type_)
    }
}

#[derive(Clone, Debug, Node)]
pub struct AnalyzedUnit {
    pub position: Position,
}

impl AnalyzerTyped for AnalyzedUnit {
    fn get_type(&self) -> AnalyzedType {
        AnalyzedType::Unit
    }
}

impl AnalyzedExpressionRef {
    pub fn for_expression<A: AnalyzerTyped + Node>(i: usize, expression: &A) -> Self {
        Self::Expression {
            index: i,
            type_: expression.get_type(),
            position: expression.get_position(),
        }
    }

    pub fn with_position(&self, position: Position) -> Self {
        match self {
            Self::BuiltIn { name, type_, .. } => Self::BuiltIn {
                name: name.clone(),
                type_: type_.clone(),
                position,
            },

            Self::Expression { index, type_, .. } => Self::Expression {
                index: *index,
                type_: type_.clone(),
                position,
            },

            Self::Function { index, type_, .. } => Self::Function {
                index: *index,
                type_: type_.clone(),
                position,
            },

            Self::Number(number) => Self::Number(AnalyzedNumber {
                value: number.value,
                type_: number.type_,
                position,
            }),

            Self::Parameter { index, type_, .. } => Self::Parameter {
                index: *index,
                type_: type_.clone(),
                position,
            },

            Self::Unit(_) => Self::Unit(AnalyzedUnit { position }),
        }
    }
}
