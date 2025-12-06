use crate::{
    data_structures::graph::DirectedGraph,
    error_reporting::Position,
    parser::{
        program::{Identifier, InfixOperator, Node, StringLiteral},
        type_::{FunctionType, NumericType, Type},
    },
    phase::built_in_values::BuiltInPathBuf,
    simplifier::simple_program::SimplePrefixOperator,
};
use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum},
    AddressSpace,
};
use std::fmt::{Debug, Display};
use ular_derive::{AnalyzerTyped, Node};

#[derive(Clone, Debug)]
pub(crate) enum AnalyzedType {
    Bool,
    Struct(usize),
    Function(AnalyzedFunctionType),
    Numeric(NumericType),
    Str,
    Unit,
}

impl AnalyzedType {
    fn debug_display_underlying<'a, A: FnMut(usize) -> &'a AnalyzedStructDefinition + Copy>(
        &self,
        mut struct_definition: A,
    ) -> Type {
        match self {
            Self::Bool => Type::Bool,
            Self::Struct(i) => Type::Identifier(struct_definition(*i).name.value.clone()),
            Self::Function(function_type) => Type::Function(FunctionType {
                parameters: function_type
                    .parameters
                    .iter()
                    .map(|parameter_type| {
                        parameter_type.debug_display_underlying(struct_definition)
                    })
                    .collect(),

                return_type: Box::new(
                    function_type
                        .return_type
                        .debug_display_underlying(struct_definition),
                ),
            }),

            Self::Numeric(numeric_type) => Type::Numeric(*numeric_type),
            Self::Str => Type::Str,
            Self::Unit => Type::Unit,
        }
    }

    pub(crate) fn display<'a, A: FnMut(usize) -> &'a AnalyzedStructDefinition + Copy>(
        &self,
        struct_definition: A,
    ) -> impl Display {
        self.debug_display_underlying(struct_definition)
    }

    pub(crate) fn inkwell_type<'a>(&self, context: &'a Context) -> Option<BasicTypeEnum<'a>> {
        match self {
            Self::Bool => Some(BasicTypeEnum::IntType(context.i8_type())),
            Self::Struct(_) | Self::Str => Some(BasicTypeEnum::PointerType(
                // https://llvm.org/docs/Statepoints.html#rewritestatepointsforgc
                context.ptr_type(AddressSpace::from(1)),
            )),

            Self::Function(_) => Some(BasicTypeEnum::PointerType(
                context.ptr_type(AddressSpace::default()),
            )),

            Self::Numeric(type_) => Some(BasicTypeEnum::IntType(type_.inkwell_type(context))),
            Self::Unit => None,
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct AnalyzedFunctionType {
    pub(crate) parameters: Vec<AnalyzedType>,
    pub(crate) return_type: Box<AnalyzedType>,
}

impl AnalyzedFunctionType {
    pub(crate) fn inkwell_type<'a>(
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

pub(crate) trait AnalyzerTyped {
    fn get_type(&self) -> AnalyzedType;
}

impl AnalyzerTyped for StringLiteral {
    fn get_type(&self) -> AnalyzedType {
        AnalyzedType::Str
    }
}

#[derive(Debug, Node)]
pub(crate) struct AnalyzedProgram {
    pub(crate) string_literals: Vec<String>,
    pub(crate) structs: Vec<AnalyzedStructDefinition>,
    pub(crate) functions: Vec<AnalyzedFunctionDefinition>,
    pub(crate) expression_graph: DirectedGraph<AnalyzedExpression>,
    pub(crate) position: Position,
}

#[derive(Debug, Node)]
pub(crate) struct AnalyzedStructDefinition {
    pub(crate) name: Identifier,
    pub(crate) fields: Vec<AnalyzedStructDefinitionField>,
    pub(crate) methods: Vec<AnalyzedFunctionDefinition>,
    pub(crate) position: Position,
}

#[derive(Debug, Node)]
pub(crate) struct AnalyzedStructDefinitionField {
    pub(crate) name: Identifier,
    pub(crate) type_: AnalyzedType,
    pub(crate) position: Position,
}

#[derive(Debug, Node)]
pub(crate) struct AnalyzedFunctionDefinition {
    pub(crate) name: Identifier,
    pub(crate) parameters: Vec<AnalyzedParameter>,
    pub(crate) body: AnalyzedBlock,
    pub(crate) type_: AnalyzedFunctionType,
    pub(crate) position: Position,
}

#[derive(Debug, Node)]
pub(crate) struct AnalyzedParameter {
    #[allow(dead_code)]
    pub(crate) name: String,
    pub(crate) type_: AnalyzedType,
    pub(crate) position: Position,
}

#[derive(AnalyzerTyped, Debug, Node)]
pub(crate) enum AnalyzedExpression {
    If(AnalyzedIf),
    InfixOperation(AnalyzedInfixOperation),
    Select(AnalyzedSelect),
    Call(AnalyzedCall),
    StructApplication(AnalyzedStructApplication),
    PrefixOperation(AnalyzedPrefixOperation),
    String(AnalyzedStringLiteral),
}

#[derive(AnalyzerTyped, Debug, Node)]
pub(crate) struct AnalyzedIf {
    pub(crate) condition: AnalyzedExpressionRef,
    pub(crate) then_block: AnalyzedBlock,
    pub(crate) else_block: AnalyzedBlock,
    pub(crate) type_: AnalyzedType,
    pub(crate) position: Position,
}

#[derive(Debug, Node)]
pub(crate) struct AnalyzedBlock {
    pub(crate) expression_graph: DirectedGraph<AnalyzedExpression>,
    pub(crate) result: Option<AnalyzedExpressionRef>,
    pub(crate) position: Position,
}

#[derive(AnalyzerTyped, Debug, Node)]
pub(crate) struct AnalyzedInfixOperation {
    pub(crate) left: AnalyzedExpressionRef,
    pub(crate) operator: InfixOperator,
    pub(crate) right: AnalyzedExpressionRef,
    pub(crate) type_: AnalyzedType,
    pub(crate) position: Position,
}

#[derive(AnalyzerTyped, Debug, Node)]
pub(crate) struct AnalyzedPrefixOperation {
    pub(crate) operator: SimplePrefixOperator,
    pub(crate) expression: AnalyzedExpressionRef,
    pub(crate) type_: AnalyzedType,
    pub(crate) position: Position,
}

#[derive(AnalyzerTyped, Debug, Node)]
pub(crate) struct AnalyzedSelect {
    pub(crate) left_hand_side: AnalyzedExpressionRef,
    pub(crate) field_index: usize,
    pub(crate) type_: AnalyzedType,
    pub(crate) position: Position,
}

#[derive(AnalyzerTyped, Debug, Node)]
pub(crate) struct AnalyzedCall {
    pub(crate) function: AnalyzedExpressionRef,
    pub(crate) arguments: Vec<AnalyzedExpressionRef>,
    pub(crate) type_: AnalyzedType,
    pub(crate) position: Position,
}

#[derive(AnalyzerTyped, Debug, Node)]
pub(crate) struct AnalyzedStructApplication {
    pub(crate) struct_index: usize,
    pub(crate) fields: Vec<AnalyzedStructApplicationField>,
    pub(crate) type_: AnalyzedType,
    pub(crate) position: Position,
}

#[derive(Debug, Node)]
pub(crate) struct AnalyzedStructApplicationField {
    pub(crate) name: String,
    pub(crate) value: AnalyzedExpressionRef,
    pub(crate) position: Position,
}

#[derive(AnalyzerTyped, Debug, Clone, Node)]
pub(crate) enum AnalyzedExpressionRef {
    BuiltIn {
        path: BuiltInPathBuf,
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

    StructMethod {
        struct_index: usize,
        method_index: usize,
        type_: AnalyzedType,
        position: Position,
    },

    Unit(AnalyzedUnit),
}

#[derive(Clone, Debug, Node)]
pub(crate) struct AnalyzedNumber {
    pub(crate) value: i128,
    pub(crate) type_: NumericType,
    pub(crate) position: Position,
}

impl AnalyzerTyped for AnalyzedNumber {
    fn get_type(&self) -> AnalyzedType {
        AnalyzedType::Numeric(self.type_)
    }
}

#[derive(Clone, Debug, Node)]
pub(crate) struct AnalyzedStringLiteral {
    pub(crate) index: usize,
    pub(crate) position: Position,
}

impl AnalyzerTyped for AnalyzedStringLiteral {
    fn get_type(&self) -> AnalyzedType {
        AnalyzedType::Str
    }
}

#[derive(Clone, Debug, Node)]
pub(crate) struct AnalyzedUnit {
    pub(crate) position: Position,
}

impl AnalyzerTyped for AnalyzedUnit {
    fn get_type(&self) -> AnalyzedType {
        AnalyzedType::Unit
    }
}

impl AnalyzedExpressionRef {
    pub(super) fn for_expression<A: AnalyzerTyped + Node>(i: usize, expression: &A) -> Self {
        Self::Expression {
            index: i,
            type_: expression.get_type(),
            position: expression.get_position(),
        }
    }

    pub(super) fn with_position(&self, position: Position) -> Self {
        match self {
            Self::BuiltIn { path, type_, .. } => Self::BuiltIn {
                path: path.clone(),
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

            Self::StructMethod {
                struct_index,
                method_index,
                type_,
                ..
            } => Self::StructMethod {
                struct_index: *struct_index,
                method_index: *method_index,
                type_: type_.clone(),
                position,
            },

            Self::Unit(_) => Self::Unit(AnalyzedUnit { position }),
        }
    }
}
