use crate::{
    data_structures::graph::DirectedGraph,
    error_reporting::Position,
    parser::{
        program::{Identifier, InfixOperator, Node},
        type_::{FunctionType, Type},
    },
    simplifier::simple_program::SimplePrefixOperator,
    typechecker::typed_program::{Typed, TypedIdentifier, TypedNumber, TypedUnit},
};
use ular_derive::{Node, Typed};

#[derive(Debug, Node)]
pub struct AnalyzedProgram {
    pub functions: Vec<AnalyzedFunctionDefinition>,
    pub expression_graph: DirectedGraph<AnalyzedExpression>,
    pub position: Position,
}

#[derive(Debug, Node)]
pub struct AnalyzedFunctionDefinition {
    pub name: Identifier,
    pub parameters: Vec<TypedIdentifier>,
    pub body: AnalyzedBlock,
    pub type_: FunctionType,
    pub position: Position,
}

impl Typed for AnalyzedFunctionDefinition {
    fn get_type(&self) -> Type {
        Type::Function(self.type_.clone())
    }
}

#[derive(Debug, Node, Typed)]
pub enum AnalyzedExpression {
    If(AnalyzedIf),
    InfixOperation(AnalyzedInfixOperation),
    Call(AnalyzedCall),
    PrefixOperation(AnalyzedPrefixOperation),
}

#[derive(Debug, Node, Typed)]
pub struct AnalyzedIf {
    pub condition: AnalyzedExpressionRef,
    pub then_block: AnalyzedBlock,
    pub else_block: AnalyzedBlock,
    pub type_: Type,
    pub position: Position,
}

#[derive(Debug)]
pub struct AnalyzedBlock {
    pub expression_graph: DirectedGraph<AnalyzedExpression>,
    pub result: Option<AnalyzedExpressionRef>,
}

#[derive(Debug, Node, Typed)]
pub struct AnalyzedInfixOperation {
    pub left: AnalyzedExpressionRef,
    pub operator: InfixOperator,
    pub right: AnalyzedExpressionRef,
    pub type_: Type,
    pub position: Position,
}

#[derive(Debug, Node, Typed)]
pub struct AnalyzedPrefixOperation {
    pub operator: SimplePrefixOperator,
    pub expression: AnalyzedExpressionRef,
    pub type_: Type,
    pub position: Position,
}

#[derive(Debug, Node, Typed)]
pub struct AnalyzedCall {
    pub function: AnalyzedExpressionRef,
    pub arguments: Vec<AnalyzedExpressionRef>,
    pub type_: Type,
    pub position: Position,
}

#[derive(Debug, Clone, Node, Typed)]
pub enum AnalyzedExpressionRef {
    BuiltIn {
        name: String,
        type_: Type,
        position: Position,
    },

    Expression {
        index: usize,
        type_: Type,
        position: Position,
    },

    Function {
        index: usize,
        type_: Type,
        position: Position,
    },

    Number(TypedNumber),
    Parameter {
        index: usize,
        type_: Type,
        position: Position,
    },

    Unit(TypedUnit),
}

impl AnalyzedExpressionRef {
    pub fn for_expression<A: Node + Typed>(i: usize, expression: &A) -> Self {
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

            Self::Number(number) => Self::Number(TypedNumber {
                value: number.value,
                type_: number.type_,
                position,
            }),

            Self::Parameter { index, type_, .. } => Self::Parameter {
                index: *index,
                type_: type_.clone(),
                position,
            },

            Self::Unit(_) => Self::Unit(TypedUnit { position }),
        }
    }
}
