use crate::{
    error_reporting::Position,
    parser::{
        program::{Identifier, InfixOperator, Node, Number, Parameter, StructDefinition, Unit},
        type_::Type,
    },
};
use ular_derive::Node;

#[derive(Debug, Node)]
pub struct SimpleProgram {
    pub statements: Vec<SimpleStatement>,
    pub position: Position,
}

#[derive(Debug, Node)]
pub enum SimpleStatement {
    StructDefinition(StructDefinition),
    VariableDefinition(SimpleVariableDefinition),
    FunctionDefinition(SimpleFunctionDefinition),
    Expression(SimpleExpression),
    NoOp { position: Position },
}

#[derive(Debug, Node)]
pub struct SimpleVariableDefinition {
    pub name: Identifier,
    pub value: SimpleExpression,
    pub position: Position,
}

#[derive(Debug, Node)]
pub struct SimpleFunctionDefinition {
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub return_type: Type,
    pub body: SimpleBlock,
    pub position: Position,
}

#[derive(Debug, Node)]
pub struct SimpleBlock {
    pub statements: Vec<SimpleStatement>,
    pub result: Option<Box<SimpleExpression>>,
    pub position: Position,
}

#[derive(Debug, Node)]
pub enum SimpleExpression {
    If(SimpleIf),
    InfixOperation(SimpleInfixOperation),
    Call(SimpleCall),
    StructApplication(SimpleStructApplication),
    Identifier(Identifier),
    Number(Number),
    PrefixOperation(SimplePrefixOperation),
    SequentialBlock(SimpleBlock),
    Unit(Unit),
}

#[derive(Debug, Node)]
pub struct SimpleIf {
    pub condition: Box<SimpleExpression>,
    pub then_block: SimpleBlock,
    pub else_block: SimpleBlock,
    pub position: Position,
}

#[derive(Debug, Node)]
pub struct SimpleInfixOperation {
    pub left: Box<SimpleExpression>,
    pub operator: InfixOperator,
    pub right: Box<SimpleExpression>,
    pub position: Position,
}

#[derive(Debug, Node)]
pub struct SimplePrefixOperation {
    pub operator: SimplePrefixOperator,
    pub expression: Box<SimpleExpression>,
    pub position: Position,
}

#[derive(Clone, Copy, Debug)]
pub enum SimplePrefixOperator {
    Not,
}

#[derive(Debug, Node)]
pub struct SimpleCall {
    pub function: Box<SimpleExpression>,
    pub arguments: Vec<SimpleExpression>,
    pub position: Position,
}

#[derive(Debug, Node)]
pub struct SimpleStructApplication {
    pub name: Identifier,
    pub fields: Vec<SimpleStructApplicationField>,
    pub position: Position,
}

#[derive(Debug)]
pub struct SimpleStructApplicationField {
    pub name: Identifier,
    pub value: SimpleExpression,
}
