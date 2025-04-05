use crate::parser::program::{Identifier, Number, Operator};

#[derive(Debug)]
pub struct SimpleProgram {
    pub statements: Vec<SimpleStatement>,
}

#[derive(Debug)]
pub enum SimpleStatement {
    VariableDefinition(SimpleVariableDefinition),
    Expression(SimpleExpression),
    NoOp,
}

#[derive(Debug)]
pub struct SimpleVariableDefinition {
    pub name: Identifier,
    pub value: SimpleExpression,
}

#[derive(Debug)]
pub enum SimpleExpression {
    If(SimpleIf),
    Infix(SimpleInfix),
    Call(SimpleCall),
    Identifier(Identifier),
    Number(Number),
}

#[derive(Debug)]
pub struct SimpleIf {
    pub condition: Box<SimpleExpression>,
    pub then_block: SimpleBlock,
    pub else_block: SimpleBlock,
}

#[derive(Debug)]
pub struct SimpleBlock {
    pub statements: Vec<SimpleStatement>,
    pub result: Option<Box<SimpleExpression>>,
}

#[derive(Debug)]
pub struct SimpleInfix {
    pub left: Box<SimpleExpression>,
    pub operator: Operator,
    pub right: Box<SimpleExpression>,
}

#[derive(Debug)]
pub struct SimpleCall {
    pub function: Identifier,
    pub arguments: Vec<SimpleExpression>,
}
