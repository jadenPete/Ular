use crate::parser::{
    program::{Identifier, InfixOperator, Number, Parameter},
    type_::Type,
};

#[derive(Debug)]
pub struct SimpleProgram {
    pub statements: Vec<SimpleStatement>,
}

#[derive(Debug)]
pub enum SimpleStatement {
    VariableDefinition(SimpleVariableDefinition),
    FunctionDefinition(SimpleFunctionDefinition),
    Expression(SimpleExpression),
    NoOp,
}

#[derive(Debug)]
pub struct SimpleVariableDefinition {
    pub name: Identifier,
    pub value: SimpleExpression,
}

#[derive(Debug)]
pub struct SimpleFunctionDefinition {
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub return_type: Type,
    pub body: SimpleBlock,
}

#[derive(Debug)]
pub struct SimpleBlock {
    pub statements: Vec<SimpleStatement>,
    pub result: Option<Box<SimpleExpression>>,
}

#[derive(Debug)]
pub enum SimpleExpression {
    If(SimpleIf),
    InfixOperation(SimpleInfixOperation),
    Call(SimpleCall),
    Identifier(Identifier),
    Number(Number),
    PrefixOperation(SimplePrefixOperation),
}

#[derive(Debug)]
pub struct SimpleIf {
    pub condition: Box<SimpleExpression>,
    pub then_block: SimpleBlock,
    pub else_block: SimpleBlock,
}

#[derive(Debug)]
pub struct SimpleInfixOperation {
    pub left: Box<SimpleExpression>,
    pub operator: InfixOperator,
    pub right: Box<SimpleExpression>,
}

#[derive(Debug)]
pub struct SimplePrefixOperation {
    pub operator: SimplePrefixOperator,
    pub expression: Box<SimpleExpression>,
}

#[derive(Clone, Copy, Debug)]
pub enum SimplePrefixOperator {
    Not,
}

#[derive(Debug)]
pub struct SimpleCall {
    pub function: Box<SimpleExpression>,
    pub arguments: Vec<SimpleExpression>,
}
