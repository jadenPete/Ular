use crate::{
    error_reporting::Position,
    parser::{
        program::{Identifier, InfixOperator, Node, Number, Parameter},
        type_::Type,
    },
};

#[derive(Debug)]
pub struct SimpleProgram {
    pub statements: Vec<SimpleStatement>,
    pub position: Position,
}

impl Node for SimpleProgram {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}

#[derive(Debug)]
pub enum SimpleStatement {
    VariableDefinition(SimpleVariableDefinition),
    FunctionDefinition(SimpleFunctionDefinition),
    Expression(SimpleExpression),
    NoOp { position: Position },
}

impl Node for SimpleStatement {
    fn get_position(&self) -> Position {
        match self {
            Self::VariableDefinition(definition) => definition.get_position(),
            Self::FunctionDefinition(definition) => definition.get_position(),
            Self::Expression(expression) => expression.get_position(),
            Self::NoOp { position } => position.clone(),
        }
    }
}

#[derive(Debug)]
pub struct SimpleVariableDefinition {
    pub name: Identifier,
    pub value: SimpleExpression,
    pub position: Position,
}

impl Node for SimpleVariableDefinition {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}

#[derive(Debug)]
pub struct SimpleFunctionDefinition {
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub return_type: Type,
    pub body: SimpleBlock,
    pub position: Position,
}

impl Node for SimpleFunctionDefinition {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}

#[derive(Debug)]
pub struct SimpleBlock {
    pub statements: Vec<SimpleStatement>,
    pub result: Option<Box<SimpleExpression>>,
    pub position: Position,
}

impl Node for SimpleBlock {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
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

impl Node for SimpleExpression {
    fn get_position(&self) -> Position {
        match self {
            Self::If(if_expression) => if_expression.get_position(),
            Self::InfixOperation(infix_operation) => infix_operation.get_position(),
            Self::Call(call) => call.get_position(),
            Self::Identifier(identifier) => identifier.get_position(),
            Self::Number(number) => number.get_position(),
            Self::PrefixOperation(prefix_operation) => prefix_operation.get_position(),
        }
    }
}

#[derive(Debug)]
pub struct SimpleIf {
    pub condition: Box<SimpleExpression>,
    pub then_block: SimpleBlock,
    pub else_block: SimpleBlock,
    pub position: Position,
}

impl Node for SimpleIf {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}

#[derive(Debug)]
pub struct SimpleInfixOperation {
    pub left: Box<SimpleExpression>,
    pub operator: InfixOperator,
    pub right: Box<SimpleExpression>,
    pub position: Position,
}

impl Node for SimpleInfixOperation {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}

#[derive(Debug)]
pub struct SimplePrefixOperation {
    pub operator: SimplePrefixOperator,
    pub expression: Box<SimpleExpression>,
    pub position: Position,
}

impl Node for SimplePrefixOperation {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}

#[derive(Clone, Copy, Debug)]
pub enum SimplePrefixOperator {
    Not,
}

#[derive(Debug)]
pub struct SimpleCall {
    pub function: Box<SimpleExpression>,
    pub arguments: Vec<SimpleExpression>,
    pub position: Position,
}

impl Node for SimpleCall {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}
