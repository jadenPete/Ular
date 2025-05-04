use crate::{
    error_reporting::Position,
    parser::type_::{NumericType, Type},
};
use std::fmt::Debug;

pub trait Node {
    fn get_position(&self) -> Position;
}

/// Grammar:
/// ```ebnf
/// program = statement*;
/// statement = variable_definition | function_definition | expression ';' | ';';
/// variable_definition = identifier ':=' expression ';';
/// function_definition = 'fn' identifier '(' (parameter (',' parameter)*)? ')' (':' type)? block;
/// parameter = identifier ':' type;
/// type = function_type;
/// function_type =
///     | (primary_type | ('(' (type (',' type)*)? ')')) '=>' type
///     | primary_type;
///
/// primary_type =
///     | numeric_type
///     | 'bool'
///     | 'unit';
///
/// numeric_type = 'i8' | 'i16' | 'i32' | 'i64' | 'u8' | 'u16' | 'u32' | 'u64';
/// block = '{' statement* expression? '}';
/// expression =  logical_or;
/// logical_or = logical_and ('||' logical_and)*;
/// logical_and = sum ('&&' sum)*;
/// sum =
/// 	| product ('+' product)*
/// 	| product ('-' product)*;
///
/// product =
/// 	| prefix_operation ('*' prefix_operation)*
/// 	| prefix_operation ('/' prefix_operation)*;
///
/// prefix_operation =
///     | '!' prefix_operation
///     | '-' prefix_operation
///     | call;
///
/// call =
/// 	| if ('(' ((expression ',')* expression)? ')')+
/// 	| if;
///
/// if =
///     | 'if' expression block else_if_clause* else_clause?
///     | primary;
///
/// else_if_clause = 'else' 'if' expression block;
/// else_clause = 'else' block;
/// primary =
/// 	| identifier
/// 	| number;
///     | '(' expression ')';
///
/// identifier = IDENTIFIER;
/// number = NUMBER numeric_type?;
/// ```
#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub position: Position,
}

impl Node for Program {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}

#[derive(Debug)]
pub enum Statement {
    VariableDefinition(VariableDefinition),
    FunctionDefinition(FunctionDefinition),
    Expression(Expression),
    NoOp { position: Position },
}

impl Node for Statement {
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
pub struct VariableDefinition {
    pub name: Identifier,
    pub value: Expression,
    pub position: Position,
}

impl Node for VariableDefinition {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: Block,
    pub position: Position,
}

impl Node for FunctionDefinition {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}

#[derive(Clone, Debug)]
pub struct Parameter {
    pub name: Identifier,
    pub type_: Type,
    pub position: Position,
}

impl Node for Parameter {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}

#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub result: Option<Box<Expression>>,
    pub position: Position,
}

impl Node for Block {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}

#[derive(Debug)]
pub enum Expression {
    If(If),
    InfixOperation(InfixOperation),
    Call(Call),
    Identifier(Identifier),
    Number(Number),
    PrefixOperation(PrefixOperation),
}

impl Node for Expression {
    fn get_position(&self) -> Position {
        match self {
            Expression::If(if_expression) => if_expression.get_position(),
            Expression::InfixOperation(infix_operation) => infix_operation.get_position(),
            Expression::Call(call) => call.get_position(),
            Expression::Identifier(identifier) => identifier.get_position(),
            Expression::Number(number) => number.get_position(),
            Expression::PrefixOperation(prefix_operation) => prefix_operation.get_position(),
        }
    }
}

#[derive(Debug)]
pub struct If {
    pub condition: Box<Expression>,
    pub body: Block,
    pub else_if_clauses: Vec<ElseIfClause>,
    pub else_clause: Option<ElseClause>,
    pub position: Position,
}

impl Node for If {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}

#[derive(Debug)]
pub struct ElseIfClause {
    pub condition: Box<Expression>,
    pub body: Block,
    pub position: Position,
}

#[derive(Debug)]
pub struct ElseClause {
    pub body: Block,
}

#[derive(Debug)]
pub struct InfixOperation {
    pub left: Box<Expression>,
    pub operator: InfixOperator,
    pub right: Box<Expression>,
    pub position: Position,
}

impl Node for InfixOperation {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}

#[derive(Clone, Copy, Debug)]
pub enum InfixOperator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulo,
    LogicalAnd,
    LogicalOr,
}

impl InfixOperator {
    pub fn operator_type(self) -> OperatorType {
        match self {
            Self::Addition
            | Self::Subtraction
            | Self::Multiplication
            | Self::Division
            | Self::Modulo => OperatorType::Arithmetic,

            Self::LogicalAnd | Self::LogicalOr => OperatorType::Logical,
        }
    }
}

#[derive(Clone, Copy)]
pub enum OperatorType {
    Arithmetic,
    Logical,
}

#[derive(Debug)]
pub struct PrefixOperation {
    pub operator: PrefixOperator,
    pub expression: Box<Expression>,
    pub position: Position,
}

impl Node for PrefixOperation {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}

#[derive(Clone, Copy, Debug)]
pub enum PrefixOperator {
    Negate,
    Not,
}

#[derive(Debug)]
pub struct Call {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
    pub position: Position,
}

impl Node for Call {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}

#[derive(Clone, Debug)]
pub struct Identifier {
    pub value: String,
    pub position: Position,
}

impl Node for Identifier {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}

#[derive(Clone, Debug)]
pub struct Number {
    pub value: i128,
    pub suffix: Option<NumericType>,
    pub position: Position,
}

impl Node for Number {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}
