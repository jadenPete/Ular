use crate::{
    error_reporting::Position,
    parser::type_::{NumericType, Type},
};
use std::fmt::Debug;
use ular_derive::Node;

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
///     | 'unit';
///     | '(' expression ')';
///     | sequential_block;
///
/// identifier = IDENTIFIER;
/// number = NUMBER numeric_type?;
/// sequential_block = 'seq' block;
/// ```
#[derive(Debug, Node)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub position: Position,
}

#[derive(Debug, Node)]
pub enum Statement {
    VariableDefinition(VariableDefinition),
    FunctionDefinition(FunctionDefinition),
    Expression(Expression),
    NoOp { position: Position },
}

#[derive(Debug, Node)]
pub struct VariableDefinition {
    pub name: Identifier,
    pub value: Expression,
    pub position: Position,
}

#[derive(Debug, Node)]
pub struct FunctionDefinition {
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: Block,
    pub position: Position,
}

#[derive(Clone, Debug, Node)]
pub struct Parameter {
    pub name: Identifier,
    pub type_: Type,
    pub position: Position,
}

#[derive(Debug, Node)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub result: Option<Box<Expression>>,
    pub position: Position,
}

#[derive(Debug, Node)]
pub enum Expression {
    If(If),
    InfixOperation(InfixOperation),
    Call(Call),
    Identifier(Identifier),
    Number(Number),
    PrefixOperation(PrefixOperation),
    SequentialBlock(Block),
    Unit(Unit),
}

#[derive(Debug, Node)]
pub struct If {
    pub condition: Box<Expression>,
    pub body: Block,
    pub else_if_clauses: Vec<ElseIfClause>,
    pub else_clause: Option<ElseClause>,
    pub position: Position,
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

#[derive(Debug, Node)]
pub struct InfixOperation {
    pub left: Box<Expression>,
    pub operator: InfixOperator,
    pub right: Box<Expression>,
    pub position: Position,
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

#[derive(Debug, Node)]
pub struct PrefixOperation {
    pub operator: PrefixOperator,
    pub expression: Box<Expression>,
    pub position: Position,
}

#[derive(Clone, Copy, Debug)]
pub enum PrefixOperator {
    Negate,
    Not,
}

#[derive(Debug, Node)]
pub struct Call {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
    pub position: Position,
}

#[derive(Clone, Debug, Node)]
pub struct Identifier {
    pub value: String,
    pub position: Position,
}

#[derive(Clone, Debug, Node)]
pub struct Number {
    pub value: i128,
    pub suffix: Option<NumericType>,
    pub position: Position,
}

#[derive(Clone, Debug, Node)]
pub struct Unit {
    pub position: Position,
}
