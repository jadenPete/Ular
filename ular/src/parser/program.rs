use crate::{
    error_reporting::Position,
    parser::type_::{NumericType, Type},
};
use std::fmt::Debug;
use ular_derive::Node;

pub trait Node {
    fn get_position(&self) -> Position;
}

/// The abstract syntax tree ([AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree)] element
/// containing the entire program.
///
/// It follows the following grammar, defined in a modified
/// [extended Backus–Naur form](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form):
/// ```ebnf
/// program = statement*;
/// statement = struct_definition | variable_definition | function_definition | expression ';' | ';';
/// struct_definition =
///     'struct' identifier '{' (struct_definition_field ';')+ struct_method* '}';
///
/// struct_definition_field = identifier ':' type;
/// struct_method =
///     'fn' identifier '(' (('self' | parameter) (',' parameter)*)? ')' (':' type)? block;
///
/// variable_definition = identifier ':=' expression ';';
/// function_definition = 'fn' identifier '(' (parameter (',' parameter)*)? ')' (':' type)? block;
/// parameter = identifier ':' type;
/// type = function_type;
/// function_type =
///     | (primary_type | ('(' (type (',' type)*)? ')')) '=>' type
///     | primary_type;
///
/// primary_type =
///     | identifier
///     | numeric_type
///     | 'bool'
///     | 'str'
///     | 'unit';
///
/// numeric_type = 'i8' | 'i16' | 'i32' | 'i64' | 'u8' | 'u16' | 'u32' | 'u64';
/// block = '{' statement* expression? '}';
/// expression = comparison;
/// comparison =
///     | logical_or '==' logical_or
///     | logical_or '<' logical_or
///     | logical_or '<=' logical_or
///     | logical_or '>' logical_or
///     | logical_or '>=' logical_or
///     | logical_or '<=' logical_or
///     | logical_or '!=' logical_or
///     | logical_or;
///
/// logical_or = logical_and ('||' logical_and)*;
/// logical_and = sum ('&&' sum)*;
/// sum =
///     | product ('+' product)*
///     | product ('-' product)*;
///
/// product =
///     | prefix_operation ('*' prefix_operation)*
///     | prefix_operation ('/' prefix_operation)*;
///
/// prefix_operation =
///     | '!' prefix_operation
///     | '-' prefix_operation
///     | select;
///
/// select =
///     | call '.' identifier
///     | call;
///
/// call =
///     | if ('(' ((expression ',')* expression)? ')')+
///     | if;
///
/// if =
///     | 'if' expression block else_if_clause* else_clause?
///     | primary;
///
/// else_if_clause = 'else' 'if' expression block;
/// else_clause = 'else' block;
/// primary =
///     | struct_application
///     | path
///     | identifier
///     | number;
///     | string;
///     | 'unit';
///     | '(' expression ')';
///     | sequential_block;
///
/// struct_application = identifier '{' struct_application_field (',' struct_application_field)* '}';
/// struct_application_field = identifier ':' expression;
/// path = identifier '::' identifier;
/// identifier = IDENTIFIER;
/// number = NUMBER numeric_type?;
/// string = STRING;
/// sequential_block = 'seq' block;
/// ```
#[derive(Debug, Node)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub position: Position,
}

#[derive(Debug, Node)]
pub enum Statement {
    StructDefinition(StructDefinition),
    VariableDefinition(VariableDefinition),
    FunctionDefinition(FunctionDefinition),
    Expression(Box<Expression>),
    NoOp { position: Position },
}

#[derive(Debug, Node)]
pub struct StructDefinition {
    pub name: Identifier,
    pub fields: Vec<StructDefinitionField>,
    pub methods: Vec<FunctionDefinition>,
    pub position: Position,
}

#[derive(Clone, Debug, Node)]
pub struct StructDefinitionField {
    pub name: Identifier,
    pub type_: Type,
    pub position: Position,
}

#[derive(Debug, Node)]
pub struct VariableDefinition {
    pub name: Identifier,
    pub value: Box<Expression>,
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
    Select(Select),
    Call(Call),
    StructApplication(StructApplication),
    Path(Path),
    Identifier(Identifier),
    Number(Number),
    String(StringLiteral),
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

#[derive(Debug, Node)]
pub struct ElseIfClause {
    pub condition: Box<Expression>,
    pub body: Block,
    pub position: Position,
}

#[derive(Debug, Node)]
pub struct ElseClause {
    pub body: Block,
    pub position: Position,
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
    Logical(LogicalInfixOperator),
    Numeric(NumericInfixOperator),
    Universal(UniversalInfixOperator),
}

#[derive(Clone, Copy, Debug)]
pub enum LogicalInfixOperator {
    LogicalAnd,
    LogicalOr,
}

#[derive(Clone, Copy, Debug)]
pub enum NumericInfixOperator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulo,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

#[derive(Clone, Copy, Debug)]
pub enum UniversalInfixOperator {
    EqualComparison,
    UnequalComparison,
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
pub struct Select {
    pub left_hand_side: Box<Expression>,
    pub right_hand_side: Identifier,
    pub position: Position,
}

#[derive(Debug, Node)]
pub struct Call {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
    pub position: Position,
}

#[derive(Debug, Node)]
pub struct StructApplication {
    pub name: Identifier,
    pub fields: Vec<StructApplicationField>,
    pub position: Position,
}

#[derive(Debug, Node)]
pub struct StructApplicationField {
    pub name: Identifier,
    pub value: Box<Expression>,
    pub position: Position,
}

#[derive(Clone, Debug, Node)]
pub struct Path {
    pub left_hand_side: Identifier,
    pub right_hand_side: Identifier,
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
pub struct StringLiteral {
    pub value: String,
    pub position: Position,
}

#[derive(Clone, Debug, Node)]
pub struct Unit {
    pub position: Position,
}
