use enum_iterator::Sequence;
use inkwell::{context::Context, types::IntType};
use std::fmt::{Debug, Display, Formatter};

/// Grammar:
/// ```ebnf
/// program = statement*;
/// statement = variable_definition | expression ';' | ';';
/// variable_definition = identifier ':=' expression ';';
/// expression = if;
/// if =
///     | 'if' expression block else_if_clause* else_clause?
///     | logical_or;
///
/// else_if_clause = 'else' 'if' expression block;
/// else_clause = 'else' block;
/// block = '{' statement* expression? '}';
/// logical_or =
///     | logical_and '||' logical_or
///     | logical_and;
///
/// logical_and =
///     | sum '&&' logical_and
///     | sum;
///
/// sum =
/// 	| product '+' sum
/// 	| product '-' sum
/// 	| product;
///
/// product =
/// 	| call '*' product
/// 	| call '/' product
/// 	| call;
///
/// call =
/// 	| identifier '(' (expression ',')* expression ')'
/// 	| primary
///
/// primary =
/// 	| identifier
/// 	| number;
///
/// identifier = IDENTIFIER;
/// number = NUMBER number_type?;
/// number_type = 'i8' | 'i16' | 'i32' | 'i64' | 'u8' | 'u16' | 'u32' | 'u64';
/// ```
#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    VariableDefinition(VariableDefinition),
    Expression(Expression),
    NoOp,
}

#[derive(Debug)]
pub struct VariableDefinition {
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Debug)]
pub enum Expression {
    If(If),
    Infix(Infix),
    Call(Call),
    Identifier(Identifier),
    Number(Number),
}

#[derive(Debug)]
pub struct If {
    pub condition: Box<Expression>,
    pub body: Block,
    pub else_if_clauses: Vec<ElseIfClause>,
    pub else_clause: Option<ElseClause>,
}

#[derive(Debug)]
pub struct ElseIfClause {
    pub condition: Box<Expression>,
    pub body: Block,
}

#[derive(Debug)]
pub struct ElseClause {
    pub body: Block,
}

#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub result: Option<Box<Expression>>,
}

#[derive(Debug)]
pub struct Infix {
    pub left: Box<Expression>,
    pub operator: Operator,
    pub right: Box<Expression>,
}

#[derive(Clone, Copy, Debug)]
pub enum Operator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulo,
    LogicalAnd,
    LogicalOr,
}

impl Operator {
    pub fn operator_type(self) -> OperatorType {
        match self {
            Operator::Addition
            | Operator::Subtraction
            | Operator::Multiplication
            | Operator::Division
            | Operator::Modulo => OperatorType::Arithmetic,

            Operator::LogicalAnd | Operator::LogicalOr => OperatorType::Logical,
        }
    }
}

#[derive(Clone, Copy)]
pub enum OperatorType {
    Arithmetic,
    Logical,
}

#[derive(Debug)]
pub struct Call {
    pub function: Identifier,
    pub arguments: Vec<Expression>,
}

#[derive(Clone, Debug)]
pub struct Identifier(pub String);

#[derive(Clone, Copy, Debug)]
pub struct Number {
    pub value: i128,
    pub suffix: Option<NumericType>,
}

#[derive(Clone, Copy, PartialEq, Sequence)]
pub enum NumericType {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
}

impl NumericType {
    pub fn is_signed(self) -> bool {
        match self {
            Self::I8 => true,
            Self::I16 => true,
            Self::I32 => true,
            Self::I64 => true,
            Self::U8 => false,
            Self::U16 => false,
            Self::U32 => false,
            Self::U64 => false,
        }
    }

    pub fn is_valid(self, value: i128) -> bool {
        match self {
            Self::I8 => i8::try_from(value).is_ok(),
            Self::I16 => i16::try_from(value).is_ok(),
            Self::I32 => i32::try_from(value).is_ok(),
            Self::I64 => i64::try_from(value).is_ok(),
            Self::U8 => u8::try_from(value).is_ok(),
            Self::U16 => u16::try_from(value).is_ok(),
            Self::U32 => u32::try_from(value).is_ok(),
            Self::U64 => u64::try_from(value).is_ok(),
        }
    }

    pub fn inkwell_type<'a>(self, context: &'a Context) -> IntType<'a> {
        match self {
            Self::I8 | Self::U8 => context.i8_type(),
            Self::I16 | Self::U16 => context.i16_type(),
            Self::I32 | Self::U32 => context.i32_type(),
            Self::I64 | Self::U64 => context.i64_type(),
        }
    }
}

impl Debug for NumericType {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::I8 => write!(formatter, "i8"),
            Self::I16 => write!(formatter, "i16"),
            Self::I32 => write!(formatter, "i32"),
            Self::I64 => write!(formatter, "i64"),
            Self::U8 => write!(formatter, "u8"),
            Self::U16 => write!(formatter, "u16"),
            Self::U32 => write!(formatter, "u32"),
            Self::U64 => write!(formatter, "u64"),
        }
    }
}

impl Display for NumericType {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{:?}", self)
    }
}
