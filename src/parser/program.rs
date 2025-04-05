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
/// number = NUMBER;
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
pub struct Number(pub i32);
