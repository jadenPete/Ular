use crate::{
    error_reporting::Position,
    parser::{
        program::{Identifier, InfixOperator, Node},
        type_::{FunctionType, NumericType, Type},
    },
    simplifier::simple_program::SimplePrefixOperator,
};

pub trait Typed {
    fn get_type(&self) -> Type;
}

#[derive(Debug)]
pub struct TypedProgram {
    pub statements: Vec<TypedStatement>,
    pub position: Position,
}

impl Node for TypedProgram {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}

#[derive(Clone, Debug)]
pub enum TypedStatement {
    VariableDefinition(TypedVariableDefinition),
    FunctionDefinition(TypedFunctionDefinition),
    Expression(TypedExpression),
    NoOp { position: Position },
}

impl Node for TypedStatement {
    fn get_position(&self) -> Position {
        match self {
            Self::VariableDefinition(definition) => definition.get_position(),
            Self::FunctionDefinition(definition) => definition.get_position(),
            Self::Expression(expression) => expression.get_position(),
            Self::NoOp { position } => position.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypedVariableDefinition {
    pub name: Identifier,
    pub value: TypedExpression,
    pub position: Position,
}

impl Node for TypedVariableDefinition {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}

#[derive(Clone, Debug)]
pub struct TypedFunctionDefinition {
    pub name: Identifier,
    pub parameters: Vec<TypedIdentifier>,
    pub body: TypedBlock,
    pub type_: FunctionType,
    pub position: Position,
}

impl Node for TypedFunctionDefinition {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}

impl Typed for TypedFunctionDefinition {
    fn get_type(&self) -> Type {
        Type::Function(self.type_.clone())
    }
}

#[derive(Clone, Debug)]
pub enum TypedExpression {
    If(TypedIf),
    InfixOperation(TypedInfixOperation),
    Call(TypedCall),
    Identifier(TypedIdentifier),
    Number(TypedNumber),
    PrefixOperation(TypedPrefixOperation),
}

impl Node for TypedExpression {
    fn get_position(&self) -> Position {
        match self {
            TypedExpression::If(if_expression) => if_expression.get_position(),
            TypedExpression::InfixOperation(infix_operation) => infix_operation.get_position(),
            TypedExpression::Call(call) => call.get_position(),
            TypedExpression::Identifier(identifier) => identifier.get_position(),
            TypedExpression::Number(number) => number.get_position(),
            TypedExpression::PrefixOperation(prefix_operation) => prefix_operation.get_position(),
        }
    }
}

impl Typed for TypedExpression {
    fn get_type(&self) -> Type {
        match self {
            TypedExpression::If(if_expression) => if_expression.get_type(),
            TypedExpression::InfixOperation(infix_operation) => infix_operation.get_type(),
            TypedExpression::Call(call) => call.get_type(),
            TypedExpression::Identifier(identifier) => identifier.get_type(),
            TypedExpression::Number(number) => number.get_type(),
            TypedExpression::PrefixOperation(prefix_operation) => prefix_operation.get_type(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypedIf {
    pub condition: Box<TypedExpression>,
    pub then_block: TypedBlock,
    pub else_block: TypedBlock,
    pub type_: Type,
    pub position: Position,
}

impl Node for TypedIf {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}

impl Typed for TypedIf {
    fn get_type(&self) -> Type {
        self.type_.clone()
    }
}

#[derive(Clone, Debug)]
pub struct TypedBlock {
    pub statements: Vec<TypedStatement>,
    pub position: Position,
    pub result: Option<Box<TypedExpression>>,
}

impl Node for TypedBlock {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}

impl Typed for TypedBlock {
    fn get_type(&self) -> Type {
        self.result
            .as_ref()
            .map(|result| result.get_type())
            .unwrap_or(Type::Unit)
    }
}

#[derive(Clone, Debug)]
pub struct TypedInfixOperation {
    pub left: Box<TypedExpression>,
    pub operator: InfixOperator,
    pub right: Box<TypedExpression>,
    pub type_: Type,
    pub position: Position,
}

impl Node for TypedInfixOperation {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}

impl Typed for TypedInfixOperation {
    fn get_type(&self) -> Type {
        self.type_.clone()
    }
}

#[derive(Clone, Debug)]
pub struct TypedPrefixOperation {
    pub operator: SimplePrefixOperator,
    pub expression: Box<TypedExpression>,
    pub type_: Type,
    pub position: Position,
}

impl Node for TypedPrefixOperation {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}

impl Typed for TypedPrefixOperation {
    fn get_type(&self) -> Type {
        self.type_.clone()
    }
}

#[derive(Clone, Debug)]
pub struct TypedCall {
    pub function: Box<TypedExpression>,
    pub arguments: Vec<TypedExpression>,
    pub type_: Type,
    pub position: Position,
}

impl Node for TypedCall {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}

impl Typed for TypedCall {
    fn get_type(&self) -> Type {
        self.type_.clone()
    }
}

#[derive(Clone, Debug)]
pub struct TypedIdentifier {
    pub underlying: Identifier,
    pub type_: Type,
    pub position: Position,
}

impl Node for TypedIdentifier {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}

impl Typed for TypedIdentifier {
    fn get_type(&self) -> Type {
        self.type_.clone()
    }
}

#[derive(Clone, Debug)]
pub struct TypedNumber {
    pub value: i128,
    pub type_: NumericType,
    pub position: Position,
}

impl Node for TypedNumber {
    fn get_position(&self) -> Position {
        self.position.clone()
    }
}

impl Typed for TypedNumber {
    fn get_type(&self) -> Type {
        Type::Numeric(self.type_)
    }
}
