use crate::{
    parser::{
        program::{Identifier, InfixOperator},
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
}

#[derive(Clone, Debug)]
pub enum TypedStatement {
    VariableDefinition(TypedVariableDefinition),
    FunctionDefinition(TypedFunctionDefinition),
    Expression(TypedExpression),
    NoOp,
}

#[derive(Clone, Debug)]
pub struct TypedVariableDefinition {
    pub name: Identifier,
    pub value: TypedExpression,
}

#[derive(Clone, Debug)]
pub struct TypedFunctionDefinition {
    pub name: Identifier,
    pub parameters: Vec<TypedIdentifier>,
    pub body: TypedBlock,
    pub type_: FunctionType,
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
}

impl Typed for TypedIf {
    fn get_type(&self) -> Type {
        self.type_.clone()
    }
}

#[derive(Clone, Debug)]
pub struct TypedBlock {
    pub statements: Vec<TypedStatement>,
    pub result: Option<Box<TypedExpression>>,
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
}

impl Typed for TypedPrefixOperation {
    fn get_type(&self) -> Type {
        self.type_.clone()
    }
}

#[derive(Clone, Debug)]
pub struct TypedCall {
    pub function: TypedIdentifier,
    pub arguments: Vec<TypedExpression>,
    pub type_: Type,
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
}

impl Typed for TypedNumber {
    fn get_type(&self) -> Type {
        Type::Numeric(self.type_)
    }
}
