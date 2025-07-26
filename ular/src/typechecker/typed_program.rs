use crate::{
    error_reporting::Position,
    parser::{
        program::{Identifier, InfixOperator, Node, StructDefinition},
        type_::{FunctionType, NumericType, Type},
    },
    simplifier::simple_program::SimplePrefixOperator,
};
use ular_derive::{Node, Typed};

pub trait Typed {
    fn get_type(&self) -> Type;
}

#[derive(Debug, Node)]
pub struct TypedProgram {
    pub statements: Vec<TypedStatement>,
    pub position: Position,
}

#[derive(Clone, Debug, Node)]
pub enum TypedStatement {
    StructDefinition(StructDefinition),
    VariableDefinition(TypedVariableDefinition),
    FunctionDefinition(TypedFunctionDefinition),
    Expression(TypedExpression),
    NoOp { position: Position },
}

#[derive(Clone, Debug, Node)]
pub struct TypedVariableDefinition {
    pub name: Identifier,
    pub value: TypedExpression,
    pub position: Position,
}

#[derive(Clone, Debug, Node)]
pub struct TypedFunctionDefinition {
    pub name: Identifier,
    pub parameters: Vec<TypedIdentifier>,
    pub body: TypedBlock,
    pub type_: FunctionType,
    pub position: Position,
}

impl Typed for TypedFunctionDefinition {
    fn get_type(&self) -> Type {
        Type::Function(self.type_.clone())
    }
}

#[derive(Clone, Debug, Node, Typed)]
pub enum TypedExpression {
    If(TypedIf),
    InfixOperation(TypedInfixOperation),
    Select(TypedSelect),
    Call(TypedCall),
    StructApplication(TypedStructApplication),
    Identifier(TypedIdentifier),
    Number(TypedNumber),
    PrefixOperation(TypedPrefixOperation),
    SequentialBlock(TypedBlock),
    Unit(TypedUnit),
}

#[derive(Clone, Debug, Node, Typed)]
pub struct TypedIf {
    pub condition: Box<TypedExpression>,
    pub then_block: TypedBlock,
    pub else_block: TypedBlock,
    pub type_: Type,
    pub position: Position,
}

#[derive(Clone, Debug, Node)]
pub struct TypedBlock {
    pub statements: Vec<TypedStatement>,
    pub result: Option<Box<TypedExpression>>,
    pub position: Position,
}

impl Typed for TypedBlock {
    fn get_type(&self) -> Type {
        self.result
            .as_ref()
            .map(|result| result.get_type())
            .unwrap_or(Type::Unit)
    }
}

#[derive(Clone, Debug, Node, Typed)]
pub struct TypedInfixOperation {
    pub left: Box<TypedExpression>,
    pub operator: InfixOperator,
    pub right: Box<TypedExpression>,
    pub type_: Type,
    pub position: Position,
}

#[derive(Clone, Debug, Node, Typed)]
pub struct TypedPrefixOperation {
    pub operator: SimplePrefixOperator,
    pub expression: Box<TypedExpression>,
    pub type_: Type,
    pub position: Position,
}

#[derive(Clone, Debug, Node, Typed)]
pub struct TypedSelect {
    pub left_hand_side: Box<TypedExpression>,
    pub field_index: usize,
    pub type_: Type,
    pub position: Position,
}

#[derive(Clone, Debug, Node, Typed)]
pub struct TypedCall {
    pub function: Box<TypedExpression>,
    pub arguments: Vec<TypedExpression>,
    pub type_: Type,
    pub position: Position,
}

#[derive(Clone, Debug, Node)]
pub struct TypedStructApplication {
    pub name: Identifier,
    pub fields: Vec<TypedStructApplicationField>,
    pub position: Position,
}

impl Typed for TypedStructApplication {
    fn get_type(&self) -> Type {
        Type::Identifier(self.name.value.clone())
    }
}

#[derive(Clone, Debug)]
pub struct TypedStructApplicationField {
    pub name: Identifier,
    pub value: TypedExpression,
}

#[derive(Clone, Debug, Node, Typed)]
pub struct TypedIdentifier {
    pub underlying: Identifier,
    pub type_: Type,
    pub position: Position,
}

#[derive(Clone, Debug, Node)]
pub struct TypedNumber {
    pub value: i128,
    pub type_: NumericType,
    pub position: Position,
}

impl Typed for TypedNumber {
    fn get_type(&self) -> Type {
        Type::Numeric(self.type_)
    }
}

#[derive(Clone, Debug, Node)]
pub struct TypedUnit {
    pub position: Position,
}

impl Typed for TypedUnit {
    fn get_type(&self) -> Type {
        Type::Unit
    }
}
