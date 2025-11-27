use crate::{
    error_reporting::Position,
    parser::{
        program::{
            Identifier, InfixOperator, Node, Number, Parameter, Path, StringLiteral,
            StructDefinitionField, Unit,
        },
        type_::{FunctionType, Type},
    },
};
use ular_derive::Node;

#[derive(Debug, Node)]
pub struct SimpleProgram {
    pub statements: Vec<SimpleStatement>,
    pub position: Position,
}

#[derive(Debug, Node)]
pub enum SimpleStatement {
    StructDefinition(SimpleStructDefinition),
    VariableDefinition(SimpleVariableDefinition),
    FunctionDefinition(SimpleFunctionDefinition),
    Expression(Box<SimpleExpression>),
    NoOp { position: Position },
}

#[derive(Debug, Node)]
pub struct SimpleStructDefinition {
    pub name: Identifier,
    pub fields: Vec<StructDefinitionField>,
    pub methods: Vec<SimpleFunctionDefinition>,
    pub position: Position,
}

#[derive(Debug, Node)]
pub struct SimpleVariableDefinition {
    pub name: Identifier,
    pub value: Box<SimpleExpression>,
    pub position: Position,
}

#[derive(Debug, Node)]
pub struct SimpleFunctionDefinition {
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub return_type: Type,
    pub body: SimpleBlock,
    pub position: Position,
}

impl SimpleFunctionDefinition {
    pub fn reference_type(&self) -> Type {
        Type::Function(FunctionType {
            parameters: self
                .parameters
                .iter()
                .map(|parameter| parameter.type_.clone())
                .collect(),

            return_type: Box::new(self.return_type.clone()),
        })
    }
}

#[derive(Debug, Node)]
pub struct SimpleBlock {
    pub statements: Vec<SimpleStatement>,
    pub result: Option<Box<SimpleExpression>>,
    pub position: Position,
}

#[derive(Debug, Node)]
pub enum SimpleExpression {
    If(SimpleIf),
    InfixOperation(SimpleInfixOperation),
    Select(SimpleSelect),
    Call(SimpleCall),
    StructApplication(SimpleStructApplication),
    Path(Path),
    Identifier(Identifier),
    Number(Number),
    String(StringLiteral),
    PrefixOperation(SimplePrefixOperation),
    SequentialBlock(SimpleBlock),
    Unit(Unit),
}

#[derive(Debug, Node)]
pub struct SimpleIf {
    pub condition: Box<SimpleExpression>,
    pub then_block: SimpleBlock,
    pub else_block: SimpleBlock,
    pub position: Position,
}

#[derive(Debug, Node)]
pub struct SimpleInfixOperation {
    pub left: Box<SimpleExpression>,
    pub operator: InfixOperator,
    pub right: Box<SimpleExpression>,
    pub position: Position,
}

#[derive(Debug, Node)]
pub struct SimplePrefixOperation {
    pub operator: SimplePrefixOperator,
    pub expression: Box<SimpleExpression>,
    pub position: Position,
}

#[derive(Clone, Copy, Debug)]
pub enum SimplePrefixOperator {
    Not,
}

#[derive(Debug, Node)]
pub struct SimpleSelect {
    pub left_hand_side: Box<SimpleExpression>,
    pub right_hand_side: Identifier,
    pub position: Position,
}

#[derive(Debug, Node)]
pub struct SimpleCall {
    pub function: Box<SimpleExpression>,
    pub arguments: Vec<SimpleExpression>,
    pub position: Position,
}

#[derive(Debug, Node)]
pub struct SimpleStructApplication {
    pub name: Identifier,
    pub fields: Vec<SimpleStructApplicationField>,
    pub position: Position,
}

#[derive(Debug)]
pub struct SimpleStructApplicationField {
    pub name: Identifier,
    pub value: Box<SimpleExpression>,
}
