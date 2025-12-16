use crate::{
    error_reporting::Position,
    parser::{
        program::{
            ClosureParameter, Identifier, InfixOperator, Node, Number, Parameter, Path,
            StringLiteral, StructDefinitionField, Unit,
        },
        type_::{FunctionType, Type},
    },
};
use ular_derive::Node;

#[derive(Debug, Node)]
pub(crate) struct SimpleProgram {
    pub(crate) statements: Vec<SimpleStatement>,
    pub(crate) position: Position,
}

#[derive(Debug, Node)]
pub(crate) enum SimpleStatement {
    StructDefinition(SimpleStructDefinition),
    VariableDefinition(SimpleVariableDefinition),
    FunctionDefinition(SimpleFunctionDefinition),
    Expression(Box<SimpleExpression>),
    NoOp { position: Position },
}

#[derive(Debug, Node)]
pub(crate) struct SimpleStructDefinition {
    pub(crate) name: Identifier,
    pub(crate) fields: Vec<StructDefinitionField>,
    pub(crate) methods: Vec<SimpleFunctionDefinition>,
    pub(crate) position: Position,
}

#[derive(Debug, Node)]
pub(crate) struct SimpleVariableDefinition {
    pub(crate) name: Identifier,
    pub(crate) value: Box<SimpleExpression>,
    pub(crate) position: Position,
}

#[derive(Debug, Node)]
pub(crate) struct SimpleFunctionDefinition {
    pub(crate) name: Identifier,
    pub(crate) parameters: Vec<Parameter>,
    pub(crate) return_type: Type,
    pub(crate) body: SimpleBlock,
    pub(crate) position: Position,
}

impl SimpleFunctionDefinition {
    pub(crate) fn reference_type(&self) -> Type {
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
pub(crate) struct SimpleBlock {
    pub(crate) statements: Vec<SimpleStatement>,
    pub(crate) result: Option<Box<SimpleExpression>>,
    pub(crate) position: Position,
}

#[derive(Debug, Node)]
pub(crate) enum SimpleExpression {
    If(SimpleIf),
    InfixOperation(SimpleInfixOperation),
    Select(SimpleSelect),
    Call(SimpleCall),
    Closure(SimpleClosure),
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
pub(crate) struct SimpleIf {
    pub(crate) condition: Box<SimpleExpression>,
    pub(crate) then_block: SimpleBlock,
    pub(crate) else_block: SimpleBlock,
    pub(crate) position: Position,
}

#[derive(Debug, Node)]
pub(crate) struct SimpleInfixOperation {
    pub(crate) left: Box<SimpleExpression>,
    pub(crate) operator: InfixOperator,
    pub(crate) right: Box<SimpleExpression>,
    pub(crate) position: Position,
}

#[derive(Debug, Node)]
pub(crate) struct SimplePrefixOperation {
    pub(crate) operator: SimplePrefixOperator,
    pub(crate) expression: Box<SimpleExpression>,
    pub(crate) position: Position,
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum SimplePrefixOperator {
    Not,
}

#[derive(Debug, Node)]
pub(crate) struct SimpleSelect {
    pub(crate) left_hand_side: Box<SimpleExpression>,
    pub(crate) right_hand_side: Identifier,
    pub(crate) position: Position,
}

#[derive(Debug, Node)]
pub(crate) struct SimpleCall {
    pub(crate) function: Box<SimpleExpression>,
    pub(crate) arguments: Vec<SimpleExpression>,
    pub(crate) position: Position,
}

#[derive(Debug, Node)]
pub(crate) struct SimpleClosure {
    pub(crate) parameters: Vec<ClosureParameter>,
    pub(crate) return_type: Option<Type>,
    pub(crate) body: SimpleBlock,
    pub(crate) position: Position,
}

#[derive(Debug, Node)]
pub(crate) struct SimpleStructApplication {
    pub(crate) name: Identifier,
    pub(crate) fields: Vec<SimpleStructApplicationField>,
    pub(crate) position: Position,
}

#[derive(Debug, Node)]
pub(crate) struct SimpleStructApplicationField {
    pub(crate) name: Identifier,
    pub(crate) value: Box<SimpleExpression>,
    pub(crate) position: Position,
}
