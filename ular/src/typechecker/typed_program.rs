use crate::{
    error_reporting::Position,
    parser::{
        program::{Identifier, InfixOperator, Node, StringLiteral, StructDefinitionField},
        type_::{FunctionType, NumericType, Type},
    },
    phase::built_in_values::BuiltInPathBuf,
    simplifier::simple_program::SimplePrefixOperator,
};
use ular_derive::{Node, Typed};

pub(crate) trait Typed {
    fn get_type(&self) -> Type;
}

impl Typed for StringLiteral {
    fn get_type(&self) -> Type {
        Type::Str
    }
}

#[derive(Debug, Node)]
pub(crate) struct TypedProgram {
    pub(crate) statements: Vec<TypedStatement>,
    pub(crate) position: Position,
}

#[derive(Clone, Debug, Node)]
pub(crate) enum TypedStatement {
    StructDefinition(TypedStructDefinition),
    VariableDefinition(TypedVariableDefinition),
    FunctionDefinition(TypedFunctionDefinition),
    Expression(Box<TypedExpression>),
    NoOp { position: Position },
}

#[derive(Clone, Debug, Node)]
pub(crate) struct TypedStructDefinition {
    pub(crate) name: Identifier,
    pub(crate) fields: Vec<StructDefinitionField>,
    pub(crate) methods: Vec<TypedFunctionDefinition>,
    pub(crate) position: Position,
}

#[derive(Clone, Debug, Node)]
pub(crate) struct TypedVariableDefinition {
    pub(crate) name: Identifier,
    pub(crate) value: Box<TypedExpression>,
    pub(crate) position: Position,
}

#[derive(Clone, Debug, Node)]
pub(crate) struct TypedFunctionDefinition {
    pub(crate) name: Identifier,
    pub(crate) parameters: Vec<TypedIdentifier>,
    pub(crate) body: TypedBlock,
    pub(crate) type_: FunctionType,
    pub(crate) position: Position,
}

#[derive(Clone, Debug, Node, Typed)]
pub(crate) enum TypedExpression {
    If(TypedIf),
    InfixOperation(TypedInfixOperation),
    Select(TypedSelect),
    Call(TypedCall),
    Closure(TypedClosure),
    StructApplication(TypedStructApplication),
    Path(TypedPath),
    Number(TypedNumber),
    String(StringLiteral),
    PrefixOperation(TypedPrefixOperation),
    SequentialBlock(TypedBlock),
    Unit(TypedUnit),
}

#[derive(Clone, Debug, Node, Typed)]
pub(crate) struct TypedIf {
    pub(crate) condition: Box<TypedExpression>,
    pub(crate) then_block: TypedBlock,
    pub(crate) else_block: TypedBlock,
    pub(crate) type_: Type,
    pub(crate) position: Position,
}

#[derive(Clone, Debug, Node)]
pub(crate) struct TypedBlock {
    pub(crate) statements: Vec<TypedStatement>,
    pub(crate) result: Option<Box<TypedExpression>>,
    pub(crate) position: Position,
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
pub(crate) struct TypedInfixOperation {
    pub(crate) left: Box<TypedExpression>,
    pub(crate) operator: InfixOperator,
    pub(crate) right: Box<TypedExpression>,
    pub(crate) type_: Type,
    pub(crate) position: Position,
}

#[derive(Clone, Debug, Node, Typed)]
pub(crate) struct TypedPrefixOperation {
    pub(crate) operator: SimplePrefixOperator,
    pub(crate) expression: Box<TypedExpression>,
    pub(crate) type_: Type,
    pub(crate) position: Position,
}

#[derive(Clone, Debug, Node, Typed)]
pub(crate) struct TypedSelect {
    pub(crate) left_hand_side: Box<TypedExpression>,
    pub(crate) field_index: usize,
    pub(crate) type_: Type,
    pub(crate) position: Position,
}

#[derive(Clone, Debug, Node, Typed)]
pub(crate) struct TypedCall {
    pub(crate) function: Box<TypedExpression>,
    pub(crate) arguments: Vec<TypedExpression>,
    pub(crate) type_: Type,
    pub(crate) position: Position,
}

#[derive(Clone, Debug, Node)]
pub(crate) struct TypedClosure {
    pub(crate) parameters: Vec<TypedIdentifier>,
    pub(crate) body: TypedBlock,
    pub(crate) type_: FunctionType,
    pub(crate) position: Position,
}

impl Typed for TypedClosure {
    fn get_type(&self) -> Type {
        Type::Function(self.type_.clone())
    }
}

#[derive(Clone, Debug, Node)]
pub(crate) struct TypedStructApplication {
    pub(crate) name: Identifier,
    pub(crate) fields: Vec<TypedStructApplicationField>,
    pub(crate) position: Position,
}

impl Typed for TypedStructApplication {
    fn get_type(&self) -> Type {
        Type::Identifier(self.name.value.clone())
    }
}

#[derive(Clone, Debug, Node, Typed)]
pub(crate) enum TypedPath {
    BuiltIn {
        underlying: BuiltInPathBuf,
        type_: Type,
        position: Position,
    },

    UserDefinedIdentifier(TypedIdentifier),
    UserDefinedMethod {
        left_hand_side: Identifier,
        method_index: usize,
        type_: Type,
        position: Position,
    },
}

#[derive(Clone, Debug, Node)]
pub(crate) struct TypedStructApplicationField {
    pub(crate) name: Identifier,
    pub(crate) value: Box<TypedExpression>,
    pub(crate) position: Position,
}

#[derive(Clone, Debug, Node, Typed)]
pub(crate) struct TypedIdentifier {
    pub(crate) underlying: Identifier,
    pub(crate) type_: Type,
    pub(crate) position: Position,
}

#[derive(Clone, Debug, Node)]
pub(crate) struct TypedNumber {
    pub(crate) value: i128,
    pub(crate) type_: NumericType,
    pub(crate) position: Position,
}

impl Typed for TypedNumber {
    fn get_type(&self) -> Type {
        Type::Numeric(self.type_)
    }
}

#[derive(Clone, Debug, Node)]
pub(crate) struct TypedUnit {
    pub(crate) position: Position,
}

impl Typed for TypedUnit {
    fn get_type(&self) -> Type {
        Type::Unit
    }
}
