mod built_in_values;
mod scope;
pub mod type_;
pub mod typed_program;

use typed_program::TypedBlock;

use crate::{
    error::CompilationError,
    parser::program::{Identifier, OperatorType},
    phase::Phase,
    simplifier::simple_program::{
        SimpleBlock, SimpleCall, SimpleExpression, SimpleIf, SimpleInfix, SimpleProgram,
        SimpleStatement, SimpleVariableDefinition,
    },
    typechecker::{
        built_in_values::BuiltInValues,
        scope::TypecheckerScope,
        type_::Type,
        typed_program::{
            Typed, TypedCall, TypedExpression, TypedIdentifier, TypedIf, TypedInfix, TypedNumber,
            TypedProgram, TypedStatement, TypedVariableDefinition,
        },
    },
};

struct Typechecker<'a> {
    scope: TypecheckerScope<'a>,
}

impl<'a> Typechecker<'a> {
    fn typecheck_block(&self, block: &SimpleBlock) -> Result<TypedBlock, CompilationError> {
        let mut typechecker = Typechecker {
            scope: TypecheckerScope::with_parent(&self.scope),
        };

        let mut typechecked_statements = Vec::with_capacity(block.statements.len());

        for statement in &block.statements {
            typechecked_statements.push(typechecker.typecheck_statement(&statement)?);
        }

        let typechecked_result = match &block.result {
            Some(result) => Some(Box::new(typechecker.typecheck_expression(&result)?)),
            None => None,
        };

        Ok(TypedBlock {
            statements: typechecked_statements,
            result: typechecked_result,
        })
    }

    fn typecheck_call(&self, call: &SimpleCall) -> Result<TypedCall, CompilationError> {
        let typechecked_function = self.typecheck_identifier(&call.function)?;

        match typechecked_function.get_type() {
            Type::Function(function_type) => {
                let expected_arguments = call.arguments.len();
                let actual_arguments = function_type.parameters.len();

                if actual_arguments != expected_arguments {
                    return Err(CompilationError::IncorrectNumberOfArguments {
                        expected: expected_arguments,
                        actual: actual_arguments,
                    });
                }

                let mut typechecked_arguments = Vec::with_capacity(call.arguments.len());

                for argument in &call.arguments {
                    typechecked_arguments.push(self.typecheck_expression(&argument)?);
                }

                for i in 0..expected_arguments {
                    assert_type(&typechecked_arguments[i], &function_type.parameters[i])?;
                }

                Ok(TypedCall {
                    function: typechecked_function,
                    arguments: typechecked_arguments,
                    type_: *function_type.result,
                })
            }
            _ => Err(CompilationError::ValueNotCallable(call.function.0.clone())),
        }
    }

    fn typecheck_expression(
        &self,
        expression: &SimpleExpression,
    ) -> Result<TypedExpression, CompilationError> {
        match expression {
            SimpleExpression::If(if_expression) => {
                Ok(TypedExpression::If(self.typecheck_if(if_expression)?))
            }

            SimpleExpression::Infix(infix) => {
                Ok(TypedExpression::Infix(self.typecheck_infix(infix)?))
            }
            SimpleExpression::Call(call) => Ok(TypedExpression::Call(self.typecheck_call(call)?)),
            SimpleExpression::Identifier(identifier) => Ok(TypedExpression::Identifier(
                self.typecheck_identifier(identifier)?,
            )),

            SimpleExpression::Number(number) => Ok(TypedExpression::Number(TypedNumber(*number))),
        }
    }

    fn typecheck_identifier(
        &self,
        identifier: &Identifier,
    ) -> Result<TypedIdentifier, CompilationError> {
        self.scope
            .get_variable_value(&identifier.0)
            .map(|value| TypedIdentifier {
                underlying: identifier.clone(),
                type_: value.get_type(),
            })
            .ok_or_else(|| CompilationError::UnknownValue(identifier.0.clone()))
    }

    fn typecheck_if(&self, if_expression: &SimpleIf) -> Result<TypedIf, CompilationError> {
        let typechecked_condition = self.typecheck_expression(&if_expression.condition)?;

        assert_type(&typechecked_condition, &Type::Boolean)?;

        let typechecked_then_block = self.typecheck_block(&if_expression.then_block)?;
        let typechecked_else_block = self.typecheck_block(&if_expression.else_block)?;
        let type_ = if typechecked_then_block.get_type() == Type::Unit
            || typechecked_else_block.get_type() == Type::Unit
        {
            Type::Unit
        } else {
            assert_type(&typechecked_else_block, &typechecked_then_block.get_type())?;

            typechecked_then_block.get_type()
        };

        Ok(TypedIf {
            condition: Box::new(typechecked_condition),
            then_block: typechecked_then_block,
            else_block: typechecked_else_block,
            type_,
        })
    }

    fn typecheck_infix(&self, infix: &SimpleInfix) -> Result<TypedInfix, CompilationError> {
        let typechecked_left = self.typecheck_expression(&infix.left)?;
        let typechecked_right = self.typecheck_expression(&infix.right)?;
        let type_ = match infix.operator.operator_type() {
            OperatorType::Arithmetic => {
                assert_type(&typechecked_left, &Type::Number)?;
                assert_type(&typechecked_right, &Type::Number)?;

                Type::Number
            }

            OperatorType::Logical => {
                assert_type(&typechecked_left, &Type::Boolean)?;
                assert_type(&typechecked_right, &Type::Boolean)?;

                Type::Boolean
            }
        };

        Ok(TypedInfix {
            left: Box::new(typechecked_left),
            operator: infix.operator,
            right: Box::new(typechecked_right),
            type_,
        })
    }

    fn typecheck_statement(
        &mut self,
        statement: &SimpleStatement,
    ) -> Result<TypedStatement, CompilationError> {
        match statement {
            SimpleStatement::VariableDefinition(definition) => Ok(
                TypedStatement::VariableDefinition(self.typecheck_variable_definition(definition)?),
            ),

            SimpleStatement::Expression(expression) => Ok(TypedStatement::Expression(
                self.typecheck_expression(expression)?,
            )),

            SimpleStatement::NoOp => Ok(TypedStatement::NoOp),
        }
    }

    fn typecheck_variable_definition(
        &mut self,
        definition: &SimpleVariableDefinition,
    ) -> Result<TypedVariableDefinition, CompilationError> {
        let result = self.typecheck_expression(&definition.value)?;

        if self
            .scope
            .declare_variable(definition.name.0.clone(), result.clone())
        {
            Err(CompilationError::VariableAlreadyDefined(
                definition.name.0.clone(),
            ))
        } else {
            Ok(TypedVariableDefinition {
                name: definition.name.clone(),
                value: result,
            })
        }
    }
}

pub struct TypecheckerPhase;

impl Phase<&SimpleProgram, TypedProgram, CompilationError> for TypecheckerPhase {
    fn name() -> String {
        String::from("typechecker")
    }

    fn execute(&self, program: &SimpleProgram) -> Result<TypedProgram, CompilationError> {
        let built_in_values = BuiltInValues::new();
        let mut typechecker = Typechecker {
            scope: TypecheckerScope::without_parent(&built_in_values),
        };

        let mut statements = Vec::new();

        for statement in program.statements.iter() {
            statements.push(typechecker.typecheck_statement(statement)?);
        }

        Ok(TypedProgram { statements })
    }
}

fn assert_type<A: Typed>(value: &A, expected_type: &Type) -> Result<(), CompilationError> {
    let value_type = value.get_type();

    if value_type == *expected_type {
        Ok(())
    } else {
        Err(CompilationError::TypeMismatch {
            expected: format!("{}", expected_type),
            actual: format!("{}", value_type),
        })
    }
}
