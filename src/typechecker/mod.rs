mod built_in_values;
mod scope;
pub mod type_;
pub mod typed_program;

use crate::{
    error::CompilationError,
    parser::program::{Identifier, Number, NumericType, OperatorType},
    phase::Phase,
    simplifier::simple_program::{
        SimpleBlock, SimpleCall, SimpleExpression, SimpleIf, SimpleInfixOperation,
        SimplePrefixOperation, SimplePrefixOperator, SimpleProgram, SimpleStatement,
        SimpleVariableDefinition,
    },
    typechecker::{
        built_in_values::BuiltInValues,
        scope::TypecheckerScope,
        type_::Type,
        typed_program::{
            Typed, TypedBlock, TypedCall, TypedExpression, TypedIdentifier, TypedIf,
            TypedInfixOperation, TypedNumber, TypedPrefixOperation, TypedProgram, TypedStatement,
            TypedVariableDefinition,
        },
    },
};

struct Typechecker<'a> {
    scope: TypecheckerScope<'a>,
}

impl<'a> Typechecker<'a> {
    fn typecheck_block(
        &self,
        block: &SimpleBlock,
        suggested_type: Option<&Type>,
    ) -> Result<TypedBlock, CompilationError> {
        let mut typechecker = Typechecker {
            scope: TypecheckerScope::with_parent(&self.scope),
        };

        let mut typechecked_statements = Vec::with_capacity(block.statements.len());

        for statement in &block.statements {
            typechecked_statements.push(typechecker.typecheck_statement(&statement)?);
        }

        let typechecked_result = match &block.result {
            Some(result) => Some(Box::new(
                typechecker.typecheck_expression(&result, suggested_type)?,
            )),
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

                for (i, argument) in call.arguments.iter().enumerate() {
                    typechecked_arguments.push(
                        self.typecheck_expression(&argument, Some(&function_type.parameters[i]))?,
                    );
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
        suggested_type: Option<&Type>,
    ) -> Result<TypedExpression, CompilationError> {
        match expression {
            SimpleExpression::If(if_expression) => Ok(TypedExpression::If(
                self.typecheck_if(if_expression, suggested_type)?,
            )),

            SimpleExpression::InfixOperation(infix_operation) => {
                Ok(TypedExpression::InfixOperation(
                    self.typecheck_infix_operation(infix_operation, suggested_type)?,
                ))
            }

            SimpleExpression::Call(call) => Ok(TypedExpression::Call(self.typecheck_call(call)?)),
            SimpleExpression::Identifier(identifier) => Ok(TypedExpression::Identifier(
                self.typecheck_identifier(identifier)?,
            )),

            SimpleExpression::Number(number) => Ok(TypedExpression::Number(
                self.typecheck_number(number, suggested_type)?,
            )),

            SimpleExpression::PrefixOperation(prefix_operation) => {
                Ok(TypedExpression::PrefixOperation(
                    self.typecheck_prefix_operation(prefix_operation, suggested_type)?,
                ))
            }
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

    fn typecheck_if(
        &self,
        if_expression: &SimpleIf,
        suggested_type: Option<&Type>,
    ) -> Result<TypedIf, CompilationError> {
        let typechecked_condition =
            self.typecheck_expression(&if_expression.condition, Some(&Type::Boolean))?;

        assert_type(&typechecked_condition, &Type::Boolean)?;

        let typechecked_then_block =
            self.typecheck_block(&if_expression.then_block, suggested_type)?;

        let typechecked_else_block =
            self.typecheck_block(&if_expression.else_block, suggested_type)?;

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

    fn typecheck_infix_operation(
        &self,
        infix_operation: &SimpleInfixOperation,
        suggested_type: Option<&Type>,
    ) -> Result<TypedInfixOperation, CompilationError> {
        let typechecked_left = self.typecheck_expression(&infix_operation.left, suggested_type)?;
        let typechecked_right =
            self.typecheck_expression(&infix_operation.right, suggested_type)?;

        let type_ = match infix_operation.operator.operator_type() {
            OperatorType::Arithmetic => {
                assert_numeric(&typechecked_left)?;
                assert_type(&typechecked_right, &typechecked_left.get_type())?;

                typechecked_left.get_type()
            }

            OperatorType::Logical => {
                assert_type(&typechecked_left, &Type::Boolean)?;
                assert_type(&typechecked_right, &Type::Boolean)?;

                Type::Boolean
            }
        };

        Ok(TypedInfixOperation {
            left: Box::new(typechecked_left),
            operator: infix_operation.operator,
            right: Box::new(typechecked_right),
            type_,
        })
    }

    fn typecheck_number(
        &self,
        number: &Number,
        suggested_type: Option<&Type>,
    ) -> Result<TypedNumber, CompilationError> {
        let result = match (number.suffix, suggested_type) {
            (Some(suffix), _) => TypedNumber {
                value: number.value,
                type_: suffix,
            },

            (None, Some(Type::Numeric(suggested))) => TypedNumber {
                value: number.value,
                type_: suggested.clone(),
            },

            (None, _) => TypedNumber {
                value: number.value,
                type_: NumericType::I32,
            },
        };

        if result.type_.is_valid(result.value) {
            Ok(result)
        } else {
            Err(CompilationError::NumberOutOfRange {
                type_: format!("{}", result.type_),
            })
        }
    }

    fn typecheck_prefix_operation(
        &self,
        prefix_operation: &SimplePrefixOperation,
        suggested_type: Option<&Type>,
    ) -> Result<TypedPrefixOperation, CompilationError> {
        let typechecked_expression =
            self.typecheck_expression(&prefix_operation.expression, suggested_type)?;

        let type_ = match prefix_operation.operator {
            SimplePrefixOperator::Not => {
                assert_type(&typechecked_expression, &Type::Boolean)?;

                Type::Boolean
            }
        };

        Ok(TypedPrefixOperation {
            operator: prefix_operation.operator,
            expression: Box::new(typechecked_expression),
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
                self.typecheck_expression(expression, Some(&Type::Unit))?,
            )),

            SimpleStatement::NoOp => Ok(TypedStatement::NoOp),
        }
    }

    fn typecheck_variable_definition(
        &mut self,
        definition: &SimpleVariableDefinition,
    ) -> Result<TypedVariableDefinition, CompilationError> {
        let result = self.typecheck_expression(&definition.value, None)?;

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

fn assert_numeric<A: Typed>(value: &A) -> Result<(), CompilationError> {
    match value.get_type() {
        Type::Numeric(_) => Ok(()),
        type_ => Err(CompilationError::ExpectedNumericType {
            actual_type: format!("{}", type_),
        }),
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
