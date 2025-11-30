pub mod built_in_values;
mod scope;
pub mod typed_program;

use crate::{
    arguments::PhaseName,
    error_reporting::{CompilationError, CompilationErrorMessage},
    parser::{
        program::{
            Identifier, InfixOperator, Node, Number, NumericInfixOperator, Path, Unit,
            UniversalInfixOperator,
        },
        type_::{FunctionType, NumericType, Type},
    },
    phase::{
        built_in_values::{BuiltInPath, BuiltInPathBuf},
        Phase,
    },
    simplifier::simple_program::{
        SimpleBlock, SimpleCall, SimpleExpression, SimpleFunctionDefinition, SimpleIf,
        SimpleInfixOperation, SimplePrefixOperation, SimplePrefixOperator, SimpleProgram,
        SimpleSelect, SimpleStatement, SimpleStructApplication, SimpleStructDefinition,
        SimpleVariableDefinition,
    },
    typechecker::{
        built_in_values::{TypecheckerBuiltInValueProducer, TypecheckerBuiltInValues},
        scope::TypecheckerScope,
        typed_program::{
            Typed, TypedBlock, TypedCall, TypedExpression, TypedFunctionDefinition,
            TypedIdentifier, TypedIf, TypedInfixOperation, TypedNumber, TypedPath,
            TypedPrefixOperation, TypedProgram, TypedSelect, TypedStatement,
            TypedStructApplication, TypedStructApplicationField, TypedStructDefinition, TypedUnit,
            TypedVariableDefinition,
        },
    },
};
use std::collections::{HashMap, HashSet};

struct Typechecker<'a> {
    built_in_values: &'a TypecheckerBuiltInValues,
    scope: TypecheckerScope<'a>,
}

impl<'a> Typechecker<'a> {
    fn with_parent(parent: &'a Typechecker<'a>) -> Self {
        Self {
            built_in_values: parent.built_in_values,
            scope: TypecheckerScope::with_parent(&parent.scope),
        }
    }

    fn typecheck_block(
        &self,
        block: &SimpleBlock,
        suggested_type: Option<&Type>,
    ) -> Result<TypedBlock, CompilationError> {
        let mut typechecker = Typechecker::with_parent(self);
        let typechecked_statements =
            typechecker.typecheck_statements_with_hoisting(&block.statements)?;

        let typechecked_result = match &block.result {
            Some(result) => Some(Box::new(
                typechecker.typecheck_expression(result, suggested_type)?,
            )),

            None => None,
        };

        Ok(TypedBlock {
            statements: typechecked_statements,
            result: typechecked_result,
            position: block.get_position(),
        })
    }

    fn typecheck_call(&self, call: &SimpleCall) -> Result<TypedCall, CompilationError> {
        let typechecked_function = self.typecheck_expression(&call.function, None)?;

        match typechecked_function.get_type() {
            Type::Function(function_type) => {
                let expected_arguments = call.arguments.len();
                let actual_arguments = function_type.parameters.len();

                if actual_arguments != expected_arguments {
                    return Err(CompilationError {
                        message: CompilationErrorMessage::IncorrectNumberOfArguments {
                            expected: expected_arguments,
                            actual: actual_arguments,
                        },

                        position: Some(call.get_position()),
                    });
                }

                let mut typechecked_arguments = Vec::with_capacity(call.arguments.len());

                for (i, argument) in call.arguments.iter().enumerate() {
                    typechecked_arguments.push(
                        self.typecheck_expression(argument, Some(&function_type.parameters[i]))?,
                    );
                }

                for (i, typechecked_argument) in typechecked_arguments.iter().enumerate() {
                    assert_type(typechecked_argument, &function_type.parameters[i])?;
                }

                Ok(TypedCall {
                    function: Box::new(typechecked_function),
                    arguments: typechecked_arguments,
                    type_: *function_type.return_type,
                    position: call.get_position(),
                })
            }

            _ => Err(CompilationError {
                message: CompilationErrorMessage::ValueNotCallable,
                position: Some(typechecked_function.get_position()),
            }),
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

            SimpleExpression::Select(select) => {
                Ok(TypedExpression::Select(self.typecheck_select(select)?))
            }

            SimpleExpression::Call(call) => Ok(TypedExpression::Call(self.typecheck_call(call)?)),
            SimpleExpression::StructApplication(struct_application) => {
                Ok(TypedExpression::StructApplication(
                    self.typecheck_struct_application(struct_application)?,
                ))
            }

            SimpleExpression::Path(path) => Ok(TypedExpression::Path(self.typecheck_path(path)?)),
            SimpleExpression::Identifier(identifier) => Ok(TypedExpression::Path(
                self.typecheck_identifier(identifier)?,
            )),

            SimpleExpression::Number(number) => Ok(TypedExpression::Number(
                self.typecheck_number(number, suggested_type)?,
            )),

            SimpleExpression::String(string) => Ok(TypedExpression::String(string.clone())),
            SimpleExpression::PrefixOperation(prefix_operation) => {
                Ok(TypedExpression::PrefixOperation(
                    self.typecheck_prefix_operation(prefix_operation, suggested_type)?,
                ))
            }

            SimpleExpression::SequentialBlock(block) => Ok(TypedExpression::SequentialBlock(
                self.typecheck_block(block, suggested_type)?,
            )),

            SimpleExpression::Unit(Unit { position }) => Ok(TypedExpression::Unit(TypedUnit {
                position: position.clone(),
            })),
        }
    }

    fn typecheck_function_definition(
        &self,
        definition: &SimpleFunctionDefinition,
    ) -> Result<TypedFunctionDefinition, CompilationError> {
        for parameter in &definition.parameters {
            self.validate_type(&parameter.type_)?;
        }

        self.validate_type(&definition.return_type)?;

        let mut typechecker = Typechecker::with_parent(self);

        for parameter in &definition.parameters {
            typechecker
                .scope
                .declare_variable(&parameter.name, parameter.type_.clone())?;
        }

        let typechecked_statements =
            typechecker.typecheck_statements_with_hoisting(&definition.body.statements)?;

        let typechecked_result = match &definition.body.result {
            Some(result) => {
                let typechecked_result =
                    typechecker.typecheck_expression(result, Some(&definition.return_type))?;

                assert_type(&typechecked_result, &definition.return_type)?;

                Some(Box::new(typechecked_result))
            }

            None => {
                if definition.return_type != Type::Unit {
                    return Err(CompilationError {
                        message: CompilationErrorMessage::ExpectedFunctionResult {
                            function_name: definition.name.value.clone(),
                            return_type: format!("{}", definition.return_type),
                        },

                        position: Some(definition.get_position()),
                    });
                }

                None
            }
        };

        Ok(TypedFunctionDefinition {
            name: definition.name.clone(),
            parameters: definition
                .parameters
                .iter()
                .map(|parameter| TypedIdentifier {
                    underlying: parameter.name.clone(),
                    type_: parameter.type_.clone(),
                    position: parameter.get_position(),
                })
                .collect(),

            body: TypedBlock {
                statements: typechecked_statements,
                result: typechecked_result,
                position: definition.body.get_position(),
            },

            type_: FunctionType {
                parameters: definition
                    .parameters
                    .iter()
                    .map(|parameter| parameter.type_.clone())
                    .collect(),

                return_type: Box::new(definition.return_type.clone()),
            },

            position: definition.get_position(),
        })
    }

    fn typecheck_identifier(&self, identifier: &Identifier) -> Result<TypedPath, CompilationError> {
        if let Some(type_) = self
            .built_in_values
            .get(&BuiltInPath::Identifier(&identifier.value))
        {
            return Ok(TypedPath::UserDefinedIdentifier(TypedIdentifier {
                underlying: identifier.clone(),
                type_: type_.clone(),
                position: identifier.get_position(),
            }));
        }

        self.scope
            .get_variable_type(&identifier.value)
            .map(|type_| {
                TypedPath::UserDefinedIdentifier(TypedIdentifier {
                    underlying: identifier.clone(),
                    type_,
                    position: identifier.get_position(),
                })
            })
            .ok_or_else(|| CompilationError {
                message: CompilationErrorMessage::UnknownValue {
                    name: identifier.value.clone(),
                },

                position: Some(identifier.get_position()),
            })
    }

    fn typecheck_if(
        &self,
        if_expression: &SimpleIf,
        suggested_type: Option<&Type>,
    ) -> Result<TypedIf, CompilationError> {
        let typechecked_condition =
            self.typecheck_expression(&if_expression.condition, Some(&Type::Bool))?;

        assert_type(&typechecked_condition, &Type::Bool)?;

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
            position: if_expression.get_position(),
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

        let type_ = match infix_operation.operator {
            InfixOperator::Logical(_) => {
                assert_type(&typechecked_left, &Type::Bool)?;
                assert_type(&typechecked_right, &Type::Bool)?;

                Type::Bool
            }

            InfixOperator::Numeric(operator) => {
                assert_numeric(&typechecked_left)?;
                assert_type(&typechecked_right, &typechecked_left.get_type())?;

                match operator {
                    NumericInfixOperator::Addition
                    | NumericInfixOperator::Subtraction
                    | NumericInfixOperator::Multiplication
                    | NumericInfixOperator::Division
                    | NumericInfixOperator::Modulo => typechecked_left.get_type(),

                    NumericInfixOperator::LessThan
                    | NumericInfixOperator::LessThanOrEqual
                    | NumericInfixOperator::GreaterThan
                    | NumericInfixOperator::GreaterThanOrEqual => Type::Bool,
                }
            }

            InfixOperator::Universal(
                UniversalInfixOperator::EqualComparison | UniversalInfixOperator::UnequalComparison,
            ) => {
                let left_type = typechecked_left.get_type();

                assert_type(&typechecked_right, &left_type)?;

                Type::Bool
            }
        };

        Ok(TypedInfixOperation {
            left: Box::new(typechecked_left),
            operator: infix_operation.operator,
            right: Box::new(typechecked_right),
            type_,
            position: infix_operation.get_position(),
        })
    }

    fn typecheck_number(
        &self,
        number: &Number,
        suggested_type: Option<&Type>,
    ) -> Result<TypedNumber, CompilationError> {
        let position = number.get_position();
        let (result, expected_type) = match (number.suffix, suggested_type) {
            (Some(suffix), _) => (
                TypedNumber {
                    value: number.value,
                    type_: suffix,
                    position,
                },
                Some(suffix),
            ),

            (None, Some(Type::Numeric(suggested))) => (
                TypedNumber {
                    value: number.value,
                    type_: *suggested,
                    position,
                },
                Some(*suggested),
            ),

            (None, _) => (
                TypedNumber {
                    value: number.value,
                    type_: NumericType::I32,
                    position,
                },
                None,
            ),
        };

        if result.type_.is_valid(result.value) {
            Ok(result)
        } else {
            Err(CompilationError {
                message: CompilationErrorMessage::NumberOutOfRange {
                    expected_type: expected_type.map(|type_| format!("{}", type_)),
                    value: number.value,
                    minimum: result.type_.minimum(),
                    maximum: result.type_.maximum(),
                },

                position: Some(number.get_position()),
            })
        }
    }

    fn typecheck_path(&self, path: &Path) -> Result<TypedPath, CompilationError> {
        if let Some(type_) = self.built_in_values.get(&BuiltInPath::Method(
            &path.left_hand_side.value,
            &path.right_hand_side.value,
        )) {
            return Ok(TypedPath::BuiltIn {
                underlying: BuiltInPathBuf::Method(
                    path.left_hand_side.value.clone(),
                    path.right_hand_side.value.clone(),
                ),

                type_: type_.clone(),
                position: path.get_position(),
            });
        }

        let struct_definition = self
            .scope
            .get_struct_definition(&path.left_hand_side.value)
            .ok_or_else(|| CompilationError {
                message: CompilationErrorMessage::UnknownType {
                    name: path.left_hand_side.value.clone(),
                },

                position: Some(path.left_hand_side.get_position()),
            })?;

        let method_index = *struct_definition
            .method_indices
            .get::<str>(&path.right_hand_side.value)
            .ok_or_else(|| CompilationError {
                message: CompilationErrorMessage::UnknownMethod {
                    type_: struct_definition.underlying.name.value.clone(),
                    method: path.right_hand_side.value.clone(),
                },

                position: Some(path.right_hand_side.get_position()),
            })?;

        Ok(TypedPath::UserDefinedMethod {
            left_hand_side: path.left_hand_side.clone(),
            method_index,
            type_: struct_definition.underlying.methods[method_index].reference_type(),
            position: path.get_position(),
        })
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
                assert_type(&typechecked_expression, &Type::Bool)?;

                Type::Bool
            }
        };

        Ok(TypedPrefixOperation {
            operator: prefix_operation.operator,
            expression: Box::new(typechecked_expression),
            type_,
            position: prefix_operation.get_position(),
        })
    }

    fn typecheck_select(&self, select: &SimpleSelect) -> Result<TypedSelect, CompilationError> {
        let typechecked_left = self.typecheck_expression(&select.left_hand_side, None)?;
        let left_type = typechecked_left.get_type();
        let unknown_field_error = || CompilationError {
            message: CompilationErrorMessage::UnknownField {
                type_: format!("{}", left_type),
                field: select.right_hand_side.value.clone(),
            },

            position: Some(select.right_hand_side.get_position()),
        };

        let struct_name = if let Type::Identifier(ref struct_name) = left_type {
            struct_name
        } else {
            return Err(unknown_field_error());
        };

        let struct_definition = self
            .scope
            .get_struct_definition(struct_name)
            .ok_or_else(|| CompilationError {
                message: CompilationErrorMessage::StructEvadesScope {
                    struct_name: struct_name.clone(),
                },
                position: Some(select.get_position()),
            })?;

        let field_index = *struct_definition
            .field_indices
            .get::<str>(&select.right_hand_side.value)
            .ok_or_else(unknown_field_error)?;

        let field = &struct_definition.underlying.fields[field_index];

        Ok(TypedSelect {
            left_hand_side: Box::new(typechecked_left),
            field_index,
            type_: field.type_.clone(),
            position: select.get_position(),
        })
    }

    fn typecheck_statement(
        &mut self,
        statement: &SimpleStatement,
    ) -> Result<TypedStatement, CompilationError> {
        match statement {
            SimpleStatement::StructDefinition(definition) => Ok(TypedStatement::StructDefinition(
                self.typecheck_struct_definition(definition)?,
            )),

            SimpleStatement::VariableDefinition(definition) => Ok(
                TypedStatement::VariableDefinition(self.typecheck_variable_definition(definition)?),
            ),

            SimpleStatement::FunctionDefinition(definition) => Ok(
                TypedStatement::FunctionDefinition(self.typecheck_function_definition(definition)?),
            ),

            SimpleStatement::Expression(expression) => Ok(TypedStatement::Expression(Box::new(
                self.typecheck_expression(expression, Some(&Type::Unit))?,
            ))),

            SimpleStatement::NoOp { position } => Ok(TypedStatement::NoOp {
                position: position.clone(),
            }),
        }
    }

    fn typecheck_statements_with_hoisting(
        &mut self,
        statements: &'a [SimpleStatement],
    ) -> Result<Vec<TypedStatement>, CompilationError> {
        for statement in statements {
            match statement {
                SimpleStatement::StructDefinition(definition) => {
                    self.scope.declare_and_validate_struct(definition)?;
                }

                SimpleStatement::FunctionDefinition(definition) => {
                    self.scope
                        .declare_variable(&definition.name, definition.reference_type())?;
                }

                _ => {}
            }
        }

        for statement in statements {
            if let SimpleStatement::StructDefinition(definition) = statement {
                self.validate_struct_definition_fields(definition)?;
            }
        }

        let mut result: Vec<Option<TypedStatement>> = vec![None; statements.len()];

        /*
         * Typecheck the functions first, so they don't yet have access to global variables.
         * Functions capturing their environment (i.e. closures) aren't yet supported.
         *
         * This won't prevent nested functions from accessing the parameters of the functions within
         * which they're nested, but the analyzer phase will take care of detecting nested functions
         * and erroring when one is detected.
         */
        for (i, statement) in statements.iter().enumerate() {
            match statement {
                SimpleStatement::StructDefinition(definition) => {
                    result[i] = Some(TypedStatement::StructDefinition(
                        self.typecheck_struct_definition(definition)?,
                    ));
                }

                SimpleStatement::FunctionDefinition(definition) => {
                    result[i] = Some(TypedStatement::FunctionDefinition(
                        self.typecheck_function_definition(definition)?,
                    ));
                }

                _ => {}
            }
        }

        for (i, statement) in statements.iter().enumerate() {
            if let SimpleStatement::StructDefinition(_) | SimpleStatement::FunctionDefinition(_) =
                statement
            {
            } else {
                result[i] = Some(self.typecheck_statement(statement)?);
            }
        }

        Ok(result
            .into_iter()
            .map(|statement| statement.unwrap())
            .collect())
    }

    fn typecheck_struct_application(
        &self,
        struct_application: &SimpleStructApplication,
    ) -> Result<TypedStructApplication, CompilationError> {
        let struct_definition = self
            .scope
            .get_struct_definition(&struct_application.name.value)
            .ok_or_else(|| CompilationError {
                message: CompilationErrorMessage::UnknownType {
                    name: struct_application.name.value.clone(),
                },
                position: Some(struct_application.name.get_position()),
            })?;

        let mut typechecked_fields = Vec::with_capacity(struct_application.fields.len());
        let mut remaining_fields = struct_definition
            .underlying
            .fields
            .iter()
            .map::<&str, _>(|field| &field.name.value)
            .collect::<HashSet<_>>();

        for field in &struct_application.fields {
            let field_index = *struct_definition
                .field_indices
                .get::<str>(&field.name.value)
                .ok_or_else(|| CompilationError {
                    message: CompilationErrorMessage::UnknownField {
                        type_: struct_definition.underlying.name.value.clone(),
                        field: field.name.value.clone(),
                    },

                    position: Some(field.name.get_position()),
                })?;

            if !remaining_fields.remove::<str>(&field.name.value) {
                return Err(CompilationError {
                    message: CompilationErrorMessage::DuplicateField {
                        field: field.name.value.clone(),
                    },

                    position: Some(field.name.get_position()),
                });
            }

            let expected_type = &struct_definition.underlying.fields[field_index].type_;
            let typechecked_field = self.typecheck_expression(&field.value, Some(expected_type))?;

            assert_type(&typechecked_field, expected_type)?;

            typechecked_fields.push(TypedStructApplicationField {
                name: field.name.clone(),
                value: Box::new(typechecked_field),
                position: field.get_position(),
            });
        }

        let mut missing_fields = remaining_fields
            .iter()
            .map(|field_name| String::from(*field_name))
            .collect::<Vec<_>>();

        missing_fields.sort();

        if !missing_fields.is_empty() {
            return Err(CompilationError {
                message: CompilationErrorMessage::MissingFields {
                    type_: struct_definition.underlying.name.value.clone(),
                    fields: missing_fields,
                },

                position: Some(struct_application.get_position()),
            });
        }

        Ok(TypedStructApplication {
            name: struct_application.name.clone(),
            fields: typechecked_fields,
            position: struct_application.get_position(),
        })
    }

    fn typecheck_struct_definition(
        &self,
        definition: &SimpleStructDefinition,
    ) -> Result<TypedStructDefinition, CompilationError> {
        Ok(TypedStructDefinition {
            name: definition.name.clone(),
            fields: definition.fields.clone(),
            methods: definition
                .methods
                .iter()
                .map(|method| self.typecheck_function_definition(method))
                .collect::<Result<_, _>>()?,

            position: definition.get_position(),
        })
    }

    fn typecheck_variable_definition(
        &mut self,
        definition: &SimpleVariableDefinition,
    ) -> Result<TypedVariableDefinition, CompilationError> {
        let result = self.typecheck_expression(&definition.value, None)?;

        self.scope
            .declare_variable(&definition.name, result.get_type())?;

        Ok(TypedVariableDefinition {
            name: definition.name.clone(),
            value: Box::new(result),
            position: definition.get_position(),
        })
    }

    fn validate_struct_definition_fields(
        &self,
        definition: &SimpleStructDefinition,
    ) -> Result<(), CompilationError> {
        for field in &definition.fields {
            self.validate_type(&field.type_)?;
        }

        Ok(())
    }

    // TODO: Differentiate `Type` as an AST node vs. an internal representation of types and add a
    // position to the former so `validate_type` can include it in the error
    fn validate_type(&self, type_: &Type) -> Result<(), CompilationError> {
        if let Type::Identifier(name) = type_ {
            if self.scope.get_struct_definition(name).is_none() {
                return Err(CompilationError {
                    message: CompilationErrorMessage::UnknownType { name: name.clone() },
                    position: None,
                });
            }
        }

        Ok(())
    }
}

pub struct TypecheckerPhase {
    pub additional_values: HashMap<String, Type>,
}

impl Phase<&SimpleProgram> for TypecheckerPhase {
    type Output = TypedProgram;

    fn name() -> PhaseName {
        PhaseName::Typechecker
    }

    fn execute(&self, program: &SimpleProgram) -> Result<TypedProgram, CompilationError> {
        let built_in_values = TypecheckerBuiltInValues::new(
            TypecheckerBuiltInValueProducer,
            self.additional_values.clone(),
        );

        let mut typechecker = Typechecker {
            built_in_values: &built_in_values,
            scope: TypecheckerScope::without_parent(),
        };

        Ok(TypedProgram {
            statements: typechecker.typecheck_statements_with_hoisting(&program.statements)?,
            position: program.get_position(),
        })
    }
}

fn assert_numeric<A: Node + Typed>(value: &A) -> Result<(), CompilationError> {
    match value.get_type() {
        Type::Numeric(_) => Ok(()),
        type_ => Err(CompilationError {
            message: CompilationErrorMessage::ExpectedNumericType {
                actual_type: format!("{}", type_),
            },

            position: Some(value.get_position()),
        }),
    }
}

fn assert_type<A: Node + Typed>(value: &A, expected_type: &Type) -> Result<(), CompilationError> {
    let value_type = value.get_type();

    if value_type == *expected_type {
        Ok(())
    } else {
        Err(CompilationError {
            message: CompilationErrorMessage::TypeMismatch {
                expected_type: format!("{}", expected_type),
                actual_type: format!("{}", value_type),
            },

            position: Some(value.get_position()),
        })
    }
}
