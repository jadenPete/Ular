pub(crate) mod simple_program;

use crate::{
    arguments::PhaseName,
    error_reporting::{CompilationError, Position},
    parser::{
        program::{
            Block, Call, Closure, Expression, FunctionDefinition, If, InfixOperation,
            InfixOperator, Node, Number, NumericInfixOperator, PrefixOperation, PrefixOperator,
            Program, Select, Statement, StructApplication, StructDefinition, VariableDefinition,
        },
        type_::Type,
    },
    phase::Phase,
    simplifier::simple_program::{
        SimpleBlock, SimpleCall, SimpleClosure, SimpleExpression, SimpleFunctionDefinition,
        SimpleIf, SimpleInfixOperation, SimplePrefixOperation, SimplePrefixOperator, SimpleProgram,
        SimpleSelect, SimpleStatement, SimpleStructApplication, SimpleStructApplicationField,
        SimpleStructDefinition, SimpleVariableDefinition,
    },
};

pub(crate) struct SimplifierPhase;

impl Phase<&Program> for SimplifierPhase {
    type Output = SimpleProgram;

    fn name() -> PhaseName {
        PhaseName::Simplifier
    }

    fn execute(&self, program: &Program) -> Result<SimpleProgram, CompilationError> {
        Ok(simplify_program(program))
    }
}

fn simplify_block(block: &Block) -> SimpleBlock {
    SimpleBlock {
        statements: block.statements.iter().map(simplify_statement).collect(),
        result: block
            .result
            .as_ref()
            .map(|result| Box::new(simplify_expression(result))),

        position: block.get_position(),
    }
}

fn simplify_call(call: &Call) -> SimpleCall {
    SimpleCall {
        function: Box::new(simplify_expression(&call.function)),
        arguments: call.arguments.iter().map(simplify_expression).collect(),
        position: call.get_position(),
    }
}

fn simplify_closure(closure: &Closure) -> SimpleClosure {
    SimpleClosure {
        parameters: closure.parameters.clone(),
        return_type: closure.return_type.clone(),
        body: simplify_block(&closure.body),
        position: closure.get_position(),
    }
}

fn simplify_expression(expression: &Expression) -> SimpleExpression {
    match expression {
        Expression::If(if_expression) => SimpleExpression::If(simplify_if(if_expression)),
        Expression::InfixOperation(infix_operation) => {
            SimpleExpression::InfixOperation(simplify_infix_operation(infix_operation))
        }

        Expression::Select(select) => SimpleExpression::Select(simplify_select(select)),
        Expression::Call(call) => SimpleExpression::Call(simplify_call(call)),
        Expression::Closure(closure) => SimpleExpression::Closure(simplify_closure(closure)),
        Expression::StructApplication(struct_application) => {
            SimpleExpression::StructApplication(simplify_struct_application(struct_application))
        }

        Expression::Path(path) => SimpleExpression::Path(path.clone()),
        Expression::Identifier(identifier) => SimpleExpression::Identifier(identifier.clone()),
        Expression::Number(number) => SimpleExpression::Number(number.clone()),
        Expression::String(string) => SimpleExpression::String(string.clone()),
        Expression::PrefixOperation(prefix_operation) => {
            simplify_prefix_operation(prefix_operation)
        }

        Expression::SequentialBlock(block) => {
            SimpleExpression::SequentialBlock(simplify_block(block))
        }

        Expression::Unit(unit) => SimpleExpression::Unit(unit.clone()),
    }
}

fn simplify_function_definition(definition: &FunctionDefinition) -> SimpleFunctionDefinition {
    SimpleFunctionDefinition {
        name: definition.name.clone(),
        parameters: definition.parameters.clone(),
        return_type: definition.return_type.clone().unwrap_or(Type::Unit),
        body: simplify_block(&definition.body),
        position: definition.get_position(),
    }
}

fn simplify_if(if_expression: &If) -> SimpleIf {
    let simple_condition = simplify_expression(&if_expression.condition);
    let simple_then = simplify_block(&if_expression.body);
    let simple_else = match &if_expression.else_clause {
        Some(else_clause) => simplify_block(&else_clause.body),
        None => SimpleBlock {
            statements: Vec::new(),
            result: None,
            position: if_expression.get_position(),
        },
    };

    let else_block = match if_expression.else_if_clauses.as_slice() {
        [init @ .., last] => {
            let mut result = SimpleIf {
                condition: Box::new(simplify_expression(&last.condition)),
                then_block: simplify_block(&last.body),
                else_block: simple_else,
                position: Position(last.position.0.start..if_expression.position.0.end),
            };

            for else_if in init.iter().rev() {
                let result_position = result.get_position();

                result = SimpleIf {
                    condition: Box::new(simplify_expression(&else_if.condition)),
                    then_block: simplify_block(&else_if.body),
                    else_block: SimpleBlock {
                        statements: Vec::new(),
                        result: Some(Box::new(SimpleExpression::If(result))),
                        position: result_position,
                    },

                    position: Position(else_if.position.0.start..if_expression.position.0.end),
                };
            }

            let result_position = result.get_position();

            SimpleBlock {
                statements: Vec::new(),
                result: Some(Box::new(SimpleExpression::If(result))),
                position: result_position,
            }
        }

        [] => simple_else,
    };

    SimpleIf {
        condition: Box::new(simple_condition),
        then_block: simple_then,
        else_block,
        position: if_expression.get_position(),
    }
}

fn simplify_infix_operation(infix_operation: &InfixOperation) -> SimpleInfixOperation {
    SimpleInfixOperation {
        left: Box::new(simplify_expression(&infix_operation.left)),
        operator: infix_operation.operator,
        right: Box::new(simplify_expression(&infix_operation.right)),
        position: infix_operation.get_position(),
    }
}

fn simplify_prefix_operation(prefix_operation: &PrefixOperation) -> SimpleExpression {
    let simple_expression = Box::new(simplify_expression(&prefix_operation.expression));

    match prefix_operation.operator {
        PrefixOperator::Negate => SimpleExpression::InfixOperation(SimpleInfixOperation {
            left: simple_expression,
            operator: InfixOperator::Numeric(NumericInfixOperator::Multiplication),
            right: Box::new(SimpleExpression::Number(Number {
                value: -1,
                suffix: None,

                /*
                 * This `-1` is generated, so it doesn't exist in the source. But if it did, it'd be
                 * to the right of `prefix_operation`, separated by a `*` token. For that reason, we
                 * set its position to a zero-byte range to the right of `prefix_operation`.
                 */
                position: Position(
                    prefix_operation.position.0.start..prefix_operation.position.0.start,
                ),
            })),

            position: prefix_operation.get_position(),
        }),

        PrefixOperator::Not => SimpleExpression::PrefixOperation(SimplePrefixOperation {
            operator: SimplePrefixOperator::Not,
            expression: simple_expression,
            position: prefix_operation.get_position(),
        }),
    }
}

fn simplify_program(program: &Program) -> SimpleProgram {
    SimpleProgram {
        statements: program.statements.iter().map(simplify_statement).collect(),
        position: program.get_position(),
    }
}

fn simplify_select(select: &Select) -> SimpleSelect {
    SimpleSelect {
        left_hand_side: Box::new(simplify_expression(&select.left_hand_side)),
        right_hand_side: select.right_hand_side.clone(),
        position: select.get_position(),
    }
}

fn simplify_statement(statement: &Statement) -> SimpleStatement {
    match statement {
        Statement::Expression(expression) => {
            SimpleStatement::Expression(Box::new(simplify_expression(expression)))
        }

        Statement::StructDefinition(definition) => {
            SimpleStatement::StructDefinition(simplify_struct_definition(definition))
        }

        Statement::FunctionDefinition(definition) => {
            SimpleStatement::FunctionDefinition(simplify_function_definition(definition))
        }

        Statement::VariableDefinition(definition) => {
            SimpleStatement::VariableDefinition(simplify_variable_definition(definition))
        }

        Statement::NoOp { position } => SimpleStatement::NoOp {
            position: position.clone(),
        },
    }
}

fn simplify_struct_application(struct_application: &StructApplication) -> SimpleStructApplication {
    SimpleStructApplication {
        name: struct_application.name.clone(),
        fields: struct_application
            .fields
            .iter()
            .map(|field| SimpleStructApplicationField {
                name: field.name.clone(),
                value: Box::new(simplify_expression(&field.value)),
                position: field.get_position(),
            })
            .collect(),
        position: struct_application.get_position(),
    }
}

fn simplify_struct_definition(definition: &StructDefinition) -> SimpleStructDefinition {
    SimpleStructDefinition {
        name: definition.name.clone(),
        fields: definition.fields.clone(),
        methods: definition
            .methods
            .iter()
            .map(simplify_function_definition)
            .collect(),

        position: definition.get_position(),
    }
}

fn simplify_variable_definition(definition: &VariableDefinition) -> SimpleVariableDefinition {
    SimpleVariableDefinition {
        name: definition.name.clone(),
        value: Box::new(simplify_expression(&definition.value)),
        position: definition.get_position(),
    }
}
