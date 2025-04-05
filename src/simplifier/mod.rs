pub mod simple_program;

use crate::{
    parser::program::{Block, Call, Expression, If, Infix, Program, Statement, VariableDefinition},
    phase::Phase,
    simplifier::simple_program::{
        SimpleBlock, SimpleCall, SimpleExpression, SimpleIf, SimpleInfix, SimpleProgram,
        SimpleStatement, SimpleVariableDefinition,
    },
};

pub struct SimplifierPhase;

impl Phase<&Program, SimpleProgram, ()> for SimplifierPhase {
    fn name() -> String {
        String::from("simplifier")
    }

    fn execute(&self, program: &Program) -> Result<SimpleProgram, ()> {
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
    }
}

fn simplify_call(call: &Call) -> SimpleCall {
    SimpleCall {
        function: call.function.clone(),
        arguments: call.arguments.iter().map(simplify_expression).collect(),
    }
}

fn simplify_expression(expression: &Expression) -> SimpleExpression {
    match expression {
        Expression::If(if_expression) => SimpleExpression::If(simplify_if(if_expression)),
        Expression::Infix(infix) => SimpleExpression::Infix(simplify_infix(infix)),
        Expression::Call(call) => SimpleExpression::Call(simplify_call(call)),
        Expression::Identifier(identifier) => SimpleExpression::Identifier(identifier.clone()),
        Expression::Number(number) => SimpleExpression::Number(number.clone()),
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
        },
    };

    match if_expression.else_if_clauses.as_slice() {
        [head, tail @ ..] => {
            let mut result = SimpleIf {
                condition: Box::new(simplify_expression(&head.condition)),
                then_block: simplify_block(&head.body),
                else_block: simple_else,
            };

            for else_if in tail.iter().rev() {
                result = SimpleIf {
                    condition: Box::new(simplify_expression(&else_if.condition)),
                    then_block: simplify_block(&else_if.body),
                    else_block: SimpleBlock {
                        statements: Vec::new(),
                        result: Some(Box::new(SimpleExpression::If(result))),
                    },
                };
            }

            result
        }

        [] => SimpleIf {
            condition: Box::new(simple_condition),
            then_block: simple_then,
            else_block: simple_else,
        },
    }
}

pub fn simplify_infix(infix: &Infix) -> SimpleInfix {
    SimpleInfix {
        left: Box::new(simplify_expression(&infix.left)),
        operator: infix.operator,
        right: Box::new(simplify_expression(&infix.right)),
    }
}

fn simplify_program(program: &Program) -> SimpleProgram {
    SimpleProgram {
        statements: program.statements.iter().map(simplify_statement).collect(),
    }
}

fn simplify_statement(statement: &Statement) -> SimpleStatement {
    match statement {
        Statement::Expression(expression) => {
            SimpleStatement::Expression(simplify_expression(expression))
        }

        Statement::VariableDefinition(definition) => {
            SimpleStatement::VariableDefinition(simplify_variable_definition(definition))
        }

        Statement::NoOp => SimpleStatement::NoOp,
    }
}

fn simplify_variable_definition(definition: &VariableDefinition) -> SimpleVariableDefinition {
    SimpleVariableDefinition {
        name: definition.name.clone(),
        value: simplify_expression(&definition.value),
    }
}
