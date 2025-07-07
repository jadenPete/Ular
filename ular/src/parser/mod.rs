pub mod program;
pub mod type_;

use crate::{
    error_reporting::Position,
    lexer::token::{PositionedToken, Token, Tokens},
    parser::{
        program::{
            Block, Call, ElseClause, ElseIfClause, Expression, FunctionDefinition, Identifier, If,
            InfixOperation, InfixOperator, LogicalInfixOperator, Number, NumericInfixOperator,
            Parameter, PrefixOperation, PrefixOperator, Program, Statement, StructApplication,
            StructApplicationField, StructDefinition, StructDefinitionField, Unit,
            UniversalInfixOperator, VariableDefinition,
        },
        type_::{FunctionType, NumericType, Type},
    },
    phase::Phase,
};

use nom::{
    branch::alt,
    combinator::{consumed, eof, map, opt},
    error::{ErrorKind, ParseError},
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{delimited, tuple},
    IResult, InputIter, Parser, Slice,
};
use program::Node;

pub struct ParserPhase;

impl<'a> Phase<Tokens<'a>, Program, nom::Err<nom::error::Error<Tokens<'a>>>> for ParserPhase {
    fn name() -> String {
        String::from("parser")
    }

    fn execute(
        &self,
        tokens: Tokens<'a>,
    ) -> Result<Program, nom::Err<nom::error::Error<Tokens<'a>>>> {
        parse_program(tokens).map(|(_, program)| program)
    }
}

fn parse_token(token: Token) -> impl Fn(Tokens) -> IResult<Tokens, Token> {
    move |input| {
        take_token(input).and_then(|(consumed, actual)| {
            if actual.token == token {
                Ok((consumed, actual.token))
            } else {
                Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    ErrorKind::IsNot,
                )))
            }
        })
    }
}

fn positioned<'a, O, F: Parser<Tokens<'a>, O, nom::error::Error<Tokens<'a>>>>(
    parser: F,
) -> impl FnMut(Tokens<'a>) -> IResult<Tokens<'a>, (Position, O)> {
    map(consumed(parser), |(consumed, output)| {
        (consumed.get_position(), output)
    })
}

fn parse_program(input: Tokens) -> IResult<Tokens, Program> {
    map(
        positioned(tuple((many0(parse_statement), eof))),
        |(position, (statements, _))| Program {
            statements,
            position,
        },
    )(input)
}

fn parse_statement(input: Tokens) -> IResult<Tokens, Statement> {
    alt((
        map(parse_struct_definition, |definition| {
            Statement::StructDefinition(definition)
        }),
        map(parse_variable_definition, |definition| {
            Statement::VariableDefinition(definition)
        }),
        map(parse_function_definition, |definition| {
            Statement::FunctionDefinition(definition)
        }),
        map(
            tuple((parse_expression, parse_token(Token::Semicolon))),
            |(expression, _)| Statement::Expression(expression),
        ),
        map(
            positioned(parse_token(Token::Semicolon)),
            |(position, _)| Statement::NoOp { position },
        ),
    ))(input)
}

fn parse_struct_definition(input: Tokens) -> IResult<Tokens, StructDefinition> {
    map(
        positioned(tuple((
            parse_token(Token::StructKeyword),
            parse_identifier,
            parse_token(Token::LeftCurlyBracket),
            separated_list1(parse_token(Token::Comma), parse_struct_definition_field),
            parse_token(Token::RightCurlyBracket),
        ))),
        |(position, (_, name, _, fields, _))| StructDefinition {
            name,
            fields,
            position,
        },
    )(input)
}

fn parse_struct_definition_field(input: Tokens) -> IResult<Tokens, StructDefinitionField> {
    map(
        tuple((
            parse_identifier,
            parse_token(Token::TypeAnnotation),
            parse_type,
        )),
        |(name, _, type_)| StructDefinitionField { name, type_ },
    )(input)
}

fn parse_variable_definition(input: Tokens) -> IResult<Tokens, VariableDefinition> {
    map(
        positioned(tuple((
            parse_identifier,
            parse_token(Token::Definition),
            parse_expression,
            parse_token(Token::Semicolon),
        ))),
        |(position, (name, _, value, _))| VariableDefinition {
            name,
            value,
            position,
        },
    )(input)
}

fn parse_function_definition(input: Tokens) -> IResult<Tokens, FunctionDefinition> {
    map(
        positioned(tuple((
            parse_token(Token::FnKeyword),
            parse_identifier,
            parse_token(Token::LeftParenthesis),
            separated_list0(parse_token(Token::Comma), parse_parameter),
            parse_token(Token::RightParenthesis),
            opt(map(
                tuple((parse_token(Token::TypeAnnotation), parse_type)),
                |(_, return_type)| return_type,
            )),
            parse_block,
        ))),
        |(position, (_, name, _, parameters, _, return_type, body))| FunctionDefinition {
            name,
            parameters,
            return_type,
            body,
            position,
        },
    )(input)
}

fn parse_parameter(input: Tokens) -> IResult<Tokens, Parameter> {
    map(
        positioned(tuple((
            parse_identifier,
            parse_token(Token::TypeAnnotation),
            parse_type,
        ))),
        |(position, (name, _, type_))| Parameter {
            name,
            type_,
            position,
        },
    )(input)
}

fn parse_type(input: Tokens) -> IResult<Tokens, Type> {
    parse_function_type(input)
}

fn parse_function_type(input: Tokens) -> IResult<Tokens, Type> {
    alt((
        map(
            tuple((
                alt((
                    map(parse_primary_type, |parameter| vec![parameter]),
                    map(
                        tuple((
                            parse_token(Token::LeftParenthesis),
                            separated_list0(parse_token(Token::Comma), parse_type),
                            parse_token(Token::RightParenthesis),
                        )),
                        |(_, parameters, _)| parameters,
                    ),
                )),
                parse_token(Token::Arrow),
                parse_type,
            )),
            |(parameters, _, return_type)| {
                Type::Function(FunctionType {
                    parameters,
                    return_type: Box::new(return_type),
                })
            },
        ),
        parse_primary_type,
    ))(input)
}

fn parse_primary_type(input: Tokens) -> IResult<Tokens, Type> {
    alt((
        map(parse_numeric_type, |numeric_type| {
            Type::Numeric(numeric_type)
        }),
        map(parse_token(Token::BoolType), |_| Type::Bool),
        map(parse_token(Token::UnitType), |_| Type::Unit),
    ))(input)
}

fn parse_numeric_type(input: Tokens) -> IResult<Tokens, NumericType> {
    alt((
        map(parse_token(Token::I8Type), |_| NumericType::I8),
        map(parse_token(Token::I16Type), |_| NumericType::I16),
        map(parse_token(Token::I32Type), |_| NumericType::I32),
        map(parse_token(Token::I64Type), |_| NumericType::I64),
        map(parse_token(Token::U8Type), |_| NumericType::U8),
        map(parse_token(Token::U16Type), |_| NumericType::U16),
        map(parse_token(Token::U32Type), |_| NumericType::U32),
        map(parse_token(Token::U64Type), |_| NumericType::U64),
    ))(input)
}

fn parse_block(input: Tokens) -> IResult<Tokens, Block> {
    map(
        positioned(tuple((
            parse_token(Token::LeftCurlyBracket),
            many0(parse_statement),
            opt(parse_expression),
            parse_token(Token::RightCurlyBracket),
        ))),
        |(position, (_, statements, result, _))| Block {
            statements,
            result: result.map(Box::new),
            position,
        },
    )(input)
}

fn parse_expression(input: Tokens) -> IResult<Tokens, Expression> {
    parse_comparison(input)
}

fn parse_infix_operation<
    'a,
    Term: Parser<Tokens<'a>, Expression, nom::error::Error<Tokens<'a>>> + Copy,
    Operator: Parser<Tokens<'a>, InfixOperator, nom::error::Error<Tokens<'a>>>,
    /*
     * This is needed because many of the parsers returned by nom's combinators don't implement
     * `Copy` or `Clone`:
     * https://github.com/rust-bakery/nom/issues/1492
     */
    OperatorGenerator: Fn() -> Operator,
>(
    parse_term: Term,
    parse_operator: OperatorGenerator,
) -> impl FnMut(Tokens<'a>) -> IResult<Tokens<'a>, Expression> {
    move |input| {
        map(
            /*
             * nom is left-associative (meaning it evaluates from left to right), so if we parsed
             * infix operations like this:
             *
             * ```
             * operation = term operator operation;
             * ```
             *
             * then our left-associative operations would be evaluated as if they were
             * right-associative (from right to left). To get around this, we employ a classic
             * parsing trick and parse operators like this:
             *
             * ```
             * operation = term (operator operation)*;
             * ```
             */
            tuple((parse_term, many0(tuple((parse_operator(), parse_term))))),
            |(left, operations)| {
                let mut result = left;

                for (operator, right) in operations {
                    let position =
                        Position(result.get_position().0.start..right.get_position().0.end);

                    result = Expression::InfixOperation(InfixOperation {
                        left: Box::new(result),
                        operator,
                        right: Box::new(right),
                        position,
                    });
                }

                result
            },
        )(input)
    }
}

fn parse_comparison(input: Tokens) -> IResult<Tokens, Expression> {
    alt((
        map(
            tuple((
                parse_logical_or,
                alt((
                    map(parse_token(Token::EqualComparison), |_| {
                        InfixOperator::Universal(UniversalInfixOperator::EqualComparison)
                    }),
                    map(parse_token(Token::LessThan), |_| {
                        InfixOperator::Numeric(NumericInfixOperator::LessThan)
                    }),
                    map(parse_token(Token::LessThanOrEqual), |_| {
                        InfixOperator::Numeric(NumericInfixOperator::LessThanOrEqual)
                    }),
                    map(parse_token(Token::GreaterThan), |_| {
                        InfixOperator::Numeric(NumericInfixOperator::GreaterThan)
                    }),
                    map(parse_token(Token::GreaterThanOrEqual), |_| {
                        InfixOperator::Numeric(NumericInfixOperator::GreaterThanOrEqual)
                    }),
                    map(parse_token(Token::UnequalComparison), |_| {
                        InfixOperator::Universal(UniversalInfixOperator::UnequalComparison)
                    }),
                )),
                parse_logical_or,
            )),
            |(left, operator, right)| {
                let position = Position(left.get_position().0.start..right.get_position().0.end);

                Expression::InfixOperation(InfixOperation {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                    position,
                })
            },
        ),
        parse_logical_or,
    ))(input)
}

fn parse_logical_or(input: Tokens) -> IResult<Tokens, Expression> {
    parse_infix_operation(parse_logical_and, || {
        map(parse_token(Token::LogicalOr), |_| {
            InfixOperator::Logical(LogicalInfixOperator::LogicalOr)
        })
    })(input)
}

fn parse_logical_and(input: Tokens) -> IResult<Tokens, Expression> {
    parse_infix_operation(parse_sum, || {
        map(parse_token(Token::LogicalAnd), |_| {
            InfixOperator::Logical(LogicalInfixOperator::LogicalAnd)
        })
    })(input)
}

fn parse_sum(input: Tokens) -> IResult<Tokens, Expression> {
    parse_infix_operation(parse_product, || {
        alt((
            map(parse_token(Token::Plus), |_| {
                InfixOperator::Numeric(NumericInfixOperator::Addition)
            }),
            map(parse_token(Token::Minus), |_| {
                InfixOperator::Numeric(NumericInfixOperator::Subtraction)
            }),
        ))
    })(input)
}

fn parse_product(input: Tokens) -> IResult<Tokens, Expression> {
    parse_infix_operation(parse_prefix_operation, || {
        alt((
            map(parse_token(Token::Times), |_| {
                InfixOperator::Numeric(NumericInfixOperator::Multiplication)
            }),
            map(parse_token(Token::Over), |_| {
                InfixOperator::Numeric(NumericInfixOperator::Division)
            }),
            map(parse_token(Token::Modulo), |_| {
                InfixOperator::Numeric(NumericInfixOperator::Modulo)
            }),
        ))
    })(input)
}

fn parse_prefix_operation(input: Tokens) -> IResult<Tokens, Expression> {
    alt((
        map(
            positioned(tuple((
                alt((
                    map(parse_token(Token::Minus), |_| PrefixOperator::Negate),
                    map(parse_token(Token::Not), |_| PrefixOperator::Not),
                )),
                parse_prefix_operation,
            ))),
            |(position, (operator, expression))| {
                Expression::PrefixOperation(PrefixOperation {
                    operator,
                    expression: Box::new(expression),
                    position,
                })
            },
        ),
        parse_call,
    ))(input)
}

fn parse_call(input: Tokens) -> IResult<Tokens, Expression> {
    alt((
        map(
            tuple((
                parse_if,
                many1(positioned(map(
                    tuple((
                        parse_token(Token::LeftParenthesis),
                        separated_list0(parse_token(Token::Comma), parse_expression),
                        parse_token(Token::RightParenthesis),
                    )),
                    |(_, arguments, _)| arguments,
                ))),
            )),
            |(function, argument_lists)| {
                let mut result = function;

                for (argument_list_position, argument_list) in argument_lists {
                    let position =
                        Position(result.get_position().0.start..argument_list_position.0.end);

                    result = Expression::Call(Call {
                        function: Box::new(result),
                        arguments: argument_list,
                        position,
                    });
                }

                result
            },
        ),
        parse_if,
    ))(input)
}

fn parse_else_if_clause(input: Tokens) -> IResult<Tokens, ElseIfClause> {
    map(
        tuple((
            parse_token(Token::ElseKeyword),
            positioned(tuple((
                parse_token(Token::IfKeyword),
                parse_expression,
                parse_block,
            ))),
        )),
        |(_, (position, (_, condition, body)))| ElseIfClause {
            condition: Box::new(condition),
            body,
            position,
        },
    )(input)
}

fn parse_else_clause(input: Tokens) -> IResult<Tokens, ElseClause> {
    map(
        tuple((parse_token(Token::ElseKeyword), parse_block)),
        |(_, body)| ElseClause { body },
    )(input)
}

fn parse_if(input: Tokens) -> IResult<Tokens, Expression> {
    alt((
        map(
            positioned(tuple((
                parse_token(Token::IfKeyword),
                parse_expression,
                parse_block,
                many0(parse_else_if_clause),
                opt(parse_else_clause),
            ))),
            |(position, (_, condition, body, else_if_clauses, else_clause))| {
                Expression::If(If {
                    condition: Box::new(condition),
                    body,
                    else_if_clauses,
                    else_clause,
                    position,
                })
            },
        ),
        parse_primary,
    ))(input)
}

fn parse_primary(input: Tokens) -> IResult<Tokens, Expression> {
    alt((
        map(parse_struct_application, |application| {
            Expression::StructApplication(application)
        }),
        map(parse_identifier, |identifier| {
            Expression::Identifier(identifier)
        }),
        map(parse_number, Expression::Number),
        map(positioned(parse_token(Token::UnitType)), |(position, _)| {
            Expression::Unit(Unit { position })
        }),
        delimited(
            parse_token(Token::LeftParenthesis),
            parse_expression,
            parse_token(Token::RightParenthesis),
        ),
        map(parse_sequential_block, |block| {
            Expression::SequentialBlock(block)
        }),
    ))(input)
}

fn parse_struct_application(input: Tokens) -> IResult<Tokens, StructApplication> {
    map(
        positioned(tuple((
            parse_identifier,
            parse_token(Token::LeftCurlyBracket),
            separated_list1(parse_token(Token::Comma), parse_struct_application_field),
            parse_token(Token::RightCurlyBracket),
        ))),
        |(position, (name, _, fields, _))| StructApplication {
            name,
            fields,
            position,
        },
    )(input)
}

fn parse_struct_application_field(input: Tokens) -> IResult<Tokens, StructApplicationField> {
    map(
        tuple((
            parse_identifier,
            parse_token(Token::TypeAnnotation),
            parse_expression,
        )),
        |(name, _, value)| StructApplicationField { name, value },
    )(input)
}

fn parse_identifier(input: Tokens) -> IResult<Tokens, Identifier> {
    let (remaining, token) = take_token(input)?;

    match token.token {
        Token::Identifier(value) => Ok((
            remaining,
            Identifier {
                value,
                position: token.position,
            },
        )),

        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::IsNot,
        ))),
    }
}

fn parse_raw_number(input: Tokens) -> IResult<Tokens, i128> {
    let (remaining, token) = take_token(input)?;

    match token.token {
        Token::Number(value) => Ok((remaining, value)),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::IsNot,
        ))),
    }
}

fn parse_number(input: Tokens) -> IResult<Tokens, Number> {
    map(
        positioned(tuple((parse_raw_number, opt(parse_numeric_type)))),
        |(position, (value, suffix))| Number {
            value,
            suffix,
            position,
        },
    )(input)
}

fn parse_sequential_block(input: Tokens) -> IResult<Tokens, Block> {
    map(
        positioned(tuple((parse_token(Token::SeqKeyword), parse_block))),
        |(position, (_, block))| Block { position, ..block },
    )(input)
}

fn take_token(input: Tokens) -> IResult<Tokens, PositionedToken> {
    match input.iter_elements().next() {
        Some(token) => Ok((input.slice(1..), token.clone())),
        None => Err(nom::Err::Error(nom::error::Error::from_error_kind(
            input,
            ErrorKind::Eof,
        ))),
    }
}
