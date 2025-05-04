pub mod program;
pub mod type_;

use crate::{
    lexer::token::{Token, Tokens},
    parser::{
        program::{
            Block, Call, ElseClause, ElseIfClause, Expression, FunctionDefinition, Identifier, If,
            InfixOperation, InfixOperator, Number, Parameter, PrefixOperation, PrefixOperator,
            Program, Statement, VariableDefinition,
        },
        type_::{FunctionType, NumericType, Type},
    },
    phase::Phase,
};

use nom::{
    branch::alt,
    combinator::{eof, map, opt},
    error::{ErrorKind, ParseError},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, tuple},
    IResult, InputIter, Parser, Slice,
};

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
            if actual == token {
                Ok((consumed, actual))
            } else {
                Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    ErrorKind::IsNot,
                )))
            }
        })
    }
}

fn parse_program(input: Tokens) -> IResult<Tokens, Program> {
    map(tuple((many0(parse_statement), eof)), |(statements, _)| {
        Program { statements }
    })(input)
}

fn parse_statement(input: Tokens) -> IResult<Tokens, Statement> {
    alt((
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
        map(parse_token(Token::Semicolon), |_| Statement::NoOp),
    ))(input)
}

fn parse_variable_definition(input: Tokens) -> IResult<Tokens, VariableDefinition> {
    map(
        tuple((
            parse_identifier,
            parse_token(Token::Definition),
            parse_expression,
            parse_token(Token::Semicolon),
        )),
        |(name, _, value, _)| VariableDefinition { name, value },
    )(input)
}

fn parse_function_definition(input: Tokens) -> IResult<Tokens, FunctionDefinition> {
    map(
        tuple((
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
        )),
        |(_, name, _, parameters, _, return_type, body)| FunctionDefinition {
            name,
            parameters,
            return_type,
            body,
        },
    )(input)
}

fn parse_parameter(input: Tokens) -> IResult<Tokens, Parameter> {
    map(
        tuple((
            parse_identifier,
            parse_token(Token::TypeAnnotation),
            parse_type,
        )),
        |(name, _, type_)| Parameter { name, type_ },
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
        tuple((
            parse_token(Token::LeftCurlyBracket),
            many0(parse_statement),
            opt(parse_expression),
            parse_token(Token::RightCurlyBracket),
        )),
        |(_, statements, result, _)| Block {
            statements,
            result: result.map(Box::new),
        },
    )(input)
}

fn parse_expression(input: Tokens) -> IResult<Tokens, Expression> {
    parse_logical_or(input)
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
                    result = Expression::InfixOperation(InfixOperation {
                        left: Box::new(result),
                        operator,
                        right: Box::new(right),
                    });
                }

                result
            },
        )(input)
    }
}

fn parse_logical_or(input: Tokens) -> IResult<Tokens, Expression> {
    parse_infix_operation(parse_logical_and, || {
        map(parse_token(Token::LogicalOr), |_| InfixOperator::LogicalOr)
    })(input)
}

fn parse_logical_and(input: Tokens) -> IResult<Tokens, Expression> {
    parse_infix_operation(parse_sum, || {
        map(parse_token(Token::LogicalAnd), |_| {
            InfixOperator::LogicalAnd
        })
    })(input)
}

fn parse_sum(input: Tokens) -> IResult<Tokens, Expression> {
    parse_infix_operation(parse_product, || {
        alt((
            map(parse_token(Token::Plus), |_| InfixOperator::Addition),
            map(parse_token(Token::Minus), |_| InfixOperator::Subtraction),
        ))
    })(input)
}

fn parse_product(input: Tokens) -> IResult<Tokens, Expression> {
    parse_infix_operation(parse_prefix_operation, || {
        alt((
            map(parse_token(Token::Times), |_| InfixOperator::Multiplication),
            map(parse_token(Token::Over), |_| InfixOperator::Division),
            map(parse_token(Token::Modulo), |_| InfixOperator::Modulo),
        ))
    })(input)
}

fn parse_prefix_operation(input: Tokens) -> IResult<Tokens, Expression> {
    alt((
        map(
            tuple((
                alt((
                    map(parse_token(Token::Minus), |_| PrefixOperator::Negate),
                    map(parse_token(Token::Not), |_| PrefixOperator::Not),
                )),
                parse_prefix_operation,
            )),
            |(operator, expression)| {
                Expression::PrefixOperation(PrefixOperation {
                    operator,
                    expression: Box::new(expression),
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
                many1(map(
                    tuple((
                        parse_token(Token::LeftParenthesis),
                        separated_list0(parse_token(Token::Comma), parse_expression),
                        parse_token(Token::RightParenthesis),
                    )),
                    |(_, arguments, _)| arguments,
                )),
            )),
            |(function, argument_lists)| {
                let mut result = function;

                for argument_list in argument_lists {
                    result = Expression::Call(Call {
                        function: Box::new(result),
                        arguments: argument_list,
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
            parse_token(Token::IfKeyword),
            parse_expression,
            parse_block,
        )),
        |(_, _, condition, body)| ElseIfClause {
            condition: Box::new(condition),
            body,
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
            tuple((
                parse_token(Token::IfKeyword),
                parse_expression,
                parse_block,
                many0(parse_else_if_clause),
                opt(parse_else_clause),
            )),
            |(_, condition, body, else_if_clauses, else_clause)| {
                Expression::If(If {
                    condition: Box::new(condition),
                    body,
                    else_if_clauses,
                    else_clause,
                })
            },
        ),
        parse_primary,
    ))(input)
}

fn parse_primary(input: Tokens) -> IResult<Tokens, Expression> {
    alt((
        map(parse_identifier, |identifier| {
            Expression::Identifier(identifier)
        }),
        map(parse_number, |number| Expression::Number(number)),
        delimited(
            parse_token(Token::LeftParenthesis),
            parse_expression,
            parse_token(Token::RightParenthesis),
        ),
    ))(input)
}

fn parse_identifier(input: Tokens) -> IResult<Tokens, Identifier> {
    let (remaining, token) = take_token(input)?;

    match token {
        Token::Identifier(value) => Ok((remaining, Identifier(value))),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::IsNot,
        ))),
    }
}

fn parse_raw_number(input: Tokens) -> IResult<Tokens, i128> {
    let (remaining, token) = take_token(input)?;

    match token {
        Token::Number(value) => Ok((remaining, value)),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::IsNot,
        ))),
    }
}

fn parse_number(input: Tokens) -> IResult<Tokens, Number> {
    map(
        tuple((parse_raw_number, opt(parse_numeric_type))),
        |(value, suffix)| Number { value, suffix },
    )(input)
}

fn take_token(input: Tokens) -> IResult<Tokens, Token> {
    match input.iter_elements().next() {
        Some(token) => Ok((input.slice(1..), token.token.clone())),
        None => Err(nom::Err::Error(nom::error::Error::from_error_kind(
            input,
            ErrorKind::Eof,
        ))),
    }
}
