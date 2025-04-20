pub mod program;

use crate::{
    lexer::token::{Token, Tokens},
    parser::program::{
        Block, Call, ElseClause, ElseIfClause, Expression, Identifier, If, InfixOperation,
        InfixOperator, Number, NumericType, PrefixOperation, PrefixOperator, Program, Statement,
        VariableDefinition,
    },
    phase::Phase,
};

use nom::{
    branch::alt,
    combinator::{eof, map, opt},
    error::{ErrorKind, ParseError},
    multi::{many0, separated_list1},
    sequence::{delimited, tuple},
    IResult, InputIter, Slice,
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

fn parse_expression(input: Tokens) -> IResult<Tokens, Expression> {
    parse_logical_or(input)
}

fn parse_logical_or(input: Tokens) -> IResult<Tokens, Expression> {
    alt((
        map(
            tuple((
                parse_logical_and,
                parse_token(Token::LogicalOr),
                parse_logical_or,
            )),
            |(left, _, right)| {
                Expression::InfixOperation(InfixOperation {
                    left: Box::new(left),
                    operator: InfixOperator::LogicalOr,
                    right: Box::new(right),
                })
            },
        ),
        parse_logical_and,
    ))(input)
}

fn parse_logical_and(input: Tokens) -> IResult<Tokens, Expression> {
    alt((
        map(
            tuple((parse_sum, parse_token(Token::LogicalAnd), parse_logical_and)),
            |(left, _, right)| {
                Expression::InfixOperation(InfixOperation {
                    left: Box::new(left),
                    operator: InfixOperator::LogicalAnd,
                    right: Box::new(right),
                })
            },
        ),
        parse_sum,
    ))(input)
}

fn parse_sum(input: Tokens) -> IResult<Tokens, Expression> {
    alt((
        map(
            tuple((
                parse_product,
                alt((
                    map(parse_token(Token::Plus), |_| InfixOperator::Addition),
                    map(parse_token(Token::Minus), |_| InfixOperator::Subtraction),
                )),
                parse_sum,
            )),
            |(left, operator, right)| {
                Expression::InfixOperation(InfixOperation {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                })
            },
        ),
        parse_product,
    ))(input)
}

fn parse_product(input: Tokens) -> IResult<Tokens, Expression> {
    alt((
        map(
            tuple((
                parse_prefix_operation,
                alt((
                    map(parse_token(Token::Times), |_| InfixOperator::Multiplication),
                    map(parse_token(Token::Over), |_| InfixOperator::Division),
                    map(parse_token(Token::Modulo), |_| InfixOperator::Modulo),
                )),
                parse_product,
            )),
            |(left, operator, right)| {
                Expression::InfixOperation(InfixOperation {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                })
            },
        ),
        parse_prefix_operation,
    ))(input)
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
        parse_if,
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
        parse_call,
    ))(input)
}

fn parse_call(input: Tokens) -> IResult<Tokens, Expression> {
    alt((
        map(
            tuple((
                parse_identifier,
                parse_token(Token::LeftParenthesis),
                separated_list1(parse_token(Token::Comma), parse_expression),
                parse_token(Token::RightParenthesis),
            )),
            |(function, _, arguments, _)| {
                Expression::Call(Call {
                    function,
                    arguments,
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

fn parse_number_type(input: Tokens) -> IResult<Tokens, NumericType> {
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

fn parse_number(input: Tokens) -> IResult<Tokens, Number> {
    map(
        tuple((parse_raw_number, opt(parse_number_type))),
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
