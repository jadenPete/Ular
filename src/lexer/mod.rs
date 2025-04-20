pub mod token;

use crate::{
    lexer::token::{PositionedToken, Token},
    phase::Phase,
};
use core::str;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{multispace0, satisfy},
    combinator::{consumed, eof, map, recognize},
    error::ParseError,
    multi::many0,
    sequence::{delimited, tuple},
    AsChar, Compare, CompareResult, IResult, InputIter, InputLength, InputTake,
    InputTakeAtPosition, Needed, Offset, Slice,
};

use std::{
    ops::{RangeFrom, RangeTo},
    str::{CharIndices, Chars},
};

#[derive(Clone, Copy)]
struct PositionedSource<'a> {
    source: &'a str,
    index: usize,
}

impl<'a, A> Compare<A> for PositionedSource<'a>
where
    &'a str: Compare<A>,
{
    fn compare(&self, other: A) -> CompareResult {
        self.source.compare(other)
    }

    fn compare_no_case(&self, other: A) -> CompareResult {
        self.source.compare_no_case(other)
    }
}

impl<'a> InputIter for PositionedSource<'a> {
    type Item = char;
    type Iter = CharIndices<'a>;
    type IterElem = Chars<'a>;

    fn iter_indices(&self) -> Self::Iter {
        self.source.iter_indices()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.source.iter_elements()
    }

    fn position<A: Fn(Self::Item) -> bool>(&self, predicate: A) -> Option<usize> {
        self.source.position(predicate)
    }

    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        self.source.slice_index(count)
    }
}

impl<'a> InputLength for PositionedSource<'a> {
    fn input_len(&self) -> usize {
        self.source.len()
    }
}

impl<'a> InputTake for PositionedSource<'a> {
    fn take(&self, count: usize) -> Self {
        Self {
            source: self.source.take(count),
            index: self.index,
        }
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (left, right) = self.source.take_split(count);

        (
            Self {
                source: left,
                index: self.index,
            },
            Self {
                source: right,
                index: self.index + count,
            },
        )
    }
}

pub struct LexerPhase;

impl<'a> Phase<&'a str, Vec<PositionedToken>, nom::Err<nom::error::Error<&'a str>>> for LexerPhase {
    fn name() -> String {
        String::from("lexer")
    }

    fn execute(
        &self,
        input: &'a str,
    ) -> Result<Vec<PositionedToken>, nom::Err<nom::error::Error<&'a str>>> {
        lex_tokens(input).map(move |(_, tokens)| tokens)
    }
}

impl<'a> Offset for PositionedSource<'a> {
    fn offset(&self, second: &Self) -> usize {
        self.source.offset(second.source)
    }
}

impl<'a> Slice<RangeFrom<usize>> for PositionedSource<'a> {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        let range_start = range.start;

        Self {
            source: self.source.slice(range),
            index: self.index + range_start,
        }
    }
}

impl<'a> Slice<RangeTo<usize>> for PositionedSource<'a> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        Self {
            source: self.source.slice(range),
            index: self.index,
        }
    }
}

impl<'a> InputTakeAtPosition for PositionedSource<'a> {
    type Item = char;

    fn split_at_position<Predicate: Fn(Self::Item) -> bool, Error: ParseError<Self>>(
        &self,
        predicate: Predicate,
    ) -> IResult<Self, Self, Error> {
        match self.source.find(predicate) {
            Some(i) => Ok((
                Self {
                    source: &self.source[i..],
                    index: self.index + i,
                },
                Self {
                    source: &self.source[..i],
                    index: self.index,
                },
            )),

            None => Err(nom::Err::Incomplete(Needed::Unknown)),
        }
    }

    fn split_at_position1<Predicate: Fn(Self::Item) -> bool, Error: ParseError<Self>>(
        &self,
        predicate: Predicate,
        error_kind: nom::error::ErrorKind,
    ) -> IResult<Self, Self, Error> {
        match self.source.find(predicate) {
            Some(0) => Err(nom::Err::Error(Error::from_error_kind(*self, error_kind))),
            Some(i) => Ok((
                Self {
                    source: &self.source[i..],
                    index: self.index + i,
                },
                Self {
                    source: &self.source[..i],
                    index: self.index,
                },
            )),

            None => Err(nom::Err::Incomplete(Needed::Unknown)),
        }
    }

    fn split_at_position_complete<Predicate: Fn(Self::Item) -> bool, Error: ParseError<Self>>(
        &self,
        predicate: Predicate,
    ) -> IResult<Self, Self, Error> {
        Ok(self
            .split_at_position(predicate)
            .unwrap_or_else(|_: nom::Err<Error>| {
                (
                    Self {
                        source: "",
                        index: self.index + self.source.len(),
                    },
                    *self,
                )
            }))
    }

    fn split_at_position1_complete<Predicate: Fn(Self::Item) -> bool, Error: ParseError<Self>>(
        &self,
        predicate: Predicate,
        error_kind: nom::error::ErrorKind,
    ) -> IResult<Self, Self, Error> {
        match (self.source.find(predicate), self.source.is_empty()) {
            (Some(0), _) | (None, true) => {
                Err(nom::Err::Error(Error::from_error_kind(*self, error_kind)))
            }

            (Some(i), _) => Ok((
                Self {
                    source: &self.source[i..],
                    index: self.index + i,
                },
                Self {
                    source: &self.source[..i],
                    index: self.index,
                },
            )),

            (None, false) => Ok((
                Self {
                    source: "",
                    index: self.index + self.source.len(),
                },
                *self,
            )),
        }
    }
}

macro_rules! lexer_function {
    ($function_name: ident, $tag_string: literal, $token: expr) => {
        fn $function_name<'a>(
            input: PositionedSource<'a>,
        ) -> IResult<PositionedSource<'a>, PositionedToken> {
            map(recognize(tag($tag_string)), |consumed: PositionedSource| {
                PositionedToken {
                    token: $token,
                    start: consumed.index,
                    end: consumed.index + consumed.source.len(),
                }
            })(input)
        }
    };
}

lexer_function! { lex_comma, ",", Token::Comma }
lexer_function! { lex_definition, "=", Token::Definition }
lexer_function! { lex_i8_type, "i8", Token::I8Type }
lexer_function! { lex_i16_type, "i16", Token::I16Type }
lexer_function! { lex_i32_type, "i32", Token::I32Type }
lexer_function! { lex_i64_type, "i64", Token::I64Type }
lexer_function! { lex_u8_type, "u8", Token::U8Type }
lexer_function! { lex_u16_type, "u16", Token::U16Type }
lexer_function! { lex_u32_type, "u32", Token::U32Type }
lexer_function! { lex_u64_type, "u64", Token::U64Type }
lexer_function! { lex_if_keyword, "if", Token::IfKeyword }
lexer_function! { lex_else_keyword, "else", Token::ElseKeyword }
lexer_function! { lex_left_curly_bracket, "{", Token::LeftCurlyBracket }
lexer_function! { lex_right_curly_bracket, "}", Token::RightCurlyBracket }
lexer_function! { lex_left_parenthesis, "(", Token::LeftParenthesis }
lexer_function! { lex_right_parenthesis, ")", Token::RightParenthesis }
lexer_function! { lex_logical_and, "&&", Token::LogicalAnd }
lexer_function! { lex_logical_or, "||", Token::LogicalOr }
lexer_function! { lex_not, "!", Token::Not }
lexer_function! { lex_over, "/", Token::Over }
lexer_function! { lex_plus, "+", Token::Plus }
lexer_function! { lex_minus, "-", Token::Minus }
lexer_function! { lex_modulo, "%", Token::Modulo }
lexer_function! { lex_times, "*", Token::Times }
lexer_function! { lex_semicolon, ";", Token::Semicolon }

fn lex_identifier(input: PositionedSource) -> IResult<PositionedSource, PositionedToken> {
    map(
        recognize(tuple((
            satisfy(|character| character.is_alpha() || character == '_'),
            many0(satisfy(|character| {
                character.is_alphanumeric() || character == '_'
            })),
        ))),
        |consumed: PositionedSource<'_>| PositionedToken {
            token: Token::Identifier(String::from(consumed.source)),
            start: consumed.index,
            end: consumed.index + consumed.source.len(),
        },
    )(input)
}

fn lex_number(input: PositionedSource) -> IResult<PositionedSource, PositionedToken> {
    map(
        consumed(nom::character::complete::i128::<PositionedSource, _>),
        |(consumed, value)| PositionedToken {
            token: Token::Number(value),
            start: consumed.index,
            end: consumed.index + consumed.source.len(),
        },
    )(input)
}

fn lex_token(input: PositionedSource) -> IResult<PositionedSource, PositionedToken> {
    alt((
        alt((
            lex_comma,
            lex_definition,
            lex_i8_type,
            lex_i16_type,
            lex_i32_type,
            lex_i64_type,
            lex_u8_type,
            lex_u16_type,
            lex_u32_type,
            lex_u64_type,
            lex_if_keyword,
            lex_else_keyword,
            lex_identifier,
            lex_left_curly_bracket,
            lex_right_curly_bracket,
            lex_left_parenthesis,
            lex_right_parenthesis,
            lex_logical_and,
            lex_logical_or,
            lex_modulo,
            lex_not,
        )),
        alt((
            lex_number,
            // This needs to come after `lex_number` so signs aren't interpreted as operators
            lex_minus,
            lex_over,
            lex_plus,
            lex_semicolon,
            lex_times,
        )),
    ))(input)
}

fn lex_tokens_underlying(
    input: PositionedSource,
) -> IResult<PositionedSource, Vec<PositionedToken>> {
    map(
        tuple((many0(delimited(multispace0, lex_token, multispace0)), eof)),
        |(result, _)| result,
    )(input)
}

fn lex_tokens(input: &str) -> IResult<&str, Vec<PositionedToken>> {
    let (input, result) = lex_tokens_underlying(PositionedSource {
        source: input,
        index: 0,
    })
    .map_err(|error| {
        error.map(|error| nom::error::Error {
            input: error.input.source,
            code: error.code,
        })
    })?;

    Ok((input.source, result))
}
