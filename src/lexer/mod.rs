pub mod token;

use crate::{
    error_reporting::Position,
    lexer::token::{PositionedToken, Token},
    phase::Phase,
};
use core::str;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{multispace0, satisfy},
    combinator::{consumed, eof, map, recognize},
    error::{ErrorKind, ParseError},
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

impl<'a> PositionedSource<'a> {
    fn to_positioned_token(&self, token: Token) -> PositionedToken {
        PositionedToken {
            token,
            position: Position(self.index..self.index + self.source.len()),
        }
    }
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
        let (remaining, consumed) = self.source.take_split(count);

        (
            Self {
                source: remaining,
                index: self.index + count,
            },
            Self {
                source: consumed,
                index: self.index,
            },
        )
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

fn lex_identifier_like(input: PositionedSource) -> IResult<PositionedSource, PositionedSource> {
    recognize(tuple((
        satisfy(|character| character.is_alpha() || character == '_'),
        many0(satisfy(|character| {
            character.is_alphanumeric() || character == '_'
        })),
    )))(input)
}

fn lex_identifier(input: PositionedSource) -> IResult<PositionedSource, PositionedToken> {
    map(lex_identifier_like, |consumed: PositionedSource<'_>| {
        consumed.to_positioned_token(Token::Identifier(String::from(consumed.source)))
    })(input)
}

/*
 * Both this function and `lex_static_token` match against a provided string.
 *
 * The difference between this function and `lex_static_token` is that this one pulls an entire
 * "identifier-like" string instead of just matching on `content` so that substrings like "boolean"
 * aren't falsly interpreted as a keyword ("bool") plus an identifier ("ean").
 */
fn lex_keyword<'a>(
    content: &'a str,
    token: Token,
) -> impl Fn(PositionedSource<'a>) -> IResult<PositionedSource<'a>, PositionedToken> {
    move |input| {
        /*
         * We pull an entire "identifier-like" string instead of just matching on `content` so that
         * substrings like "boolean" aren't falsly interpreted as a keyword ("bool") plus an
         * identifier ("ean").
         */
        let (remaining, consumed) = lex_identifier_like(input)?;

        if consumed.source == content {
            Ok((remaining, consumed.to_positioned_token(token.clone())))
        } else {
            Err(nom::Err::Error(nom::error::Error::from_error_kind(
                consumed,
                ErrorKind::Tag,
            )))
        }
    }
}

fn lex_number(input: PositionedSource) -> IResult<PositionedSource, PositionedToken> {
    map(
        consumed(nom::character::complete::i128::<PositionedSource, _>),
        |(consumed, value)| consumed.to_positioned_token(Token::Number(value)),
    )(input)
}

/*
 * Both this function and `lex_keyword` match against a provided string.
 *
 * The difference between this function and `lex_keyword` is that this one only matches against
 * `content` and doesn't pull any additional characters before validating that the characters
 * consumed match `content`.
 */
fn lex_static_token<'a>(
    content: &'a str,
    token: Token,
) -> impl FnMut(PositionedSource<'a>) -> IResult<PositionedSource<'a>, PositionedToken> {
    map(
        recognize(tag(content)),
        move |consumed: PositionedSource| consumed.to_positioned_token(token.clone()),
    )
}

fn lex_token(input: PositionedSource) -> IResult<PositionedSource, PositionedToken> {
    alt((
        alt((
            lex_static_token("=>", Token::Arrow),
            lex_static_token(",", Token::Comma),
            lex_static_token("=", Token::Definition),
            lex_keyword("fn", Token::FnKeyword),
            lex_keyword("i8", Token::I8Type),
            lex_keyword("i16", Token::I16Type),
            lex_keyword("i32", Token::I32Type),
            lex_keyword("i64", Token::I64Type),
            lex_keyword("u8", Token::U8Type),
            lex_keyword("u16", Token::U16Type),
            lex_keyword("u32", Token::U32Type),
            lex_keyword("u64", Token::U64Type),
            lex_keyword("bool", Token::BoolType),
            lex_keyword("unit", Token::UnitType),
            lex_keyword("if", Token::IfKeyword),
            lex_keyword("else", Token::ElseKeyword),
            lex_identifier,
            lex_static_token("{", Token::LeftCurlyBracket),
            lex_static_token("}", Token::RightCurlyBracket),
            lex_static_token("(", Token::LeftParenthesis),
            lex_static_token(")", Token::RightParenthesis),
        )),
        alt((
            lex_static_token("&&", Token::LogicalAnd),
            lex_static_token("||", Token::LogicalOr),
            lex_static_token("%", Token::Modulo),
            lex_static_token("!", Token::Not),
            lex_number,
            // This needs to come after `lex_number` so signs aren't interpreted as operators
            lex_static_token("-", Token::Minus),
            lex_static_token("/", Token::Over),
            lex_static_token("+", Token::Plus),
            lex_static_token("*", Token::Times),
            lex_static_token(";", Token::Semicolon),
            lex_static_token(":", Token::TypeAnnotation),
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
