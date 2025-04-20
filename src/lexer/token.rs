use nom::{InputIter, InputLength, Needed, Slice};
use std::{
    fmt::{Debug, Formatter},
    iter::Enumerate,
    ops::{Index, RangeFrom},
    slice::SliceIndex,
};

/// Grammer:
/// ```ebnf
/// COMMA = ",";
/// DEFINITION = ":=";
/// I8_TYPE = "i8";
/// I16_TYPE = "i16";
/// I32_TYPE = "i32";
/// I64_TYPE = "i64";
/// U8_TYPE = "u8";
/// U16_TYPE = "u8";
/// U32_TYPE = "u16";
/// U64_TYPE = "u64";
/// IF_KEYWORD = "if";
/// ELSE_KEYWORD = "else";
/// IDENTIFIER = ?[a-zA-Z_][a-zA-Z0-9_]*?;
/// LEFT_CURLY_BRACKET = "{";
/// RIGHT_CURLY_BRACKET = "}";
/// LEFT_PARENTHESIS = "(";
/// RIGHT_PARENTHESIS = ")";
/// LOGICAL_AND = "&&";
/// LOGICAL_OR = "||";
/// OVER = "/";
/// PLUS = "+";
/// MINUS = "-";
/// MODULO = "%";
/// NUMBER = ?[+-]?[0-9]+?;
/// TIMES = "*";
/// SEMICOLON = ";";
/// ```
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Comma,
    Definition,
    I8Type,
    I16Type,
    I32Type,
    I64Type,
    U8Type,
    U16Type,
    U32Type,
    U64Type,
    IfKeyword,
    ElseKeyword,
    Identifier(String),
    LeftCurlyBracket,
    RightCurlyBracket,
    LeftParenthesis,
    RightParenthesis,
    LogicalAnd,
    LogicalOr,
    Over,
    Plus,
    Minus,
    Modulo,
    Number(i128),
    Times,
    Semicolon,
}

#[derive(Debug)]
pub struct PositionedToken {
    pub token: Token,
    pub start: usize,
    pub end: usize,
}

#[derive(Copy, Clone)]
pub struct Tokens<'a> {
    pub tokens: &'a [PositionedToken],
    pub source: &'a str,
}

impl<'a> Debug for Tokens<'a> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        let source_slice = match (self.tokens.first(), self.tokens.last()) {
            (Some(first_token), Some(last_token)) => {
                &self.source[first_token.start..last_token.end]
            }
            _ => self.source,
        };

        source_slice.fmt(formatter)
    }
}

impl<'a, A: SliceIndex<[PositionedToken]>> Index<A> for Tokens<'a> {
    type Output = A::Output;

    fn index(&self, index: A) -> &Self::Output {
        &self.tokens[index]
    }
}

impl<'a> InputIter for Tokens<'a> {
    type Item = &'a PositionedToken;
    type Iter = Enumerate<std::slice::Iter<'a, PositionedToken>>;
    type IterElem = std::slice::Iter<'a, PositionedToken>;

    fn iter_indices(&self) -> Self::Iter {
        self.tokens.iter().enumerate()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.tokens.iter()
    }

    fn position<A: Fn(Self::Item) -> bool>(&self, predicate: A) -> Option<usize> {
        self.tokens.iter().position(predicate)
    }

    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        if count <= self.tokens.len() {
            Ok(count)
        } else {
            Err(Needed::Unknown)
        }
    }
}

impl<'a> InputLength for Tokens<'a> {
    fn input_len(&self) -> usize {
        self.tokens.len()
    }
}

impl<'a> Slice<RangeFrom<usize>> for Tokens<'a> {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        Self {
            tokens: self.tokens.slice(range),
            source: self.source,
        }
    }
}
