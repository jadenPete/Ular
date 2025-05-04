use crate::error_reporting::Position;
use nom::{InputIter, InputLength, Needed, Offset, Slice};
use std::{
    fmt::{Debug, Formatter},
    iter::Enumerate,
    ops::{Index, RangeFrom, RangeTo},
    slice::SliceIndex,
};

/// Grammer:
/// ```ebnf
/// ARROW = "=>";
/// COMMA = ",";
/// DEFINITION = "=";
/// FN_KEYWORD = "fn";
/// I8_TYPE = "i8";
/// I16_TYPE = "i16";
/// I32_TYPE = "i32";
/// I64_TYPE = "i64";
/// U8_TYPE = "u8";
/// U16_TYPE = "u8";
/// U32_TYPE = "u16";
/// U64_TYPE = "u64";
/// BOOL_TYPE = 'bool';
/// UNIT_TYPE = 'unit';
/// IF_KEYWORD = "if";
/// ELSE_KEYWORD = "else";
/// IDENTIFIER = ?[a-zA-Z_][a-zA-Z0-9_]*?;
/// LEFT_CURLY_BRACKET = "{";
/// RIGHT_CURLY_BRACKET = "}";
/// LEFT_PARENTHESIS = "(";
/// RIGHT_PARENTHESIS = ")";
/// LOGICAL_AND = "&&";
/// LOGICAL_OR = "||";
/// NOT = "!";
/// OVER = "/";
/// PLUS = "+";
/// MINUS = "-";
/// MODULO = "%";
/// NUMBER = ?[+-]?[0-9]+?;
/// TIMES = "*";
/// SEMICOLON = ";";
/// TYPE_ANNOTATION = ":";
/// ```
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Arrow,
    I8Type,
    I16Type,
    I32Type,
    I64Type,
    U8Type,
    U16Type,
    U32Type,
    U64Type,
    BoolType,
    UnitType,
    Comma,
    Definition,
    FnKeyword,
    IfKeyword,
    ElseKeyword,
    Identifier(String),
    LeftCurlyBracket,
    RightCurlyBracket,
    LeftParenthesis,
    RightParenthesis,
    LogicalAnd,
    LogicalOr,
    Not,
    Over,
    Plus,
    Minus,
    Modulo,
    Number(i128),
    Times,
    Semicolon,
    TypeAnnotation,
}

#[derive(Clone, Debug)]
pub struct PositionedToken {
    pub token: Token,
    pub position: Position,
}

#[derive(Copy, Clone)]
pub struct Tokens<'a> {
    pub tokens: &'a [PositionedToken],
    pub source: &'a str,
}

impl<'a> Tokens<'a> {
    pub fn get_position(&self) -> Position {
        Position(match (self.tokens.first(), self.tokens.last()) {
            (Some(first_token), Some(last_token)) => {
                first_token.position.0.start..last_token.position.0.end
            }

            /*
             * Technically, this isn't correct because `self.tokens` being empty doesn't imply that
             * `self.source` is empty, but empty token slices should be rare.
             */
            _ => 0..self.source.len(),
        })
    }
}

impl<'a> Debug for Tokens<'a> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{}", &self.source[self.get_position().0])
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

impl<'a> Offset for Tokens<'a> {
    fn offset(&self, second: &Self) -> usize {
        (second.tokens.as_ptr() as usize - self.tokens.as_ptr() as usize)
            / std::mem::size_of::<PositionedToken>()
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

impl<'a> Slice<RangeTo<usize>> for Tokens<'a> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        Self {
            tokens: self.tokens.slice(range),
            source: self.source,
        }
    }
}
