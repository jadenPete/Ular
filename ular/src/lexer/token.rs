use crate::error_reporting::Position;
use nom::{InputIter, InputLength, Needed, Offset, Slice};
use std::{
    fmt::{Debug, Formatter},
    iter::Enumerate,
    ops::{Index, RangeFrom, RangeTo},
    slice::SliceIndex,
};

/// A token in the language.
///
/// Tokens follow the following grammer, defined in
/// [extended Backus–Naur form](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form):
/// ```ebnf
/// (*
///  * Structure:
///  *
///  * Tokens are divided into two categories: fixed-length and variable-length. We define the
///  * fixed-length tokens first to prevent them from being interpreted as variable-length tokens
///  * (e.g. a keyword being interpreted as an identifier).
///  *
///  * For the most part, they're are ordered alphabetically by name, with the exception that
///  * similar tokens are grouped together. For example, the following tokens are grouped together:
///  * - Comparison operators
///  * - Keywords
///  * - Braces
///  * - Logical operators
///  * - Arithmetic operators
///  *)
///
/// (* Fixed-length tokens *)
/// ARROW = "=>";
/// COMMA = ",";
/// EQUAL_COMPARISON = "==";
/// LESS_THAN_OR_EQUAL = "<=";
/// LESS_THAN = "<";
/// GREATER_THAN_OR_EQUAL = ">=";
/// GREATER_THAN = ">";
/// UNEQUAL_COMPARISON = "!=";
///
/// (*
///  * This needs to come after the comparison operators so they aren't misinterpreted as
///  * `DEFINITION`s
///  *)
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
/// SEQ_KEYWORD = "seq";
/// LEFT_CURLY_BRACKET = "{";
/// RIGHT_CURLY_BRACKET = "}";
/// LEFT_PARENTHESIS = "(";
/// RIGHT_PARENTHESIS = ")";
/// LOGICAL_AND = "&&";
/// LOGICAL_OR = "||";
/// NOT = "!";
/// OVER = "/";
/// PLUS = "+";
/// MODULO = "%";
/// TIMES = "*";
/// SEMICOLON = ";";
/// TYPE_ANNOTATION = ":";
///
/// (* Variable-length tokens *)
/// IDENTIFIER = ?[a-zA-Z_][a-zA-Z0-9_]*?;
/// NUMBER = ?[+-]?[0-9]+?;
///
/// (* This needs to come after `NUMBER` so signs aren't interpreted as operators *)
/// MINUS = "-";
/// ```
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Arrow,
    Comma,
    Definition,
    EqualComparison,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    UnequalComparison,
    FnKeyword,
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
    IfKeyword,
    ElseKeyword,
    SeqKeyword,
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
    Times,
    Semicolon,
    TypeAnnotation,
    Identifier(String),
    Number(i128),
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

impl Tokens<'_> {
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

impl Debug for Tokens<'_> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{}", &self.source[self.get_position().0])
    }
}

impl<A: SliceIndex<[PositionedToken]>> Index<A> for Tokens<'_> {
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

impl InputLength for Tokens<'_> {
    fn input_len(&self) -> usize {
        self.tokens.len()
    }
}

impl Offset for Tokens<'_> {
    fn offset(&self, second: &Self) -> usize {
        (second.tokens.as_ptr() as usize - self.tokens.as_ptr() as usize)
            / std::mem::size_of::<PositionedToken>()
    }
}

impl Slice<RangeFrom<usize>> for Tokens<'_> {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        Self {
            tokens: self.tokens.slice(range),
            source: self.source,
        }
    }
}

impl Slice<RangeTo<usize>> for Tokens<'_> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        Self {
            tokens: self.tokens.slice(range),
            source: self.source,
        }
    }
}
