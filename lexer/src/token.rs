use std::ops::Range;

use crate::source_info::Span;

#[derive(Debug, Clone, Copy, Default)]
pub struct Token {
    kind: Kind,
    span: Span,
}

impl Token {
    pub fn new(kind: Kind, span: Span) -> Token {
        Self { kind, span }
    }

    pub fn kind(&self) -> Kind {
        self.kind
    }

    pub fn range(&self) -> Range<usize> {
        self.span.range()
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    Fn,
    Ret,
    Use,
    Extern,
    If,
    Else,

    Ident,
    Operator,

    Int(i16),
    String,
    Bool(bool),

    LeftCurly,
    RightCurly,

    LeftParen,
    RightParen,

    LeftBracket,
    RightBracket,

    Comma,
    Colon,
    RightArrow,

    NewLine,

    Eof,

    None,
}

impl Default for Kind {
    fn default() -> Self {
        Kind::None
    }
}
