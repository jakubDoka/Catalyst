#![feature(auto_traits)]
#![feature(negative_impls)]
#![feature(int_log)]
#![feature(bool_to_option)]
#![feature(let_else)]

pub mod types;

pub use types::{Source, TokenKind, Token, Span, SourceEnt, Sources, SourcesExt, BuiltinSource};

pub struct Lexer<'a> {
    offset: usize,
    inner: logos::Lexer<'a, TokenKind>,
    source: Source,
}

impl<'a> Lexer<'a> {
    pub fn new(offset: usize, source: Source, input: &'a str) -> Self {
        Self {
            offset,
            inner: logos::Lexer::new(&input[offset..]),
            source,
        }
    }

    pub fn next_token(&mut self) -> Token {
        let kind = self.inner.next().unwrap_or(TokenKind::Eof); 
        let span = self.inner.span();
        let span = Span::new(self.source, span.start + self.offset, span.end + self.offset);

        Token {
            kind,
            span,
        }
    }

    pub fn progress(&mut self) -> usize {
        self.inner.span().end + self.offset
    }
}