use crate::{TokenKind, Token, Span};

pub struct Lexer<'a> {
    inner: logos::Lexer<'a, TokenKind>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            inner: logos::Lexer::new(source),
        }
    }

    pub fn progress(&self) -> usize {
        self.inner.source().len() - self.inner.remainder().len()
    }

    pub fn finished(&self) -> bool {
        self.inner.remainder().len() == 0
    }

    pub fn next(&mut self) -> Token {
        let kind = self.inner.next().unwrap_or(TokenKind::Eof);
        let span = Span::new(self.inner.span());

        Token {
            kind,
            span,
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        Some(self.next())
    }
}