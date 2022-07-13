use crate::{Span, Token, TokenKind};

pub struct Lexer<'a> {
    inner: logos::Lexer<'a, TokenKind>,
    skip: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str, skip: usize) -> Self {
        Lexer {
            inner: logos::Lexer::new(&source[skip..]),
            skip,
        }
    }

    pub fn display(&self, span: Span) -> &str {
        &self.inner.source()[span.range()]
    }

    pub fn progress(&self) -> usize {
        self.inner.source().len() - self.inner.remainder().len() + self.skip
    }

    pub fn finished(&self) -> bool {
        self.inner.remainder().len() == 0
    }

    pub fn next(&mut self) -> Token {
        let kind = self.inner.next().unwrap_or(TokenKind::Eof);
        let span = Span::new(self.inner.span()).shifted(self.skip);

        Token { kind, span }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        Some(self.next())
    }
}
