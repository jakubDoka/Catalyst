use crate::*;
use lexing_t::*;

/// Tight wrapper around logos lexer that provides specific tokens
/// instead.
pub struct Lexer<'a> {
    inner: logos::Lexer<'a, TokenKind>,
}

impl<'a> Lexer<'a> {
    #[inline]
    pub fn new(source: &'a str, skipped_bytes: usize) -> Self {
        let mut inner = logos::Lexer::new(source);
        inner.bump(skipped_bytes);
        Lexer { inner }
    }

    #[inline]
    pub fn inner_span_str(&self, span: Span) -> &str {
        &self.inner.source()[span.range()]
    }

    /// Returns amount of *bytes* processed.
    #[inline]
    pub fn progress(&self) -> usize {
        self.inner.source().len() - self.inner.remainder().len()
    }

    #[inline]
    pub fn is_finished(&self) -> bool {
        self.inner.remainder().is_empty()
    }

    #[inline]
    pub fn next_tok(&mut self) -> Token {
        let kind = self.inner.next().unwrap_or(TokenKind::Eof);
        let span = Span::new(self.inner.span());

        Token { kind, span }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        Some(self.next_tok())
    }
}
