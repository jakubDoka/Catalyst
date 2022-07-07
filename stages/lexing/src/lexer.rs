use crate::{TokenKind, Span, Token};

pub struct Lexer<'a> {
    inner: logos::Lexer<'a, TokenKind>,
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let kind = self.inner.next().unwrap_or(TokenKind::Eof);
        let span = Span::new(self.inner.span());
        
        Some(Token {
            kind,
            span,
        })
    }
}