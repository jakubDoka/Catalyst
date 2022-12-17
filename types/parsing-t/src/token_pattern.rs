use lexing::*;

use crate::ctx::*;

pub trait TokenPattern {
    fn matches(&self, ctx: &ParsingCtx, token: Token) -> bool;
    fn to_str(&self, ctx: &ParsingCtx) -> String;
}

impl<T: TokenPattern> TokenPattern for &T {
    fn matches(&self, ctx: &ParsingCtx, token: Token) -> bool {
        (*self).matches(ctx, token)
    }
    fn to_str(&self, ctx: &ParsingCtx) -> String {
        (*self).to_str(ctx)
    }
}

impl TokenPattern for TokenKind {
    fn matches(&self, _ctx: &ParsingCtx, token: Token) -> bool {
        self == &token.kind
    }
    fn to_str(&self, _ctx: &ParsingCtx) -> String {
        format!("{self}")
    }
}

impl TokenPattern for &str {
    fn matches(&self, ctx: &ParsingCtx, token: Token) -> bool {
        *self == ctx.inner_span_str(token.span)
    }
    fn to_str(&self, _ctx: &ParsingCtx) -> String {
        self.to_string()
    }
}

impl<T: TokenPattern> TokenPattern for &[T] {
    fn matches(&self, ctx: &ParsingCtx, token: Token) -> bool {
        self.iter().any(|pattern| pattern.matches(ctx, token))
    }
    fn to_str(&self, ctx: &ParsingCtx) -> String {
        self.iter()
            .map(|pattern| pattern.to_str(ctx))
            .collect::<Vec<_>>()
            .join(" | ")
    }
}

impl<T: TokenPattern, const SIZE: usize> TokenPattern for [T; SIZE] {
    fn matches(&self, ctx: &ParsingCtx, token: Token) -> bool {
        self.iter().any(|pattern| pattern.matches(ctx, token))
    }
    fn to_str(&self, ctx: &ParsingCtx) -> String {
        self.iter()
            .map(|pattern| pattern.to_str(ctx))
            .collect::<Vec<_>>()
            .join(" | ")
    }
}
