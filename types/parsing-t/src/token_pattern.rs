use lexing::*;

pub trait TokenPattern {
    fn matches(&self, source: &str, kind: TokenKind, span: Span) -> bool;
    fn to_str(&self, source: &str) -> String;
}

impl<T: TokenPattern> TokenPattern for &T {
    fn matches(&self, source: &str, kind: TokenKind, span: Span) -> bool {
        (*self).matches(source, kind, span)
    }
    fn to_str(&self, source: &str) -> String {
        (*self).to_str(source)
    }
}

impl TokenPattern for TokenKind {
    fn matches(&self, _source: &str, kind: TokenKind, _span: Span) -> bool {
        self == &kind
    }
    fn to_str(&self, _source: &str) -> String {
        format!("{self}")
    }
}

impl TokenPattern for &str {
    fn matches(&self, source: &str, _kind: TokenKind, span: Span) -> bool {
        *self == &source[span.range()]
    }
    fn to_str(&self, _source: &str) -> String {
        self.to_string()
    }
}

impl<T: TokenPattern> TokenPattern for &[T] {
    fn matches(&self, source: &str, kind: TokenKind, span: Span) -> bool {
        self.iter()
            .any(|pattern| pattern.matches(source, kind, span))
    }
    fn to_str(&self, source: &str) -> String {
        self.iter()
            .map(|pattern| pattern.to_str(source))
            .collect::<Vec<_>>()
            .join(" | ")
    }
}

impl<T: TokenPattern, const SIZE: usize> TokenPattern for [T; SIZE] {
    fn matches(&self, source: &str, kind: TokenKind, span: Span) -> bool {
        self.iter()
            .any(|pattern| pattern.matches(source, kind, span))
    }
    fn to_str(&self, source: &str) -> String {
        self.iter()
            .map(|pattern| pattern.to_str(source))
            .collect::<Vec<_>>()
            .join(" | ")
    }
}
