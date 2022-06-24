#![feature(auto_traits)]
#![feature(negative_impls)]
#![feature(int_log)]
#![feature(let_else)]

pub mod types;

pub use types::{
    BuiltinSource, Source, SourceEnt, Sources, SourcesExt, Span, Token, TokenKind,
    EQUAL_SIGN_PRECEDENCE,
};

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
        let span = Span::new(
            self.source,
            span.start + self.offset,
            span.end + self.offset,
        );

        Token { kind, span }
    }

    pub fn progress(&mut self) -> usize {
        self.inner.span().end + self.offset
    }
}

#[derive(Debug, Clone, Copy)]
pub enum EscapeError {
    InvalidEscape,
}

pub struct EscapeTranslator<'a> {
    str: &'a str,
    progress: usize,
    error: Option<EscapeError>,
}

impl<'a> EscapeTranslator<'a> {
    pub fn new(str: &'a str) -> Self {
        Self {
            str,
            progress: 0,
            error: None,
        }
    }

    pub fn report_error(&self) -> Option<(EscapeError, usize)> {
        self.error.map(|error| (error, self.progress))
    }

    pub fn next_char(&mut self) -> Option<char> {
        if self.error.is_some() {
            return None;
        }

        let mut chars = self.str[self.progress..].chars();
        let mut next = || {
            let c = chars.next()?;
            self.progress += c.len_utf8();
            Some(c)
        };

        Some(match next() {
            Some('\\') => {
                let c = next()?;
                match c {
                    'n' => '\n',
                    _ => {
                        self.error = Some(EscapeError::InvalidEscape);
                        return None;
                    }
                }
            }
            Some(c) => c,
            None => return None,
        })
    }
}

impl Iterator for EscapeTranslator<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_char()
    }
}
