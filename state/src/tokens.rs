
use std::ops::Range;

use logos::*;

pub struct Lexer<'a> {
    inner: logos::Lexer<'a, TokKind>,
    current: Tok,
    next: Tok,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut inner = logos::Lexer::new(input);
        Lexer {
            current: Tok::new(&mut inner),
            next: Tok::new(&mut inner),
            inner,
        }
    }

    pub fn current(&self) -> Tok {
        self.current.clone()
    }

    pub fn peek(&self) -> Tok {
        self.next.clone()
    }

    pub fn advance(&mut self) {
        self.current = self.next.clone();
        self.next = Tok::new(&mut self.inner);
    }

    pub fn show(&self, span: Range<usize>) -> &str {
        &self.inner.source()[span]
    }

    
    pub fn error_many(&self, got: TokKind, options: &[TokKind]) -> ! {
        let kind = options.iter().map(|o| format!("{:?}", o)).collect::<Vec<_>>().join(" or ");
        let line = self.inner.source()[..self.current.span.end].lines().count();
        let col = self.current.span.start - self.inner.source()[..self.current.span.end].rfind('\n').unwrap_or(0) + 1;
        println!("expected {} but got {:?} at line {}, column {}", kind, got, line, col);
        panic!();
    }

    pub fn expect(&self, kind: TokKind) {
        if self.current.kind != kind {
            self.error_many(self.current.kind.clone(), &[kind]); 
        }
    }

    pub fn skip_newlines(&mut self) {
        while self.current().kind == TokKind::Newline {
            self.advance();
        }
    }

}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tok {
    pub kind: TokKind,
    pub span: Range<usize>,
}

impl Tok {
    pub fn new(lexer: &mut logos::Lexer<TokKind>) -> Tok {
        let kind = lexer.next().unwrap_or(TokKind::Eof);
        let span = lexer.span();

        if kind == TokKind::EscapedIdent {
            return Tok {
                kind: TokKind::Ident,
                span: span.start + 1..span.end - 1,
            };
        }

        if kind == TokKind::String {
            return Tok {
                kind: TokKind::String,
                span: span.start + 1..span.end - 1,
            };
        }

        Tok {
            kind,
            span,
        }
    }
}

#[derive(Logos, Debug, Clone, PartialEq, Eq)]
pub enum TokKind {
    #[regex("call")]
    Call,
    #[regex("file")]
    File,
    #[regex("mut")]
    Mut,
    #[regex("struct")]
    Struct,
    #[regex("use")]
    Use,
    #[regex("owned")]
    Owned,
    #[regex("passed")]
    Passed,

    #[regex("`[^`]*`")]
    EscapedIdent,
    #[regex("[a-zA-Z][a-zA-Z0-9]*")]
    Ident,
    
    #[regex("\\{")]
    LBrace,
    #[regex("\\}")]
    RBrace,
    #[regex(":")]
    Colon,
    #[regex("=")]
    Equals,
    #[regex("(\n|;)")]
    Newline,

    #[regex(r#""[^"]*""#)]
    String,

    #[regex(r"[ \r\t]*", skip)]
    Spaces,
    #[error]
    Error,
    Eof,
}