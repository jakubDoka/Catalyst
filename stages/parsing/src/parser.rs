mod imports;

use std::vec;

use lexing::*;
use storage::{Maybe, Ident};
use crate::*;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    state: &'a mut ParserState,
    ast_data: &'a mut AstData,
    diagnostics: &'a mut Diagnostics,
}


impl<'a> Parser<'a> {
    pub fn new(
        lexer: Lexer<'a>,
        state: &'a mut ParserState,
        ast_data: &'a mut AstData,
        diagnostics: &'a mut Diagnostics,
    ) -> Self {
        Self {
            lexer,
            state,
            ast_data,
            diagnostics,
        }
    }

    fn parse_with(&mut self, method: fn(&mut Self) -> errors::Result) -> errors::Result<(Maybe<AstList>, bool)> {
        self.ast_data.start_cache();
        method(self)?;
        self.state.progress = self.lexer.progress();
        Ok((self.ast_data.bump_cached(), self.lexer.finished()))
    }

    fn capture(&mut self, kind: AstKind) {
        self.ast_data.push(AstEnt::leaf(kind, self.state.current.span));
        self.advance();
    }

    fn advance(&mut self) {
        self.state.current = self.state.next;
        self.state.next = self.lexer.next();
    }

    fn optional(&mut self, keyword: TokenKind, method: impl Fn(&mut Self) -> errors::Result) -> errors::Result {
        if self.at(keyword) {
            method(self)
        } else {
            Ok(())
        }
    }

    fn start(&mut self) {
        self.state.start.push(self.state.current.span);
        self.ast_data.start_cache();
    }

    fn close(&mut self) -> Maybe<AstList> {
        self.ast_data.bump_cached()
    }

    fn finish(&mut self, kind: AstKind) {
        let span = self.total_span();
        let ast = self.close();
        self.ast_data.cache(AstEnt::new(kind, ast, span));
    }

    fn total_span(&mut self) -> Span {
        let start = self.state.start.pop().unwrap();
        let end = self.ast_data.cached().last().unwrap().span;
        start.join(end)
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.state.current.kind == kind
    }

    fn list(&mut self, left: TokenKind, sep: TokenKind, right: TokenKind, method: impl Fn(&mut Self) -> errors::Result) -> errors::Result {
        self.expect(left)?;
        self.advance();

        loop {
            if self.at(right) {
                break;
            }

            if let Err(()) = method(self) {
                self.recover(&[sep, right])?;
            }

            if self.at(right) {
                break;
            }

            self.expect(sep)?;
            self.advance();
        }

        self.advance();
        
        Ok(())
    }

    fn expect(&mut self, kind: TokenKind) -> errors::Result {
        self.expect_many(&[kind])
    }

    fn expect_many(&mut self, kinds: &[TokenKind]) -> errors::Result {
        if kinds.contains(&self.state.current.kind) {
            Ok(())
        } else {
            self.expect_error(kinds);
            Err(())
        }
    }

    fn recover(&mut self, kinds: &[TokenKind]) -> errors::Result {
        let mut pair_stack: Vec<(Span, TokenKind)> = vec![];
        loop {
            if let Some(complement) = self.state.current.kind.complement() {
                pair_stack.push((self.state.current.span, complement));
                self.advance();
                continue;
            } else if let Some(&(span, kind)) = pair_stack.last() {
                if kind == self.state.current.kind {
                    pair_stack.pop();
                } else if self.state.current.kind.is_closing() {
                    self.unmatched_paren(kind, span);
                    return Err(());
                }
                self.advance();
                continue;
            }

            if kinds.contains(&self.state.current.kind) {
                break;
            }

            if self.state.current.kind == TokenKind::Eof {
                self.expect_error(kinds);
                return Err(());
            }

            self.advance();
        }

        Ok(())
    }

    
    fn expect_error(&mut self, kinds: &[TokenKind]) {
        self.diagnostics.push(diagnostic! {
            loc: (self.state.current.span, self.state.path.unwrap()),
            message: "expected one of {} but got {}" => (
                kinds.iter().map(|k| k.as_str()).collect::<Vec<_>>().join(" | "),
                self.state.current.kind.as_str(),
            ),
            level: ERROR,
        });
    }
    
    fn unmatched_paren(&mut self, kind: TokenKind, span: Span) {
        self.diagnostics.push(diagnostic! {
            loc: (self.state.current.span, self.state.path.unwrap()),
            message: "unmatched paren {}" => (kind.as_str()),
            level: ERROR,
            related: (
                (span, self.state.path.unwrap()) => "the starting paren",
            ),
        });
    }
}

#[derive(Default)]
pub struct ParserState {
    start: Vec<Span>,
    current: Token,
    next: Token,
    progress: usize,
    path: Maybe<Ident>,
}

impl ParserState {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn start(&mut self, source: &str, path: Ident) {
        let mut lexer = Lexer::new(source);
        self.current = lexer.next();
        self.next = lexer.next();
        self.progress = lexer.progress();
        self.path = Maybe::some(path);
    }
}