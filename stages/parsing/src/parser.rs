mod imports;
mod manifest;
mod r#struct;
mod ty;

use std::vec;

use crate::*;
use diags::*;
use lexing::*;
use lexing_t::*;
use parsing_t::*;
use storage::*;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    state: &'a mut ParserState,
    ast_data: &'a mut AstData,
    workspace: &'a mut Workspace,
}

impl<'a> Parser<'a> {
    pub fn new(
        source: &'a str,
        state: &'a mut ParserState,
        ast_data: &'a mut AstData,
        workspace: &'a mut Workspace,
    ) -> Self {
        Self {
            lexer: Lexer::new(source, state.progress),
            state,
            ast_data,
            workspace,
        }
    }

    fn parse_with(&mut self, method: fn(&mut Self) -> errors::Result) -> (Maybe<AstList>, bool) {
        self.ast_data.start_cache();
        drop(method(self));
        self.state.progress = self.lexer.progress();
        (self.ast_data.bump_cached(), self.lexer.finished())
    }

    fn capture(&mut self, kind: AstKind) {
        self.ast_data
            .cache(AstEnt::leaf(kind, self.state.current.span));
        self.advance();
    }

    fn advance(&mut self) {
        self.state.current = self.state.next;
        self.state.next = self.lexer.next();
    }

    fn optional(
        &mut self,
        keyword: TokenKind,
        method: impl Fn(&mut Self) -> errors::Result,
    ) -> errors::Result {
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
        let end = self.ast_data.cached().last().map_or(start, |n| n.span);
        start.joined(end)
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.state.current.kind == kind
    }

    fn list(
        &mut self,
        left: Option<TokenKind>,
        sep: Option<TokenKind>,
        right: Option<TokenKind>,
        method: impl Fn(&mut Self) -> errors::Result,
    ) -> errors::Result {
        assert!(!matches!((left, sep, right), (.., None, None)));

        let frame_count = self.ast_data.frame_count();

        if let Some(left) = left {
            self.expect(left)?;
            self.advance();
            self.skip_newlines();
        }

        loop {
            if let Some(right) = right && self.at(right) {
                break;
            }

            if let Err(()) = method(self) {
                let terminals = sep.into_iter().chain(right).collect::<Vec<_>>();
                self.recover(&terminals, frame_count)?;
            }

            if let Some(right) = right {
                if self.at(right) {
                    self.advance();
                    break;
                }

                if let Some(sep) = sep {
                    self.expect(sep)?;
                    self.advance();
                }
            } else if let Some(sep) = sep {
                if !self.at(sep) {
                    break;
                }
                self.advance();
            } else {
                unreachable!();
            }

            self.skip_newlines();
        }

        Ok(())
    }

    fn skip_newlines(&mut self) {
        while self.at(TokenKind::NewLine) {
            self.advance();
        }
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

    fn recover(&mut self, kinds: &[TokenKind], frame_count: usize) -> errors::Result {
        while frame_count < self.ast_data.frame_count() {
            drop(self.ast_data.discard_cache());
        }

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
        self.workspace.push(diag! {
            (self.state.current.span, self.state.path.unwrap())
            error => "expected {} but got {}" {
                kinds.iter().map(|k| k.as_str()).collect::<Vec<_>>().join(" | "),
                self.state.current.kind.as_str(),
            },
        });
    }

    fn expect_str_error(&mut self, strs: &[&str]) {
        self.workspace.push(diag! {
            (self.state.current.span, self.state.path.unwrap())
            error => "expected {} but got {}" {
                strs.iter().map(|s| format!("'{}'", s)).collect::<Vec<_>>().join(" | "),
                self.current_token_str()
            },
        });
    }

    fn unmatched_paren(&mut self, kind: TokenKind, span: Span) {
        self.workspace.push(diag! {
            (self.state.current.span, self.state.path.unwrap())
            error => "unmatched paren {}" { kind.as_str() },
            (span, self.state.path.unwrap()) => "the starting paren",
        });
    }

    fn ctx_keyword(&mut self, keyword: &str) -> bool {
        let present = self.lexer.display(self.state.current.span) == keyword;
        if present {
            self.advance();
        }
        present
    }

    fn generics(&mut self, has_bounds: bool) -> errors::Result {
        self.start();

        let parser = if has_bounds {
            Self::generic_bounded_param
        } else {
            Self::ident
        };

        self.list(
            Some(TokenKind::LeftBracket),
            Some(TokenKind::Comma),
            Some(TokenKind::RightBracket),
            parser,
        )?;

        self.finish(AstKind::Generics);

        Ok(())
    }

    fn generic_bounded_param(&mut self) -> errors::Result {
        self.start();
        self.expect(TokenKind::Ident)?;
        self.capture(AstKind::Ident);

        if self.at(TokenKind::Colon) {
            self.advance();
            self.ident()?;

            while self.ctx_keyword("+") {
                self.advance();
                self.skip_newlines();
                self.ident()?;
            }
        }

        self.finish(AstKind::GenericParam);

        Ok(())
    }

    fn ident(&mut self) -> errors::Result {
        self.expect(TokenKind::Ident)?;
        self.capture(AstKind::Ident);
        Ok(())
    }

    fn visibility(&mut self) -> Vis {
        let res = match self.state.current.kind {
            TokenKind::Pub => Vis::Pub,
            TokenKind::Priv => Vis::Priv,
            _ => Vis::None,
        };

        if res != Vis::None {
            self.advance();
        }

        res
    }

    fn advance_if(&mut self, kind: TokenKind) -> bool {
        let res = self.state.current.kind == kind;
        if res {
            self.advance();
        }
        res
    }

    fn current_token_str(&self) -> &str {
        self.lexer.display(self.state.current.span)
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
        let mut lexer = Lexer::new(source, 0);
        self.current = lexer.next();
        self.next = lexer.next();
        self.progress = lexer.progress();
        self.path = Maybe::some(path);
    }
}
