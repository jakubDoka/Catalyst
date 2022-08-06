mod bound;
mod expr;
mod r#fn;
mod imports;
mod items;
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

    fn start_with(&mut self, amount: usize) {
        self.state
            .start
            .push(self.ast_data.split_cache_at(amount).span);
    }

    fn join_frames(&mut self) {
        self.state.start.pop().unwrap();
        self.ast_data.join_cache_frames();
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

    fn at<P>(&self, kind: P) -> bool
    where
        P: IntoIterator<Item = TokenKind>,
    {
        contains(kind, self.state.current.kind)
    }

    fn opt_list<L, S, R, M>(&mut self, left: L, sep: S, right: R, method: M) -> errors::Result
    where
        L: IntoIterator<Item = TokenKind> + Clone,
        L::IntoIter: Clone,
        S: IntoIterator<Item = TokenKind> + Clone,
        S::IntoIter: Clone,
        R: IntoIterator<Item = TokenKind> + Clone,
        R::IntoIter: Clone,
        M: Fn(&mut Self) -> errors::Result,
    {
        if self.at(left.clone()) {
            self.list(left, sep, right, method)?;
        }

        Ok(())
    }

    fn list<L, S, R, M>(&mut self, left: L, sep: S, right: R, method: M) -> errors::Result
    where
        L: IntoIterator<Item = TokenKind> + Clone,
        L::IntoIter: Clone,
        S: IntoIterator<Item = TokenKind> + Clone,
        S::IntoIter: Clone,
        R: IntoIterator<Item = TokenKind> + Clone,
        R::IntoIter: Clone,
        M: Fn(&mut Self) -> errors::Result,
    {
        let right = move || right.clone();
        let sep = move || sep.clone();

        assert!(!matches!(
            (sep().into_iter().next(), right().into_iter().next()),
            (None, None)
        ));

        let frame_count = self.ast_data.chace_frame_count();

        if non_empty(left.clone()) {
            self.expect(left)?;
            self.advance();
            self.skip_newlines();
        }

        let terminals = sep().into_iter().chain(right());

        loop {
            if self.at(right()) {
                self.advance();
                break;
            }

            if let Err(()) = method(self) {
                let terminal = self.recover(terminals.clone(), frame_count)?;
                if contains(right(), terminal) {
                    break;
                }
                continue;
            }

            if non_empty(right()) {
                if self.at(right()) {
                    self.advance();
                    break;
                }

                if non_empty(sep()) {
                    if let Err(()) = self.expect(sep()) {
                        let terminal = self.recover(terminals.clone(), frame_count)?;
                        if contains(right(), terminal) {
                            break;
                        }
                        continue;
                    }
                    self.advance();
                }
            } else if non_empty(sep()) {
                if !self.at(sep()) {
                    return Ok(());
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

    fn expect<P>(&mut self, kinds: P) -> errors::Result
    where
        P: IntoIterator<Item = TokenKind> + Clone,
    {
        if self.at(kinds.clone()) {
            Ok(())
        } else {
            self.expect_error(kinds);
            Err(())
        }
    }

    fn recover(
        &mut self,
        terminals: impl IntoIterator<Item = TokenKind> + Clone,
        frame_count: usize,
    ) -> errors::Result<TokenKind> {
        while frame_count < self.ast_data.chace_frame_count() {
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

            if contains(terminals.clone(), self.state.current.kind) {
                let cur = self.state.current.kind;
                self.advance();
                return Ok(cur);
            }

            if self.at(TokenKind::Eof) {
                self.expect_error(terminals);
                return Err(());
            }

            self.advance();
        }
    }

    fn ident_chain(&mut self) -> errors::Result {
        self.start();
        self.expect(TokenKind::Ident)?;
        self.capture(AstKind::Ident);

        while self.at(TokenKind::Tick) && self.next(TokenKind::Ident) {
            self.advance();
            self.capture(AstKind::Ident);
        }

        if self.ast_data.cached().len() == 1 {
            self.join_frames();
        } else {
            self.finish(AstKind::IdentChain);
        }

        Ok(())
    }

    fn next(&self, kind: TokenKind) -> bool {
        self.state.next.kind == kind
    }

    fn expect_error(&mut self, kinds: impl IntoIterator<Item = TokenKind>) {
        self.workspace.push(diag! {
            (self.state.current.span, self.state.path)
            error => "expected {} but got {}" {
                kinds.into_iter().map(|k| k.as_str()).collect::<Vec<_>>().join(" | "),
                self.state.current.kind.as_str(),
            },
        });
    }

    fn expect_str_error(&mut self, strs: &[&str]) {
        self.workspace.push(diag! {
            (self.state.current.span, self.state.path)
            error => "expected {} but got {}" {
                strs.iter().map(|s| format!("'{}'", s)).collect::<Vec<_>>().join(" | "),
                self.current_token_str()
            },
        });
    }

    fn unmatched_paren(&mut self, kind: TokenKind, span: Span) {
        self.workspace.push(diag! {
            (self.state.current.span, self.state.path)
            error => "unmatched paren {}" { kind.as_str() },
            (span, self.state.path) => "the starting paren",
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

        self.opt_list(
            TokenKind::LeftBracket,
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
            self.ty()?;

            while self.ctx_keyword("+") {
                self.advance();
                self.skip_newlines();
                self.ty()?;
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

    fn expect_ctx_keyword(&mut self, arg: &str) -> errors::Result {
        if self.ctx_keyword(arg) {
            Ok(())
        } else {
            self.expect_str_error(&[arg]);
            Err(())
        }
    }
}

fn non_empty(iter: impl IntoIterator<Item = TokenKind>) -> bool {
    iter.into_iter().next().is_some()
}

fn contains(iter: impl IntoIterator<Item = TokenKind>, kind: TokenKind) -> bool {
    iter.into_iter().any(|item| item == kind)
}

#[derive(Default)]
pub struct ParserState {
    start: Vec<Span>,
    current: Token,
    next: Token,
    progress: usize,
    path: Ident,
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
        self.path = path;
    }
}
