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
use scope::Vis;
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

    fn parse_with(&mut self, method: fn(&mut Self) -> errors::Result) -> (VSlice<Ast>, bool) {
        self.ast_data.start_cache();
        drop(method(self));
        self.state.progress = self.lexer.progress();
        (self.ast_data.bump_cached(), self.lexer.is_finished())
    }

    fn capture(&mut self, kind: AstKind) {
        self.ast_data
            .cache(Ast::leaf(kind, self.state.current.span));
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

    fn start(&mut self) -> Span {
        self.ast_data.start_cache();
        self.state.current.span
    }

    fn start_with(&mut self, amount: usize) -> Span {
        self.ast_data.split_cache_at(amount).span
    }

    fn join_frames(&mut self) {
        self.ast_data.join_cache_frames();
    }

    fn close(&mut self) -> VSlice<Ast> {
        self.ast_data.bump_cached()
    }

    fn finish(&mut self, kind: AstKind, span: Span) {
        let ast = self.close();
        self.ast_data.cache(Ast::new(kind, ast, span));
    }

    fn finish_last(&mut self, kind: AstKind, span: Span) {
        let span = self
            .ast_data
            .cached()
            .last()
            .map(|ast| span.joined(ast.span))
            .unwrap_or(span);
        let ast = self.close();
        self.ast_data.cache(Ast::new(kind, ast, span));
    }

    fn at<P>(&self, kind: P) -> bool
    where
        P: IntoIterator<Item = TokenKind>,
    {
        contains(kind, self.state.current.kind)
    }

    fn opt_list<L, S, R, M>(
        &mut self,
        left: L,
        sep: S,
        right: R,
        method: M,
    ) -> errors::Result<Option<Span>>
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
            Ok(Some(self.list(left, sep, right, method)?))
        } else {
            Ok(None)
        }
    }

    fn list<L, S, R, M>(&mut self, left: L, sep: S, right: R, mut method: M) -> errors::Result<Span>
    where
        L: IntoIterator<Item = TokenKind> + Clone,
        L::IntoIter: Clone,
        S: IntoIterator<Item = TokenKind> + Clone,
        S::IntoIter: Clone,
        R: IntoIterator<Item = TokenKind> + Clone,
        R::IntoIter: Clone,
        M: FnMut(&mut Self) -> errors::Result,
    {
        let mut total_span: Option<Span> = None;
        let mut include =
            |span| total_span = total_span.map(|start| start.joined(span)).or(Some(span));

        let right = move || right.clone();
        let sep = move || sep.clone();

        assert!(!matches!(
            (sep().into_iter().next(), right().into_iter().next()),
            (None, None)
        ));

        let frame_count = self.ast_data.cache_frame_count();

        if non_empty(left.clone()) {
            self.expect(left)?;
            include(self.state.current.span);
            self.advance();
            self.skip_newlines();
        }

        let terminals = sep().into_iter().chain(right());

        loop {
            if self.at(right()) {
                include(self.state.current.span);
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
                    include(self.state.current.span);
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
                    self.ast_data.cached().last().map(|ast| include(ast.span));
                    return Ok(total_span.unwrap());
                }
                self.advance();
            } else {
                unreachable!();
            }

            self.skip_newlines();
        }

        println!("{}", self.lexer.inner_span_str(total_span.unwrap()));

        Ok(total_span.unwrap())
    }

    fn skip_newlines(&mut self) {
        while self.at(TokenKind::NewLine) {
            self.advance();
        }
    }

    fn reduce_repetition(&mut self, tok: TokenKind) -> bool {
        if !self.at(tok) {
            return false;
        }

        while self.next(tok) {
            self.advance();
        }

        true
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
        while frame_count < self.ast_data.cache_frame_count() {
            drop(self.ast_data.discard_cache());
        }

        let mut pair_stack: BumpVec<(Span, TokenKind)> = bumpvec![];
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
        let span = self.start();
        self.expect(TokenKind::Ident)?;
        self.capture(AstKind::Ident);

        while self.at(TokenKind::Tick) && self.next(TokenKind::Ident) {
            self.advance();
            self.capture(AstKind::Ident);
        }

        if self.ast_data.cached().len() == 1 {
            self.join_frames();
        } else {
            self.finish_last(AstKind::IdentChain, span);
        }

        Ok(())
    }

    fn next(&self, kind: TokenKind) -> bool {
        self.state.next.kind == kind
    }

    gen_error_fns! {
        push expect_error(self, kinds: impl IntoIterator<Item = TokenKind> + Clone) {
            err: (
                "expected {} but got {}",
                kinds.into_iter().map(|k| k.as_str()).collect::<BumpVec<_>>().join(" | "),
                self.state.current.kind.as_str(),
            );
            (self.state.current.span, self.state.path) {
                err[self.state.current.span]: "token located here";
            }
        }

        push expect_str_error(self, strings: &[&str]) {
            err: (
                "expected '{}' but got {}",
                strings.join("' | '"),
                self.current_token_str(),
            );
            (self.state.current.span, self.state.path) {
                err[self.state.current.span]: "token located here";
            }
        }

        push unmatched_paren(self, kind: TokenKind, span: Span) {
            err: ("unmatched paren {}", kind.as_str());
            (span.joined(self.state.current.span), self.state.path) {
                err[span]: "the starting paren";
            }
        }
    }

    fn ctx_keyword(&mut self, keyword: &str) -> bool {
        let present = self.lexer.inner_span_str(self.state.current.span) == keyword;
        if present {
            self.advance();
        }
        present
    }

    fn generics(&mut self) -> errors::Result {
        let span = self.start();

        self.opt_list(
            TokenKind::LeftBracket,
            Some(TokenKind::Comma),
            Some(TokenKind::RightBracket),
            Self::generic_bounded_param,
        )?;

        self.finish(AstKind::Generics, span);

        Ok(())
    }

    fn generic_bounded_param(&mut self) -> errors::Result {
        let span = self.start();
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

        self.finish_last(AstKind::GenericParam, span);

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
        self.lexer.inner_span_str(self.state.current.span)
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
    current: Token,
    next: Token,
    progress: usize,
    path: Ident,
    is_formatting: bool,
}

impl ParserState {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn start(&mut self, source: &str, path: Ident, is_formatting: bool) {
        let mut lexer = Lexer::new(source, 0);
        self.current = lexer.next();
        self.next = lexer.next();
        self.progress = lexer.progress();
        self.path = path;
        self.is_formatting = is_formatting;
    }
}

pub fn to_snippet(ast: Ast, ast_data: &AstData, origin: Ident) -> diags::Snippet {
    diags::Snippet {
        title: annotation!(info: "parsing analysis"),
        slices: vec![Some(diags::Slice {
            span: ast.span,
            origin,
            annotations: {
                fn walk(root: Ast, ast_data: &AstData, fun: &mut impl FnMut(Ast)) {
                    fun(root);
                    for &child in &ast_data[root.children] {
                        if child.kind == AstKind::None {
                            continue;
                        }
                        walk(child, ast_data, fun);
                    }
                }

                let mut vec = vec![];
                walk(ast, ast_data, &mut |ast| {
                    vec.push(source_annotation!(info[ast.span]: ("{:?}", ast.kind)));
                });
                vec
            },
        })],
        ..Default::default()
    }
}
