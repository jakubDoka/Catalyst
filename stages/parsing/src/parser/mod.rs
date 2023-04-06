pub mod expr;
pub mod func;
pub mod imports;
pub mod items;
pub mod manifest;
pub mod spec;
pub mod r#struct;
pub mod ty;

use std::{
    mem,
    ops::{Deref, DerefMut},
};

use crate::*;
use diags::*;
use lexing::*;
use resources::*;
use storage::*;

use lexing::TokenKind as Tk;

pub struct Parser<'ctx, 'arena, M = NoTokenMeta> {
    interner: &'ctx mut Interner,
    workspace: &'ctx mut Workspace,
    state: &'ctx mut ParserCtx<M>,
    arena: &'ctx ProxyArena<'arena>,
    source: VRef<Source>,
    lexer: Lexer<'ctx>,
}

impl<'ctx, 'arena, M> Drop for Parser<'ctx, 'arena, M> {
    fn drop(&mut self) {
        self.state.progress = self.lexer.progress();
    }
}

impl<'ctx, 'arena, M: TokenMeta> Parser<'ctx, 'arena, M> {
    pub fn new(
        interner: &'ctx mut Interner,
        workspace: &'ctx mut Workspace,
        state: &'ctx mut ParserCtx<M>,
        arena: &'ctx ProxyArena<'arena>,
        source: VRef<Source>,
        content: &'ctx str,
    ) -> Self {
        Self {
            interner,
            workspace,
            lexer: Lexer::new(content, state.progress),
            state,
            arena,
            source,
        }
    }

    fn generics(&mut self) -> Option<Option<ListAst<'arena, ParamAst<'arena, M>, M>>> {
        self.opt_array("generic parameters", Self::param)
    }

    fn opt_object<T>(
        &mut self,
        for_the: &'static str,
        parser: impl FnMut(&mut Self) -> Option<T>,
    ) -> Option<Option<ListAst<'arena, T, M>>> {
        self.opt_list(for_the, parser, Tk::LeftBrace, Tk::NewLine, Tk::RightBrace)
    }

    fn opt_array<T>(
        &mut self,
        for_the: &'static str,
        parser: impl FnMut(&mut Self) -> Option<T>,
    ) -> Option<Option<ListAst<'arena, T, M>>> {
        self.opt_list(
            for_the,
            parser,
            Tk::LeftBracket,
            Tk::Comma,
            Tk::RightBracket,
        )
    }

    fn opt_tuple<T>(
        &mut self,
        for_the: &'static str,
        parser: impl FnMut(&mut Self) -> Option<T>,
    ) -> Option<Option<ListAst<'arena, T, M>>> {
        self.opt_list(for_the, parser, Tk::LeftParen, Tk::Comma, Tk::RightParen)
    }

    fn opt_list<T>(
        &mut self,
        for_the: &'static str,
        parser: impl FnMut(&mut Self) -> Option<T>,
        start: impl TokenPattern,
        sep: impl TokenPattern,
        end: impl TokenPattern,
    ) -> Option<Option<ListAst<'arena, T, M>>> {
        if !self.at(&start) {
            return Some(None);
        }

        self.list(for_the, parser, start, sep, end).map(Some)
    }

    fn block<T>(
        &mut self,
        for_the: &'static str,
        parser: impl FnMut(&mut Self) -> Option<T>,
    ) -> Option<ListAst<'arena, T, M>> {
        self.list(for_the, parser, Tk::LeftBrace, Tk::NewLine, Tk::RightBrace)
    }

    fn object<T>(
        &mut self,
        for_the: &'static str,
        parser: impl FnMut(&mut Self) -> Option<T>,
    ) -> Option<ListAst<'arena, T, M>> {
        self.list(for_the, parser, Tk::LeftBrace, Tk::Comma, Tk::RightBrace)
    }

    fn array<T>(
        &mut self,
        for_the: &'static str,
        parser: impl FnMut(&mut Self) -> Option<T>,
    ) -> Option<ListAst<'arena, T, M>> {
        self.list(
            for_the,
            parser,
            Tk::LeftBracket,
            Tk::Comma,
            Tk::RightBracket,
        )
    }

    fn tuple<T>(
        &mut self,
        for_the: &'static str,
        parser: impl FnMut(&mut Self) -> Option<T>,
    ) -> Option<ListAst<'arena, T, M>> {
        self.list(for_the, parser, Tk::LeftParen, Tk::Comma, Tk::RightParen)
    }

    fn list<T>(
        &mut self,
        for_the: &'static str,
        mut parser: impl FnMut(&mut Self) -> Option<T>,
        start: impl TokenPattern,
        sep: impl TokenPattern,
        end: impl TokenPattern,
    ) -> Option<ListAst<'arena, T, M>> {
        let mut start = self.expect(&start, |s| ExpectedStartOfAst {
            ast_name: for_the,
            found: s.current.kind,
            expected: start.to_str(s.lexer.source()),
            loc: s.loc(),
        })?;

        self.append_newlines(&mut start.meta);

        let mut elements = bumpvec![];
        let end = loop {
            if self.at(&end) {
                break self.advance();
            }

            let Some(value) = parser(self) else {
                if let Some(end) = self.recover_list(&sep, &end)? {
                    break end;
                }
                continue;
            };

            if self.at(&end) {
                elements.push(ListElemAst { value, delim: None });
                break self.advance();
            }

            let Some(mut delim) = self.expect(&sep, |s| ExpectedListSep {
                ast_name: for_the,
                found: s.current.kind,
                expected_sep: sep.to_str(s.lexer.source()),
                expected_end: end.to_str(s.lexer.source()),
                loc: s.loc(),
            }) else {
                if let Some(end) = self.recover_list(&sep, &end)? {
                    break end;
                }
                continue;
            };

            self.append_newlines(&mut delim.meta);

            elements.push(ListElemAst {
                value,
                delim: Some(delim),
            });
        };

        Some(ListAst {
            start,
            elements: self.arena.alloc_iter(elements),
            end,
        })
    }

    fn wrapped<T>(
        &mut self,
        parser: impl FnOnce(&mut Self) -> Option<T>,
        around_the: &'static str,
        start: impl TokenPattern,
        end: impl TokenPattern,
    ) -> Option<WrappedAst<T, M>> {
        let handler = |s: &mut Self| WrapperMissing {
            pattern: start.to_str(s.lexer.source()),
            value: around_the,
            loc: s.loc(),
        };
        Some(WrappedAst {
            start: self.expect(&start, handler)?,
            value: parser(self)?,
            end: self.expect(end, handler)?,
        })
    }

    fn name(&mut self, hint: &'static str) -> Option<NameAst<M>> {
        if !self.at(TokenKind::Ident) {
            self.error(ExpectedName {
                hint,
                got: self.current.kind,
                loc: self.loc(),
            })?;
        }

        Some(self.name_unchecked())
    }

    fn name_unchecked(&mut self) -> NameAst<M> {
        let ident = self.interner.intern(self.lexer.span_str(self.current.span));
        let source_info = self.advance();
        NameAst { ident, source_info }
    }

    fn vis(&mut self) -> Option<VisAst<M>> {
        let vis = match self.current.kind {
            Tk::Pub => Vis::Pub,
            Tk::Priv => Vis::Priv,
            _ => return None,
        };

        Some(VisAst {
            vis,
            source_meta: self.advance(),
        })
    }

    //////////////
    // checking //
    //////////////

    fn skip(&mut self, pat: impl TokenPattern) -> Option<SourceInfo<M>> {
        let mut source_meta = None;
        while let Some(current) = self.try_advance(&pat) {
            match source_meta {
                None => source_meta = Some(current),
                Some(ref mut source_meta) => source_meta.meta = current.meta,
            }
        }
        source_meta
    }

    fn span_str(&self, span: Span) -> &str {
        self.lexer.span_str(span)
    }

    fn try_advance_ignore_lines(&mut self, pat: impl TokenPattern) -> Option<SourceInfo<M>> {
        if !self.reduce_repetition(Tk::NewLine) {
            return self.try_advance(pat);
        }

        if self.next_at(pat) {
            self.advance();
            Some(self.advance())
        } else {
            None
        }
    }

    fn reduce_repetition(&mut self, pat: impl TokenPattern) -> bool {
        if !self.at(&pat) {
            return false;
        }

        while self.next_at(&pat) {
            self.advance();
        }

        true
    }

    fn append_newlines(&mut self, to: &mut M) {
        if self.reduce_repetition(Tk::NewLine) {
            *to = self.advance().meta;
        }
    }

    fn at(&self, pattern: impl TokenPattern) -> bool {
        pattern.matches(self.lexer.source(), self.current.kind, self.current.span)
    }

    fn next_at(&self, pattern: impl TokenPattern) -> bool {
        pattern.matches(self.lexer.source(), self.next.kind, self.next.span)
    }

    fn expect<E: CtlError>(
        &mut self,
        pattern: impl TokenPattern,
        error: impl FnOnce(&mut Self) -> E,
    ) -> Option<SourceInfo<M>> {
        #[cold]
        #[inline(never)]
        fn fail<'ctx, 'arena, E: CtlError, M: TokenMeta>(
            s: &mut Parser<'ctx, 'arena, M>,
            error: impl FnOnce(&mut Parser<'ctx, 'arena, M>) -> E,
        ) -> Option<!> {
            let err = error(s);
            s.error(err)
        }

        if !self.at(pattern) {
            fail(self, error)?
        }

        Some(self.advance())
    }

    fn try_advance(&mut self, pattern: impl TokenPattern) -> Option<SourceInfo<M>> {
        if self.at(pattern) {
            Some(self.advance())
        } else {
            None
        }
    }

    fn advance(&mut self) -> SourceInfo<M> {
        let current = mem::replace(&mut self.state.next, self.lexer.next_tok());
        mem::replace(&mut self.current, current).source_meta()
    }

    ////////////
    // errors //
    ////////////

    fn recover_list(
        &mut self,
        sep: impl TokenPattern,
        end: impl TokenPattern,
    ) -> Option<Option<SourceInfo<M>>> {
        loop {
            if self.at(TokenKind::Eof) {
                return None;
            }

            if self.at(&end) {
                return Some(Some(self.advance()));
            }

            if self.at(&sep) {
                self.advance();
                return Some(None);
            }

            self.skip_nesting()?;

            self.advance();
        }
    }

    fn skip_nesting(&mut self) -> Option<()> {
        let mut pair_stack = bumpvec![];
        loop {
            if let Some(complement) = self.state.current.kind.complement() {
                pair_stack.push((self.advance(), complement));
                continue;
            }

            let Some(&(meta, kind)) = pair_stack.last() else {
                return Some(());
            };

            if self.at(kind) {
                pair_stack.pop();

                // we don't want to take the last token
                if pair_stack.is_empty() {
                    return Some(());
                }

                self.advance();
                continue;
            }

            if !self.state.current.kind.is_closing() {
                self.advance();
                continue;
            }

            self.workspace.push(UnmatchedParen {
                token_kind: kind,
                previous_paren: meta.span,
                contradictor: self.state.current.span,
                source: self.source,
            })?;
        }
    }

    fn loc(&self) -> SourceLoc {
        SourceLoc {
            origin: self.source,
            span: self.current.span,
        }
    }

    fn error(&mut self, error: impl CtlError) -> Option<!> {
        self.workspace.push(error)
    }
}

impl<M> Deref for Parser<'_, '_, M> {
    type Target = ParserCtx<M>;

    fn deref(&self) -> &Self::Target {
        self.state
    }
}

impl<M> DerefMut for Parser<'_, '_, M> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.state
    }
}

pub struct ParserCtx<M = NoTokenMeta> {
    current: Token<M>,
    next: Token<M>,
    progress: usize,
}

impl<M: TokenMeta> ParserCtx<M> {
    pub fn new(source: &str) -> Self {
        let mut lexer = Lexer::new(source, 0);
        Self {
            current: lexer.next_tok(),
            next: lexer.next_tok(),
            progress: lexer.progress(),
        }
    }
}

ctl_errors! {
    #[err => "expected start of {ast_name} but found {found}"]
    #[info => "{ast_name} can only start with: {expected}"]
    error ExpectedStartOfAst: fatal {
        #[err loc]
        ast_name: &'static str,
        found: TokenKind,
        expected ref: String,
        loc: SourceLoc,
    }

    #[err => "expected {expected_sep} or {expected_end} but found {found}"]
    #[info => "{ast_name} can only be separated by {expected_sep} or ended by {expected_end}"]
    error ExpectedListSep: fatal {
        #[err loc]
        ast_name: &'static str,
        found: TokenKind,
        expected_sep ref: String,
        expected_end ref: String,
        loc: SourceLoc,
    }

    #[err => "expected name of {hint} but got {got}"]
    error ExpectedName: fatal {
        #[err loc]
        hint: &'static str,
        got: TokenKind,
        loc: SourceLoc,
    }

    #[err => "expected {pattern} as the wrapper around {value}"]
    error WrapperMissing: fatal {
        #[err loc]
        pattern ref: String,
        value: &'static str,
        loc: SourceLoc,
    }

    #[err => "unmatched {token_kind}"]
    #[info => "paired tokens ('()[]{{}}') must always be balanced"]
    error UnmatchedParen: fatal {
        #[info source, previous_paren, "the starting {token_kind} is located here"]
        #[info source, contradictor, "this token would create imbalance"]
        token_kind: TokenKind,
        previous_paren: Span,
        contradictor: Span,
        source: VRef<Source>,
    }

    #[err => "expected ':' but found '{found}' when parsing {something}"]
    #[info => "colons are required to maintain common stile and readability"]
    error MissingColon: fatal {
        #[err loc]
        something: &'static str,
        found: TokenKind,
        loc: SourceLoc,
    }
}
