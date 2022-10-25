use std::{default::default, fmt};

use diags::*;
use lexing::*;
use lexing_t::*;
use packaging_t::Source;

use storage::*;

use crate::*;

pub struct ParsingCtx<'ctx, 'ast: 'ctx, 'macros: 'ctx> {
    pub lexer: Lexer<'ctx>,
    pub state: &'ctx mut ParsingState,
    pub arena: &'ast AstData,
    pub workspace: &'ctx mut Workspace,
    pub interner: &'ctx mut Interner,
    pub token_macro_ctx: Option<&'ctx mut TokenMacroCtx<'macros>>,
    pub token_macro_stack: BumpVec<TokenMacro<'macros>>,
    pub source: VRef<Source>,
}

impl<'ctx, 'ast, 'macros> ParsingCtx<'ctx, 'ast, 'macros> {
    pub fn new(
        source_code: &'ctx str,
        state: &'ctx mut ParsingState,
        ast_data: &'ast AstData,
        workspace: &'ctx mut Workspace,
        interner: &'ctx mut Interner,
        source: VRef<Source>,
    ) -> Self {
        Self {
            lexer: Lexer::new(source_code, state.progress),
            state,
            arena: ast_data,
            workspace,
            interner,
            token_macro_ctx: None,
            token_macro_stack: default(),
            source,
        }
    }

    pub fn new_with_macros(
        source_code: &'ctx str,
        state: &'ctx mut ParsingState,
        ast_data: &'ast AstData,
        workspace: &'ctx mut Workspace,
        interner: &'ctx mut Interner,
        token_macro_ctx: Option<&'ctx mut TokenMacroCtx<'macros>>,
        source: VRef<Source>,
    ) -> Self {
        Self {
            lexer: Lexer::new(source_code, state.progress),
            state,
            arena: ast_data,
            workspace,
            interner,
            token_macro_ctx,
            token_macro_stack: default(),
            source,
        }
    }
}

impl Drop for ParsingCtx<'_, '_, '_> {
    fn drop(&mut self) {
        self.state.progress = self.lexer.progress();
    }
}

impl<'ast> ParsingCtx<'_, 'ast, '_> {
    pub fn visibility(&mut self) -> Vis {
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

    pub fn parse<T: Ast<'ast>>(&mut self) -> Option<T>
    where
        T::Args: Default,
    {
        T::parse(self)
    }

    pub fn parse_alloc<T: Ast<'ast>>(&mut self) -> Option<&'ast T>
    where
        T::Args: Default,
    {
        self.parse().map(|t| self.arena.alloc(t))
    }

    pub fn parse_args<T: Ast<'ast>>(&mut self, args: T::Args) -> Option<T> {
        T::parse_args(self, args)
    }

    pub fn parse_args_alloc<T: Ast<'ast>>(&mut self, args: T::Args) -> Option<&'ast T> {
        self.parse_args(args).map(|t| self.arena.alloc(t))
    }

    pub fn reduce_repetition(&mut self, pat: TokenKind) -> bool {
        if self.at_tok(pat) {
            while self.at_next_tok(pat) {
                self.advance();
            }
            true
        } else {
            false
        }
    }

    pub fn try_advance_ignore_lines(&mut self, pat: TokenKind) -> Option<Token> {
        if self.at_tok(pat) {
            return Some(self.advance());
        }
        self.reduce_repetition(TokenKind::NewLine);
        if self.at_next_tok(pat) {
            self.advance();
            Some(self.advance())
        } else {
            None
        }
    }

    pub fn advance(&mut self) -> Token {
        let current = self.state.current;
        self.state.current = self.state.next;
        self.state.next = self.next_token();
        self.check_for_macro();
        current
    }

    fn check_for_macro(&mut self) {
        if self.state.next.kind != TokenKind::Macro {
            return;
        }

        let Some(ref mut token_macro_ctx) = self.token_macro_ctx else {
            return;
        };

        let macro_name =
            &self.lexer.inner_span_str(self.state.next.span)[..self.state.next.span.len() - 1];
        let macro_name_ident = self.interner.intern(macro_name);

        let Some(mut token_macro) = token_macro_ctx.alloc(macro_name_ident) else {
            return;
        };

        if !token_macro.start(&mut self.lexer) {
            token_macro_ctx.free(token_macro);
            return;
        }

        self.token_macro_stack.push(token_macro);

        self.state.next = self.next_token();
    }

    fn next_token(&mut self) -> Token {
        let Some(mut token_macro) = self.token_macro_stack.pop() else {
            return self.lexer.next_tok();
        };

        let Some(token) = token_macro.next(&mut self.lexer) else {
            self.token_macro_ctx.as_mut().unwrap().free(token_macro);
            return self.lexer.next_tok();
        };

        self.token_macro_stack.push(token_macro);

        token
    }

    pub fn expect_advance(&mut self, expected: impl Into<TokenPat> + Clone) -> Option<Token> {
        if self.at(expected.clone().into()) {
            Some(self.advance())
        } else {
            let terminals = [expected.into()];
            self.expect_error(&terminals);
            None
        }
    }

    pub fn optional_advance(&mut self, kind: impl Into<TokenPat>) -> Option<Token> {
        if self.at([kind.into()]) {
            Some(self.advance())
        } else {
            None
        }
    }

    pub fn try_advance(&mut self, kind: TokenKind) -> Option<Token> {
        (self.state.current.kind == kind).then(|| self.advance())
    }

    pub fn skip(&mut self, tok: TokenKind) {
        while self.state.current.kind == tok {
            self.advance();
        }
    }

    pub fn recover(
        &mut self,
        terminals: impl IntoIterator<Item = impl AsRef<TokenPat>> + Clone,
    ) -> Option<Token> {
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
                    return None;
                }
                self.advance();
                continue;
            }

            if self.matches(terminals.clone(), self.state.current) {
                let cur = self.state.current;
                self.advance();
                return Some(cur);
            }

            if self.at_tok(TokenKind::Eof) {
                self.expect_error(terminals);
                return None;
            }

            self.advance();
        }
    }

    pub fn matches(
        &self,
        terminals: impl IntoIterator<Item = impl AsRef<TokenPat>>,
        token: Token,
    ) -> bool {
        terminals.into_iter().any(|pat| match *pat.as_ref() {
            TokenPat::Kind(kind) => kind == token.kind,
            TokenPat::Str(lit) => lit == self.lexer.inner_span_str(token.span),
        })
    }

    pub fn at_tok(&self, tok: TokenKind) -> bool {
        self.state.current.kind == tok
    }

    pub fn at_next_tok(&self, tok: TokenKind) -> bool {
        self.state.next.kind == tok
    }

    pub fn at(&self, terminals: impl IntoIterator<Item = impl AsRef<TokenPat>>) -> bool {
        self.matches(terminals, self.state.current)
    }

    pub fn next_at(&self, terminals: impl IntoIterator<Item = impl AsRef<TokenPat>>) -> bool {
        self.matches(terminals, self.state.next)
    }

    pub fn current_token_str(&self) -> &str {
        self.lexer.inner_span_str(self.state.current.span)
    }

    pub fn name_unchecked(&mut self) -> NameAst {
        let span = self.advance().span;
        NameAst::new(self, span)
    }

    gen_error_fns! {
        push expect_error(self, kinds: impl IntoIterator<Item = impl AsRef<TokenPat>> + Clone) {
            err: (
                "expected {} but got {}",
                kinds.into_iter().map(|k| k.as_ref().as_string()).collect::<BumpVec<_>>().join(" | "),
                self.state.current.kind.as_str(),
            );
            info: ("{}", self.display_parse_stack());
            (self.state.current.span, self.source) {
                err[self.state.current.span]: "token located here";
            }
        }

        push expect_str_error(self, strings: &[&str]) {
            err: (
                "expected '{}' but got '{}'",
                strings.join("' | '"),
                self.current_token_str(),
            );
            info: ("{}", self.display_parse_stack());
            (self.state.current.span, self.source) {
                err[self.state.current.span]: "token located here";
            }
        }

        push unmatched_paren(self, kind: TokenKind, span: Span) {
            err: ("unmatched paren {}", kind.as_str());
            info: ("{}", self.display_parse_stack());
            (span.joined(self.state.current.span), self.source) {
                err[span]: "the starting paren";
            }
        }

        push invalid_struct_constructor_type(self, span: Span) {
            err: "invalid struct constructor type";
            help: ("{}", concat!(
                "this part of the constructor has same syntax as type, with a little",
                " difference which is a '\\' between path and generic parameters"
            ));
            info: ("{}", self.display_parse_stack());
            (span, self.source) {
                err[span]: "this is invalid";
            }
        }

        push invalid_instance_path(self, path: Span) {
            err: "invalid path for instance";
            help: "valid syntax for path in instance is `<module>::<item> | <item>`";
            (path, self.source) {
                err[path]: "this is invalid";
            }
        }

        push invalid_typed_path(self, path: Span) {
            err: "invalid path for type";
            help: "valid syntax for path in type is `<module>::<item>::[<generics>...] | <item>::[<generics>...]`";
            (path, self.source) {
                err[path]: "this is invalid";
            }
        }

        push invalid_spec_syntax(self, span: Span) {
            err: "invalid syntax for spec";
            help: "valid syntax of spec is only `<path> | <path>[<type>, ..]";
            (span, self.source) {
                err[span]: "this is invalid";
            }
        }
    }

    pub fn display_parse_stack(&self) -> String {
        self.state
            .parse_stack
            .iter()
            .copied()
            .intersperse(" -> ")
            .collect()
    }
}

#[derive(Default)]
pub struct ParsingState {
    pub current: Token,
    pub next: Token,
    pub progress: usize,
    pub parse_stack: Vec<&'static str>,
}

impl ParsingState {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn start(&mut self, source: &str) {
        let mut lexer = Lexer::new(source, 0);
        self.current = lexer.next_tok();
        self.next = lexer.next_tok();
        self.progress = lexer.progress();
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Vis {
    Pub,
    None,
    Priv,
}

impl Vis {
    pub fn or(self, other: Self) -> Self {
        match (self, other) {
            (Vis::None, vis) => vis,
            (vis, ..) => vis,
        }
    }
}

impl fmt::Display for Vis {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Vis::Pub => write!(f, "pub"),
            Vis::Priv => write!(f, "priv"),
            Vis::None => write!(f, ""),
        }
    }
}

impl Default for Vis {
    fn default() -> Self {
        Vis::None
    }
}
