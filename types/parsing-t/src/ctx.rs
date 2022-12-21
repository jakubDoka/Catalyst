use std::{fmt, marker::PhantomData};

use crate::*;
use diags::*;
use lexing::*;
use lexing_t::*;
use packaging_t::*;
use storage::*;

pub struct ParsingCtx<'ctx, 'ast: 'ctx, 'macros: 'ctx> {
    pub source_code: &'ctx str,
    pub lexer: Lexer<'ctx>,
    pub state: &'ctx mut ParsingState,
    pub arena: &'ast AstData,
    pub workspace: &'ctx mut Workspace,
    pub interner: &'ctx mut Interner,
    pd: PhantomData<&'macros ()>, // we will have macro references eventually
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
            source_code,
            lexer: Lexer::new(source_code, state.progress),
            state,
            arena: ast_data,
            workspace,
            interner,
            pd: PhantomData,
            source,
        }
    }

    // pub fn new_with_macros(
    //     source_code: &'ctx str,
    //     state: &'ctx mut ParsingState,
    //     ast_data: &'ast AstData,
    //     workspace: &'ctx mut Workspace,
    //     interner: &'ctx mut Interner,
    //     token_macro_ctx: Option<&'ctx TokenMacroCtx<'macros>>,
    //     source: VRef<Source>,
    // ) -> Self {
    //     Self {
    //         source_code,
    //         lexer: CtlLexer::Base(Lexer::new(source_code, state.progress)),
    //         state,
    //         arena: ast_data,
    //         workspace,
    //         interner,
    //         token_macro_ctx,
    //         source,
    //     }
    // }
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
        if self.at(pat) {
            while self.at_next(pat) {
                self.advance();
            }
            true
        } else {
            false
        }
    }

    pub fn try_advance_ignore_lines(&mut self, pat: TokenKind) -> Option<Token> {
        if self.at(pat) {
            return Some(self.advance());
        }
        self.reduce_repetition(TokenKind::NewLine);
        if self.at_next(pat) {
            self.advance();
            Some(self.advance())
        } else {
            None
        }
    }

    pub fn advance(&mut self) -> Token {
        let current = self.state.current;
        self.state.current = self.state.next;
        self.state.next = self.lexer.next_tok();
        current
    }

    pub fn expect_advance<T: TokenPattern, E: CtlError>(
        &mut self,
        expected: T,
        handler: impl FnOnce(&mut Self) -> E,
    ) -> Option<Token> {
        if self.at(expected) {
            Some(self.advance())
        } else {
            let err = handler(self);
            self.workspace.push(err);
            None
        }
    }

    pub fn try_advance(&mut self, kind: impl TokenPattern) -> Option<Token> {
        self.at(kind).then(|| self.advance())
    }

    pub fn loc(&self) -> SourceLoc {
        SourceLoc {
            origin: self.source,
            span: self.state.current.span,
        }
    }

    pub fn skip(&mut self, tok: TokenKind) {
        while self.state.current.kind == tok {
            self.advance();
        }
    }

    pub fn recover(&mut self, terminals: impl TokenPattern) -> Option<Token> {
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
                    self.workspace.push(UnmatchedParen {
                        token_kind: kind,
                        previous_paren: span,
                        contradictor: self.state.current.span,
                        source: self.source,
                    })?;
                }
                self.advance();
                continue;
            }

            if self.matches(&terminals, self.state.current) {
                let cur = self.state.current;
                self.advance();
                return Some(cur);
            }

            if self.at(TokenKind::Eof) {
                self.workspace.push(FailedRecoveryEof {
                    source: self.source,
                    recovery_symbols: terminals.to_str(self),
                    eof: self.state.current.span,
                })?;
            }

            self.advance();
        }
    }

    pub fn matches(&self, terminals: impl TokenPattern, token: Token) -> bool {
        terminals.matches(self, token)
    }

    pub fn at(&self, terminals: impl TokenPattern) -> bool {
        self.matches(terminals, self.state.current)
    }

    pub fn at_next(&self, terminals: impl TokenPattern) -> bool {
        self.matches(terminals, self.state.next)
    }

    pub fn current_token_str(&self) -> &str {
        self.inner_span_str(self.state.current.span)
    }

    pub fn inner_span_str(&self, span: Span) -> &str {
        &self.source_code[span.range()]
    }

    pub fn name_unchecked(&mut self) -> NameAst {
        let span = self.advance().span;
        NameAst::new(self, span)
    }

    // gen_error_fns! {
    //     push expect_error(self, kinds: impl IntoIterator<Item = impl Into<TokenPat>> + Clone) {
    //         err: (
    //             "expected {} but got {}",
    //             kinds.into_iter().map(|k| k.into().as_string()).collect::<BumpVec<_>>().join(" | "),
    //             self.state.current.kind.as_str(),
    //         );
    //         info: ("{}", self.display_parse_stack());
    //         (self.state.current.span, self.source) {
    //             err[self.state.current.span]: "token located here";
    //         }
    //     }

    //     push expect_str_error(self, strings: &[&str]) {
    //         err: (
    //             "expected '{}' but got '{}'",
    //             strings.join("' | '"),
    //             self.current_token_str(),
    //         );
    //         info: ("{}", self.display_parse_stack());
    //         (self.state.current.span, self.source) {
    //             err[self.state.current.span]: "token located here";
    //         }
    //     }

    //     push unmatched_paren(self, kind: TokenKind, span: Span) {
    //         err: ("unmatched paren {}", kind.as_str());
    //         info: ("{}", self.display_parse_stack());
    //         (span.joined(self.state.current.span), self.source) {
    //             err[span]: "the starting paren";
    //         }
    //     }

    //     push invalid_struct_constructor_type(self, span: Span) {
    //         err: "invalid struct constructor type";
    //         help: ("{}", concat!(
    //             "this part of the constructor has same syntax as type, with a little",
    //             " difference which is a '\\' between path and generic parameters"
    //         ));
    //         info: ("{}", self.display_parse_stack());
    //         (span, self.source) {
    //             err[span]: "this is invalid";
    //         }
    //     }

    //     push invalid_instance_path(self, path: Span) {
    //         err: "invalid path for instance";
    //         help: "valid syntax for path in instance is `<module>::<item> | <item>`";
    //         (path, self.source) {
    //             err[path]: "this is invalid";
    //         }
    //     }

    //     push invalid_typed_path(self, path: Span) {
    //         err: "invalid path for type";
    //         help: "valid syntax for path in type is `<module>::<item>::[<generics>...] | <item>::[<generics>...]`";
    //         (path, self.source) {
    //             err[path]: "this is invalid";
    //         }
    //     }

    //     push invalid_spec_syntax(self, span: Span) {
    //         err: "invalid syntax for spec";
    //         help: "valid syntax of spec is only `<path> | <path>[<type>, ..]";
    //         (span, self.source) {
    //             err[span]: "this is invalid";
    //         }
    //     }
    // }
}

ctl_errors! {
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

    #[err => "expected one of {recovery_symbols} but file already ended"]
    error FailedRecoveryEof: fatal {
        #[info source, eof, "file ends here"]
        recovery_symbols ref: String,
        eof: Span,
        source: VRef<Source>,
    }
}

#[derive(Default)]
pub struct ParsingState {
    pub current: Token,
    pub next: Token,
    pub progress: usize,
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
