use diags::*;
use lexing::*;
use lexing_t::*;
use scope::*;
use storage::*;

use crate::*;

pub struct ParsingCtx<'a, 'b> {
    pub lexer: Lexer<'a>,
    pub state: &'a mut ParsingState,
    pub arena: &'b AstData,
    pub workspace: &'a mut Workspace,
    pub interner: &'a mut Interner,
}

impl<'a, 'b> ParsingCtx<'a, 'b> {
    pub fn new(
        source: &'a str,
        state: &'a mut ParsingState,
        ast_data: &'b AstData,
        workspace: &'a mut Workspace,
        interner: &'a mut Interner,
    ) -> Self {
        Self {
            lexer: Lexer::new(source, state.progress),
            state,
            arena: ast_data,
            workspace,
            interner,
        }
    }
}

impl Drop for ParsingCtx<'_, '_> {
    fn drop(&mut self) {
        self.state.progress = self.lexer.progress();
    }
}

impl<'a> ParsingCtx<'_, 'a> {
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

    pub fn parse<T: Ast<'a>>(&mut self) -> Option<T>
    where
        T::Args: Default,
    {
        T::parse(self)
    }

    pub fn parse_alloc<T: Ast<'a>>(&mut self) -> Option<&'a T>
    where
        T::Args: Default,
    {
        self.parse().map(|t| self.arena.alloc(t))
    }

    pub fn parse_args<T: Ast<'a>>(&mut self, args: T::Args) -> Option<T> {
        T::parse_args(self, args)
    }

    pub fn parse_args_alloc<T: Ast<'a>>(&mut self, args: T::Args) -> Option<&'a T> {
        self.parse_args(args).map(|t| self.arena.alloc(t))
    }

    pub fn advance(&mut self) -> Token {
        let current = self.state.current;
        self.state.current = self.state.next;
        self.state.next = self.lexer.next_tok();
        current
    }

    pub fn expect_advance(&mut self, kind: TokenKind) -> Option<Token> {
        let current = self.state.current;
        if current.kind == kind {
            Some(self.advance())
        } else {
            let terminals = [TokenPattern::Kind(kind)];
            self.expect_error(&terminals);
            None
        }
    }

    pub fn optional_advance(&mut self, kind: impl Into<TokenPattern>) -> Option<Token> {
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
        terminals: impl IntoIterator<Item = impl AsRef<TokenPattern>> + Clone,
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
        terminals: impl IntoIterator<Item = impl AsRef<TokenPattern>>,
        token: Token,
    ) -> bool {
        terminals.into_iter().any(|pat| match *pat.as_ref() {
            TokenPattern::Kind(kind) => kind == token.kind,
            TokenPattern::Str(lit) => lit == self.lexer.inner_span_str(token.span),
        })
    }

    pub fn at_tok(&self, tok: TokenKind) -> bool {
        self.state.current.kind == tok
    }

    pub fn at_next_tok(&self, tok: TokenKind) -> bool {
        self.state.next.kind == tok
    }

    pub fn at(&self, terminals: impl IntoIterator<Item = impl AsRef<TokenPattern>>) -> bool {
        self.matches(terminals, self.state.current)
    }

    pub fn next_at(&self, terminals: impl IntoIterator<Item = impl AsRef<TokenPattern>>) -> bool {
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
        push expect_error(self, kinds: impl IntoIterator<Item = impl AsRef<TokenPattern>> + Clone) {
            err: (
                "expected {} but got {}",
                kinds.into_iter().map(|k| k.as_ref().as_string()).collect::<BumpVec<_>>().join(" | "),
                self.state.current.kind.as_str(),
            );
            info: ("{}", self.display_parse_stack());
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
            info: ("{}", self.display_parse_stack());
            (self.state.current.span, self.state.path) {
                err[self.state.current.span]: "token located here";
            }
        }

        push unmatched_paren(self, kind: TokenKind, span: Span) {
            err: ("unmatched paren {}", kind.as_str());
            info: ("{}", self.display_parse_stack());
            (span.joined(self.state.current.span), self.state.path) {
                err[span]: "the starting paren";
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
    pub path: VRef<str>,
    pub parse_stack: Vec<&'static str>,
}

impl ParsingState {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn start(&mut self, source: &str, path: VRef<str>) {
        let mut lexer = Lexer::new(source, 0);
        self.current = lexer.next_tok();
        self.next = lexer.next_tok();
        self.progress = lexer.progress();
        self.path = path;
    }
}
