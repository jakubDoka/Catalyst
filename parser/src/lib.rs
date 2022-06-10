#![feature(generic_associated_types)]
#![feature(let_else)]

use ast::*;
use lexer::*;
use storage::*;

pub mod error;

pub use error::AstError;

pub struct Parser<'a> {
    next: Token,
    current: Token,
    sources: &'a Sources,
    diagnostics: &'a mut errors::Diagnostics,
    lexer: Lexer<'a>,
    data: &'a mut AstData,
    stack: &'a mut FramedStack<Ast>,
}

impl<'a> Parser<'a> {
    pub fn parse_imports(
        sources: &'a Sources,
        diagnostics: &'a mut errors::Diagnostics,
        ast_file: &'a mut AstData,
        temp: &'a mut FramedStack<Ast>,
        source: Source,
    ) -> InterState {
        let inter_state = InterState::new(source);
        Self::parse_with(
            sources,
            diagnostics,
            ast_file,
            temp,
            inter_state,
            Self::take_imports,
        )
        .unwrap()
    }

    pub fn parse_manifest(
        sources: &'a Sources,
        diagnostics: &'a mut errors::Diagnostics,
        ast_file: &'a mut AstData,
        temp: &'a mut FramedStack<Ast>,
        source: Source,
    ) {
        let inter_state = InterState::new(source);
        Self::parse_with(
            sources,
            diagnostics,
            ast_file,
            temp,
            inter_state,
            Self::take_manifest,
        );
    }

    pub fn parse_code_chunk(
        sources: &'a Sources,
        diagnostics: &'a mut errors::Diagnostics,
        ast_file: &'a mut AstData,
        temp: &'a mut FramedStack<Ast>,
        inter_state: InterState,
    ) -> Option<InterState> {
        Self::parse_with(
            sources,
            diagnostics,
            ast_file,
            temp,
            inter_state,
            Self::take_chunk,
        )
    }

    pub fn parse_with(
        sources: &'a Sources,
        diagnostics: &'a mut errors::Diagnostics,
        ast_file: &'a mut AstData,
        temp: &'a mut FramedStack<Ast>,
        InterState {
            mut next,
            mut current,
            progress,
            source,
        }: InterState,
        pfn: impl Fn(&mut Self) -> bool,
    ) -> Option<InterState> {
        let source_str = &sources[source].content;
        let mut lexer = Lexer::new(progress, source, source_str);
        if current.kind() == TokenKind::None {
            current = lexer.next_token();
            next = lexer.next_token();
        }

        let mut s = Self {
            next,
            current,
            sources,
            diagnostics,
            lexer,
            data: ast_file,
            stack: temp,
        };

        let unfinished = pfn(&mut s);

        if !unfinished {
            return None;
        }

        Some(InterState {
            next: s.next,
            current: s.current,
            progress: s.lexer.progress(),
            source,
        })
    }

    fn take_imports(&mut self) -> bool {
        self.skip_new_lines();
        if self.current.kind() != TokenKind::Use {
            return true;
        }

        let span = self.current.span();

        self.advance();
        self.skip_new_lines();

        self.expect(TokenKind::LeftCurly);

        self.stack.mark_frame();
        let end = self.list(
            TokenKind::LeftCurly,
            TokenKind::NewLine,
            TokenKind::RightCurly,
            Self::import,
        );
        let imports = self.alloc(AstKind::Imports, span.join(end));

        self.data.push(imports);

        true
    }

    fn take_manifest(&mut self) -> bool {
        self.stack.mark_frame();

        self.list(
            TokenKind::None,
            TokenKind::NewLine,
            TokenKind::Eof,
            Self::constructor_field,
        );

        for &item in self.stack.top_frame() {
            self.data.push(item);
        }

        self.stack.pop_frame();

        false
    }

    fn take_chunk(&mut self) -> bool {
        while self.current.kind() != TokenKind::Eof {
            let Some(item) = self.item() else {
                return true;
            };

            if item.is_reserved_value() {
                break;
            }
            self.data.push(item);
        }

        false
    }

    fn import(&mut self) -> Ast {
        let span = self.current.span();
        self.stack.mark_frame();
        if self.current.kind() == TokenKind::Ident {
            let ident = self.ident();
            self.stack.push(ident);
        } else {
            self.stack.push_default();
        }

        self.skip_new_lines();
        self.expect(TokenKind::String);
        let end = self.current.span();
        let path = self.literal_expr();
        self.stack.push(path);

        self.alloc(AstKind::Import, span.join(end))
    }

    fn item(&mut self) -> Option<Ast> {
        Some(loop {
            match self.current.kind() {
                TokenKind::Let => break self.variable(),
                TokenKind::Fn => break self.func(),
                TokenKind::Struct => break self.struct_decl(),
                TokenKind::NewLine => self.advance(),
                TokenKind::Eof => break Ast::reserved_value(),
                TokenKind::Bound => break self.bound(),
                TokenKind::Impl => break self.implementation(),
                TokenKind::Hash => break self.tag(),
                TokenKind::Enum => break self.enum_decl(),
                TokenKind::Break => {
                    self.advance();
                    return None;
                }
                _ => {
                    self.expect_many(&[
                        TokenKind::Fn,
                        TokenKind::Struct,
                        TokenKind::NewLine,
                        TokenKind::Eof,
                        TokenKind::Bound,
                        TokenKind::Impl,
                        TokenKind::Hash,
                        TokenKind::Enum,
                        TokenKind::Break,
                    ]);
                    self.advance();
                }
            }
        })
    }

    fn enum_decl(&mut self) -> Ast {
        let span = self.current.span();

        self.advance();
        self.stack.mark_frame();

        // generics will be handled later but we reserve them now
        self.stack.push_default();

        let ident = self.ident();
        self.stack.push(ident);

        let end = if self.current.kind() == TokenKind::LeftCurly {
            self.stack.mark_frame();
            let end = self.list(
                TokenKind::LeftCurly,
                TokenKind::NewLine,
                TokenKind::RightCurly,
                Self::enum_variant,
            );
            let variants = self.alloc(AstKind::EnumVariants, end);
            self.stack.push(variants);
            end
        } else {
            self.stack.push_default();
            Span::default()
        };

        self.alloc(AstKind::Enum, span.join(end))
    }

    fn enum_variant(&mut self) -> Ast {
        let span = self.current.span();
        self.stack.mark_frame();

        let ident = self.ident();
        self.stack.push(ident);

        let end = if self.current.kind() == TokenKind::Colon {
            self.advance();
            let expr = self.type_expr();
            self.stack.push(expr);
            self.data.nodes[expr].span
        } else {
            self.stack.push(ident);
            span
        };

        self.alloc(AstKind::EnumVariant, span.join(end))
    }

    fn tag(&mut self) -> Ast {
        let span = self.current.span();

        self.advance();

        self.stack.mark_frame();
        let expr = self.expr();
        self.stack.push(expr);

        let end = self.data.nodes[expr].span;

        self.alloc(AstKind::Tag, span.join(end))
    }

    fn implementation(&mut self) -> Ast {
        let span = self.current.span();
        self.advance();

        self.stack.mark_frame();

        let ident = self.ident();
        self.stack.push(ident);

        // handle bound impl
        let mut end = span;
        let parser = if self.current.kind() == TokenKind::As {
            self.advance();
            let ty = self.type_expr();
            self.stack.push(ty);
            end = self.data.nodes[ty].span;
            Self::bound_impl_item
        } else {
            self.stack.push_default();
            Self::func
        };

        // implementation can already map as a sugar
        if self.current.kind() == TokenKind::LeftCurly {
            self.stack.mark_frame();
            end = self.list(
                TokenKind::LeftCurly,
                TokenKind::NewLine,
                TokenKind::RightCurly,
                parser,
            );
            let functions = self.alloc(AstKind::ImplBody, span.join(end));
            self.stack.push(functions);
        } else {
            self.stack.push_default();
        }

        self.alloc(AstKind::Impl, span.join(end))
    }

    /// '<func> | <use_bound_func>'
    fn bound_impl_item(&mut self) -> Ast {
        match self.current.kind() {
            TokenKind::Fn => self.func(),
            TokenKind::Use => self.use_bound_func(),
            _ => {
                self.expect_many(&[TokenKind::Fn, TokenKind::Use]);
                let err = self.data.alloc_sonless(AstKind::Error, self.current.span());
                self.advance();
                err
            }
        }
    }

    /// 'use <simple_expr> as <ident>'
    fn use_bound_func(&mut self) -> Ast {
        let span = self.current.span();
        self.advance();

        self.stack.mark_frame();

        let func = self.ident_expr();
        self.stack.push(func);

        self.expect(TokenKind::As);
        self.advance();

        let ident = self.ident();
        self.stack.push(ident);

        let end = self.data.nodes[ident].span;

        self.alloc(AstKind::UseBoundFunc, span.join(end))
    }

    fn bound(&mut self) -> Ast {
        let span = self.current.span();
        self.advance();

        self.stack.mark_frame();

        let ident = self.ident();
        self.stack.push(ident);

        self.stack.mark_frame();
        let end = self.list(
            TokenKind::LeftCurly,
            TokenKind::NewLine,
            TokenKind::RightCurly,
            Self::func,
        );
        let body = self.alloc(AstKind::Block, span.join(end));
        self.stack.push(body);

        self.alloc(AstKind::Bound, span.join(end))
    }

    fn struct_decl(&mut self) -> Ast {
        self.stack.mark_frame();
        let span = self.current.span();

        self.advance();
        let generics = self.generics(false);
        self.stack.push(generics);

        let name = self.ident();
        self.stack.push(name);

        self.stack.mark_frame();

        let end = self.list(
            TokenKind::LeftCurly,
            TokenKind::NewLine,
            TokenKind::RightCurly,
            Self::sfield,
        );

        let fields = self.alloc(AstKind::StructBody, end);
        self.stack.push(fields);

        self.alloc(AstKind::Struct, span.join(end))
    }

    fn sfield(&mut self) -> Ast {
        let span = self.current.span();

        let used = self.current.kind() == TokenKind::Use;
        if used {
            self.advance();
        }

        self.stack.mark_frame();

        let name = self.ident();
        self.stack.push(name);

        self.expect(TokenKind::Colon);
        self.advance();

        let ty = self.type_expr();
        self.stack.push(ty);
        let end = self.data.nodes[ty].span;

        self.alloc(AstKind::SField(used), span.join(end))
    }

    fn _compute_next(&mut self) {
        if self.next.kind() == TokenKind::None {
            self.advance()
        }
    }

    fn advance(&mut self) {
        self.current = self.next;
        self.next = self.lexer.next_token();
    }

    fn func(&mut self) -> Ast {
        let span = self.current.span();
        self.advance();

        self.stack.mark_frame();

        let generics = self.generics(true);
        self.stack.push(generics);

        // call convention
        if self.current.kind() == TokenKind::String {
            let ast = self
                .data
                .alloc_sonless(AstKind::String, self.current.span());
            self.stack.push(ast);
            self.advance();
        } else {
            self.stack.push_default();
        }

        let name = self.ident();
        self.stack.push(name);

        // arguments
        let end = self.list(
            TokenKind::LeftParen,
            TokenKind::Comma,
            TokenKind::RightParen,
            Self::func_arg,
        );

        // return type
        if self.current.kind() == TokenKind::RightArrow {
            self.advance();
            let return_type = self.type_expr();
            self.stack.push(return_type);
        } else {
            self.stack.push_default();
        }

        // body
        let (end, external) = if self.current.kind() == TokenKind::LeftCurly {
            let body = self.block();
            self.stack.push(body);
            (self.data.nodes[body].span, false)
        } else if self.current.kind() == TokenKind::Extern {
            let end = self.current.span();
            self.advance();
            self.stack.push_default();
            (end, true)
        } else {
            self.stack.push_default();
            (end, false)
        };

        self.alloc(AstKind::Function(external), span.join(end))
    }

    fn func_arg(&mut self) -> Ast {
        let span = self.current.span();

        self.stack.mark_frame();

        let name = self.ident();
        self.stack.push(name);

        self.expect(TokenKind::Colon);
        self.advance();

        let ty = self.type_expr();
        self.stack.push(ty);

        let end = self.data.nodes[ty].span;

        self.alloc(AstKind::FunctionArgument, span.join(end))
    }

    fn generics(&mut self, has_bounds: bool) -> Ast {
        if self.current.kind() != TokenKind::LeftBracket {
            return Ast::reserved_value();
        }

        self.stack.mark_frame();

        let parser = if has_bounds {
            Self::generic_param
        } else {
            Self::ident
        };

        let span = self.list(
            TokenKind::LeftBracket,
            TokenKind::Comma,
            TokenKind::RightBracket,
            parser,
        );

        if self.stack.top_frame().is_empty() {
            self.stack.pop_frame();
            return Ast::reserved_value();
        }

        self.alloc(AstKind::Generics, span)
    }

    fn generic_param(&mut self) -> Ast {
        let span = self.current.span();

        self.stack.mark_frame();

        let name = self.ident();
        self.stack.push(name);

        let mut end = span;
        if self.current.kind() == TokenKind::Colon {
            self.advance();

            let ty = self.type_expr();
            self.stack.push(ty);
            end = self.data.nodes[ty].span;

            while self.sources.display(self.current.span()) == "+" {
                self.advance();
                self.skip_new_lines();
                let ty = self.type_expr();
                self.stack.push(ty);
                end = self.data.nodes[ty].span;
            }
        }

        self.alloc(AstKind::GenericParam, span.join(end))
    }

    fn type_expr(&mut self) -> Ast {
        match self.current.kind() {
            TokenKind::Fn => self.func_ptr_type(),
            TokenKind::Operator(..) => match self.sources.display(self.current.span()) {
                "^" => self.type_pointer_expr(),
                _ => unimplemented!(
                    "unhandled token as type expr:\n{}",
                    self.current.span().log(self.sources),
                ),
            },
            TokenKind::Ident => self.type_ident_expr(),
            _ => {
                let span = self.current.span();
                unimplemented!("unhandled token as type expr:\n{}", span.log(self.sources),)
            }
        }
    }

    fn func_ptr_type(&mut self) -> Ast {
        let span = self.current.span();
        self.advance();

        self.stack.mark_frame();

        if self.current.kind() == TokenKind::String {
            let ast = self
                .data
                .alloc_sonless(AstKind::String, self.current.span());
            self.stack.push(ast);
            self.advance();
        } else {
            self.stack.push_default();
        }

        let end = self.list(
            TokenKind::LeftParen,
            TokenKind::Comma,
            TokenKind::RightParen,
            Self::type_expr,
        );

        let end = if self.current.kind() == TokenKind::RightArrow {
            self.advance();
            let return_type = self.type_expr();
            self.stack.push(return_type);
            self.data.nodes[return_type].span
        } else {
            self.stack.push_default();
            end
        };

        self.alloc(AstKind::FuncPtr, span.join(end))
    }

    fn type_pointer_expr(&mut self) -> Ast {
        let span = self.current.span();
        self.advance();

        let mutable = self.current.kind() == TokenKind::Mut;
        if mutable {
            self.advance();
        }

        self.stack.mark_frame();

        let ty = self.type_expr();
        self.stack.push(ty);

        self.alloc(AstKind::Ref(mutable), span.join(self.data.nodes[ty].span))
    }

    fn literal_expr(&mut self) -> Ast {
        let span = self.current.span();
        let kind = match self.current.kind() {
            TokenKind::Int => AstKind::Int,
            TokenKind::String => AstKind::String,
            TokenKind::Bool => AstKind::Bool,
            TokenKind::Char => AstKind::Char,
            _ => {
                let span = self.current.span();
                unimplemented!(
                    "unhandled token as literal expr:\n{}",
                    span.log(self.sources),
                )
            }
        };
        self.advance();
        self.data.alloc_sonless(kind, span)
    }

    fn block(&mut self) -> Ast {
        let span = self.current.span();
        self.stack.mark_frame();
        self.expect(TokenKind::LeftCurly);
        let end = self.list(
            TokenKind::LeftCurly,
            TokenKind::NewLine,
            TokenKind::RightCurly,
            Self::expr,
        );
        self.alloc(AstKind::Block, span.join(end))
    }

    fn expr(&mut self) -> Ast {
        let prev = self.simple_expr();
        if let TokenKind::Operator(precedence) = self.current.kind() {
            self.composite_expr(prev, precedence)
        } else {
            prev
        }
    }

    fn composite_expr(&mut self, mut prev: Ast, prev_precedence: u8) -> Ast {
        while let TokenKind::Operator(..) = self.current.kind() {
            let op_span = self.current.span();
            self.advance();

            let mut expr = self.simple_expr();

            let precedence = match self.current.kind() {
                TokenKind::Operator(precedence) => precedence,
                _ => u8::MAX,
            };

            if precedence < prev_precedence {
                expr = self.composite_expr(expr, precedence);
            }

            let span = {
                let start = self.data.nodes[prev].span;
                let end = self.data.nodes[expr].span;
                start.join(end)
            };

            let op = if prev_precedence == EQUAL_SIGN_PRECEDENCE && op_span.len() > 1 {
                // transforms `a += b` into `a = a + b` for any operator ending with `=` except `==` and `!=`
                let op = {
                    let span = op_span.slice(..op_span.len() - 1);
                    let ent = AstEnt::childless(AstKind::Ident, span);
                    self.data.nodes.push(ent)
                };

                let equal = {
                    let span = op_span.slice(op_span.len() - 1..);
                    let ent = AstEnt::childless(AstKind::Ident, span);
                    self.data.nodes.push(ent)
                };

                self.stack.mark_frame();
                self.stack.push(prev);
                self.stack.push(op);
                self.stack.push(expr);
                expr = self.alloc(AstKind::Binary, span);

                equal
            } else {
                let ent = AstEnt::childless(AstKind::Ident, op_span);
                self.data.nodes.push(ent)
            };

            self.stack.mark_frame();
            self.stack.push(prev);
            self.stack.push(op);
            self.stack.push(expr);

            prev = self.alloc(AstKind::Binary, span);
        }

        prev
    }

    fn simple_expr(&mut self) -> Ast {
        let span = self.current.span();
        let result = match self.current.kind() {
            TokenKind::Return => self.return_expr(),
            TokenKind::Ident => self.ident_expr(),
            TokenKind::Int | TokenKind::String | TokenKind::Bool | TokenKind::Char => {
                self.literal_expr()
            }
            TokenKind::If => self.if_expr(),
            TokenKind::LeftCurly => self.block(),
            TokenKind::Let => self.variable(),
            TokenKind::Loop => self.loop_expr(),
            TokenKind::Break => self.break_expr(),
            TokenKind::Continue => self.continue_expr(),
            TokenKind::Operator(..) => self.unary(),
            TokenKind::LeftParen => self.paren_expr(),
            TokenKind::Match => self.match_expr(),
            _ => {
                self.expect_many(&[
                    TokenKind::Return,
                    TokenKind::Ident,
                    TokenKind::Int,
                    TokenKind::String,
                    TokenKind::Bool,
                    TokenKind::Char,
                    TokenKind::If,
                    TokenKind::LeftCurly,
                    TokenKind::Let,
                    TokenKind::Loop,
                    TokenKind::Break,
                    TokenKind::Continue,
                    TokenKind::Operator(0),
                    TokenKind::LeftParen,
                    TokenKind::Match,
                ]);
                return self.data.alloc_sonless(AstKind::Error, span);
            }
        };

        self.handle_tail_expr(span, result)
    }

    fn continue_expr(&mut self) -> Ast {
        let span = self.current.span();
        self.advance();
        self.stack.mark_frame();
        self.label();
        self.alloc(AstKind::Continue, span)
    }

    fn match_expr(&mut self) -> Ast {
        let span = self.current.span();
        self.advance();

        self.stack.mark_frame();

        let expr = self.expr();
        self.stack.push(expr);

        self.expect(TokenKind::LeftCurly);

        let end = {
            self.stack.mark_frame();

            let end = self.list(
                TokenKind::LeftCurly,
                TokenKind::NewLine,
                TokenKind::RightCurly,
                Self::match_arm,
            );

            let body = self.alloc(AstKind::MatchBody, end);
            self.stack.push(body);

            end
        };

        self.alloc(AstKind::Match, span.join(end))
    }

    fn match_arm(&mut self) -> Ast {
        let span = self.current.span();
        self.stack.mark_frame();

        let pattern = self.pattern();
        self.stack.push(pattern);

        self.expect(TokenKind::ThickRightArrow);
        self.advance();

        let expr = self.expr();
        self.stack.push(expr);
        let end = self.data.nodes[expr].span;

        self.alloc(AstKind::MatchArm, span.join(end))
    }

    fn pattern(&mut self) -> Ast {
        let span = self.current.span();
        match self.current.kind() {
            TokenKind::Ident => self.ident_pattern(),
            TokenKind::Int | TokenKind::String | TokenKind::Bool | TokenKind::Char => {
                self.literal_expr()
            }
            _ => {
                self.advance();
                self.expect_many(&[
                    TokenKind::Ident,
                    TokenKind::Int,
                    TokenKind::String,
                    TokenKind::Bool,
                    TokenKind::Char,
                ]);
                self.data.alloc_sonless(AstKind::Error, span)
            }
        }
    }

    fn ident_pattern(&mut self) -> Ast {
        let path = self.path();
        let span = self.data.nodes[path].span;

        if self.current.kind() == TokenKind::DoubleColon {
            if self.next.kind() == TokenKind::LeftCurly {
                self.advance();
                self.stack.mark_frame();
                self.stack.push(path);

                self.stack.mark_frame();
                let end = self.list(
                    TokenKind::LeftCurly,
                    TokenKind::Comma,
                    TokenKind::RightCurly,
                    Self::pattern_field,
                );

                let body = self.alloc(AstKind::StructBody, end);
                self.stack.push(body);

                return self.alloc(AstKind::StructPattern, span.join(end));
            }

            if self.next.kind() == TokenKind::LeftParen {
                self.advance();
                self.stack.mark_frame();
                self.stack.push(path);

                self.stack.mark_frame();
                let end = self.list(
                    TokenKind::LeftParen,
                    TokenKind::Comma,
                    TokenKind::RightParen,
                    Self::pattern,
                );

                let body = self.alloc(AstKind::TupleStructBody, end);
                self.stack.push(body);

                return self.alloc(AstKind::TupleStructPattern, span.join(end));
            }
        }

        path
    }

    fn pattern_field(&mut self) -> Ast {
        let span = self.current.span();
        self.stack.mark_frame();

        let name = self.ident();
        self.stack.push(name);

        let end = if self.current.kind() == TokenKind::Colon {
            self.advance();
            let pattern = self.pattern();
            self.stack.push(pattern);
            self.data.nodes[pattern].span
        } else {
            self.stack.push_default();
            span
        };

        self.alloc(AstKind::StructPatternField, span.join(end))
    }

    fn handle_tail_expr(&mut self, span: Span, mut result: Ast) -> Ast {
        loop {
            self.stack.mark_frame();
            self.stack.push(result);
            result = match self.current.kind() {
                TokenKind::As => {
                    self.advance();
                    let ty = self.type_expr();
                    self.stack.push(ty);
                    let end = self.data.nodes[ty].span;
                    self.alloc(AstKind::BitCast, span.join(end))
                }
                TokenKind::Dot => {
                    self.advance();
                    // can be the tuple field
                    self.expect_many(&[TokenKind::Ident, TokenKind::Int]);
                    let field = self.path();
                    self.stack.push(field);
                    self.alloc(AstKind::DotExpr, span)
                }
                TokenKind::LeftParen => {
                    let end = self.list(
                        TokenKind::LeftParen,
                        TokenKind::Comma,
                        TokenKind::RightParen,
                        Self::expr,
                    );

                    self.alloc(AstKind::Call, span.join(end))
                }
                TokenKind::LeftBracket => {
                    self.advance();
                    let index = self.expr();
                    self.expect(TokenKind::RightBracket);
                    let end = self.current.span();
                    self.advance();
                    self.stack.push(index);
                    self.alloc(AstKind::Index, span.join(end))
                }
                _ => {
                    self.pop_frame();
                    break;
                }
            };
        }

        result
    }

    fn paren_expr(&mut self) -> Ast {
        self.advance();

        let expr = self.expr();

        self.expect_many(&[TokenKind::RightParen, TokenKind::Comma]);

        if self.current.kind() == TokenKind::Comma {
            unimplemented!("tuples are not yet supported");
        } else {
            self.advance();
        }

        expr
    }

    fn unary(&mut self) -> Ast {
        let span = self.current.span();

        match self.sources.display(span) {
            "*" => return self.deref(),
            "^" => return self.r#ref(),
            _ => (),
        }

        let op = {
            let ent = AstEnt::childless(AstKind::Ident, span);
            self.data.nodes.push(ent)
        };

        self.advance();

        let expr = self.simple_expr();
        let end = self.data.nodes[expr].span;

        self.stack.mark_frame();
        self.stack.push(op);
        self.stack.push(expr);
        self.alloc(AstKind::Unary, span.join(end))
    }

    fn r#ref(&mut self) -> Ast {
        let span = self.current.span();
        self.advance();

        let mutable = self.current.kind() == TokenKind::Mut;
        if mutable {
            self.advance();
        }

        let expr = self.simple_expr();
        let end = self.data.nodes[expr].span;

        self.stack.mark_frame();
        self.stack.push(expr);
        self.alloc(AstKind::Ref(mutable), span.join(end))
    }

    fn deref(&mut self) -> Ast {
        let span = self.current.span();
        self.advance();

        let expr = self.simple_expr();
        let end = self.data.nodes[expr].span;

        self.stack.mark_frame();
        self.stack.push(expr);
        self.alloc(AstKind::Deref, span.join(end))
    }

    fn break_expr(&mut self) -> Ast {
        let span = self.current.span();
        self.advance();

        self.stack.mark_frame();

        self.label();

        let end = if self.current.kind() == TokenKind::NewLine {
            self.stack.push_default();
            span
        } else {
            let value = self.expr();
            self.stack.push(value);
            self.data.nodes[value].span
        };

        self.alloc(AstKind::Break, span.join(end))
    }

    fn loop_expr(&mut self) -> Ast {
        let span = self.current.span();
        self.advance();

        self.stack.mark_frame();

        self.label();

        let body = self.block();
        self.stack.push(body);
        let end = self.data.nodes[body].span;

        self.alloc(AstKind::Loop, span.join(end))
    }

    fn label(&mut self) {
        if self.current.kind() == TokenKind::Label {
            let label = self.data.alloc_sonless(AstKind::Label, self.current.span());
            self.stack.push(label);
            self.advance();
        } else {
            self.stack.push_default();
        }
    }

    fn variable(&mut self) -> Ast {
        let span = self.current.span();
        self.advance();

        let mutable = self.current.kind() == TokenKind::Mut;
        if mutable {
            self.advance();
        }

        self.stack.mark_frame();

        let name = self.ident();
        self.stack.push(name);

        {
            let span = self.current.span();
            let str = self.sources.display(span);
            if str != "=" {
                self.diagnostics.push(AstError::ExpectedAssign {
                    got: self.current.kind(),
                    loc: span,
                });
            }
            self.advance();
        }

        let value = self.expr();
        self.stack.push(value);
        let end = self.data.nodes[value].span;

        self.alloc(AstKind::Variable(mutable), span.join(end))
    }

    fn if_expr(&mut self) -> Ast {
        self.stack.mark_frame();
        let span = self.current.span();
        self.advance();

        let cond = self.expr();
        let then = self.block();
        let otherwise = if self.current.kind() == TokenKind::Else {
            self.advance();
            if self.current.kind() == TokenKind::If {
                self.stack.mark_frame();
                let expr = self.if_expr();
                self.stack.push(expr);
                let end = self.data.nodes[expr].span;
                self.alloc(AstKind::Block, end)
            } else {
                self.block()
            }
        } else {
            Ast::reserved_value()
        };

        self.stack.push(cond);
        self.stack.push(then);
        self.stack.push(otherwise);

        let end = if otherwise.is_reserved_value() {
            self.data.nodes[then].span
        } else {
            self.data.nodes[otherwise].span
        };

        self.alloc(AstKind::If, span.join(end))
    }

    fn ident_expr(&mut self) -> Ast {
        let mut result = self.path();
        let span = self.data.nodes[result].span;

        // handle instantiation
        if self.current.kind() == TokenKind::DoubleColon
            && self.next.kind() == TokenKind::LeftBracket
        {
            self.stack.mark_frame();
            self.stack.push(result);

            self.advance();

            let end = self.list(
                TokenKind::LeftBracket,
                TokenKind::Comma,
                TokenKind::RightBracket,
                Self::type_expr,
            );

            result = self.alloc(AstKind::Instantiation, span.join(end));
        }

        // handle constructor
        if self.current.kind() == TokenKind::DoubleColon {
            if self.next.kind() == TokenKind::LeftCurly {
                self.advance();
                self.stack.mark_frame();
                self.stack.push(result);

                self.stack.mark_frame();
                let end = self.list(
                    TokenKind::LeftCurly,
                    TokenKind::NewLine,
                    TokenKind::RightCurly,
                    Self::constructor_field,
                );
                let body = self.alloc(AstKind::ConstructorBody, end);
                self.stack.push(body);

                return self.alloc(AstKind::Constructor, span.join(end));
            }

            if self.next.kind() == TokenKind::LeftParen {
                self.advance();
                self.stack.mark_frame();
                self.stack.push(result);

                self.stack.mark_frame();
                let end = self.list(
                    TokenKind::LeftParen,
                    TokenKind::Comma,
                    TokenKind::RightParen,
                    Self::expr,
                );
                let body = self.alloc(AstKind::TupleConstructorBody, end);
                self.stack.push(body);

                return self.alloc(AstKind::Constructor, span.join(end));
            }
        }

        result
    }

    fn path(&mut self) -> Ast {
        let mut span = self.current.span();
        let mut result = self.ident();

        self.stack.mark_frame();
        self.stack.push(result);
        while self.current.kind() == TokenKind::DoubleColon && self.next.kind() == TokenKind::Ident
        {
            self.advance();
            span = span.join(self.current.span());
            let ident = self.ident();
            self.stack.push(ident);
        }

        // we simplify one segment path into ident, this happens when
        // while loop does not run at all
        if self.stack.top_frame().len() == 1 {
            self.stack.pop_frame();
        } else {
            result = self.alloc(AstKind::Path, span);
        }

        result
    }

    fn return_expr(&mut self) -> Ast {
        let span = self.current.span();
        self.advance();
        self.stack.mark_frame();
        let end = if self.current.kind() == TokenKind::NewLine
            || self.current.kind() == TokenKind::Comma
        {
            self.stack.push_default();
            span
        } else {
            let expr = self.expr();
            self.stack.push(expr);
            self.data.nodes[expr].span
        };
        self.alloc(AstKind::Return, span.join(end))
    }

    fn constructor_field(&mut self) -> Ast {
        let span = self.current.span();
        let name = self.ident();
        self.stack.mark_frame();
        self.stack.push(name);

        let (expr, end) = if self.current.kind() == TokenKind::LeftCurly {
            // inferred branch
            self.stack.mark_frame();
            let end = self.list(
                TokenKind::LeftCurly,
                TokenKind::NewLine,
                TokenKind::RightCurly,
                Self::constructor_field,
            );
            let expr = self.alloc(AstKind::ConstructorBody, end);
            (expr, end)
        } else {
            // normal expr after colon
            self.expect(TokenKind::Colon);
            self.advance();
            let expr = self.expr();
            (expr, self.data.nodes[expr].span)
        };

        self.stack.push(expr);
        self.alloc(AstKind::ConstructorField, span.join(end))
    }

    fn type_ident_expr(&mut self) -> Ast {
        let span = self.current.span();
        let name = self.data.alloc_sonless(AstKind::Ident, span);
        self.advance();
        self.stack.mark_frame();
        self.stack.push(name);

        if self.current.kind() == TokenKind::LeftBracket {
            let end = self.list(
                TokenKind::LeftBracket,
                TokenKind::Comma,
                TokenKind::RightBracket,
                Self::type_expr,
            );

            return self.alloc(AstKind::Instantiation, span.join(end));
        }

        self.alloc(AstKind::Ident, span)
    }

    fn ident(&mut self) -> Ast {
        self.expect(TokenKind::Ident);
        let res = self.data.alloc_sonless(AstKind::Ident, self.current.span());
        self.advance();
        res
    }

    fn list(
        &mut self,
        start: TokenKind,
        sep: TokenKind,
        end: TokenKind,
        function_arg: impl Fn(&mut Self) -> Ast,
    ) -> Span {
        let span = self.current.span();
        if start != TokenKind::None {
            if start != self.current.kind() {
                return Span::default();
            }
            self.advance();
        }
        self.skip_new_lines();

        if end == self.current.kind() {
            let end = self.current.span();
            self.advance();
            return span.join(end);
        }

        loop {
            let ast = function_arg(self);
            self.stack.push(ast);
            let end_span = self.data.nodes[ast].span;

            if end == self.current.kind() {
                let end = self.current.span();
                self.advance();
                return span.join(end);
            } else if end == TokenKind::None && self.current.kind() != sep {
                return span.join(end_span);
            }

            self.expect(sep);
            self.advance();
            self.skip_new_lines();

            if end == self.current.kind() {
                let end = self.current.span();
                self.advance();
                return span.join(end);
            }
        }
    }

    fn skip_new_lines(&mut self) {
        while self.current.kind() == TokenKind::NewLine {
            self.advance();
        }
    }

    fn expect(&mut self, expected: TokenKind) {
        if self.current.kind() != expected {
            self.diagnostics.push(AstError::UnexpectedToken {
                got: self.current.kind(),
                expected: vec![expected],
                loc: self.current.span(),
            });
        }
    }

    fn alloc(&mut self, kind: AstKind, span: Span) -> Ast {
        let cons = self.pop_frame();
        self.data.alloc_ent(AstEnt::new(kind, cons, span))
    }

    fn pop_frame(&mut self) -> AstList {
        self.stack.save_and_pop_frame(&mut self.data.conns)
    }

    fn expect_many(&mut self, expected: &[TokenKind]) {
        if !expected.contains(&self.current.kind()) {
            self.diagnostics.push(AstError::UnexpectedToken {
                got: self.current.kind(),
                expected: expected.to_vec(),
                loc: self.current.span(),
            });
        }
    }
}

pub struct InterState {
    next: Token,
    current: Token,
    progress: usize,
    source: Source,
}

impl InterState {
    fn new(source: Source) -> Self {
        Self {
            next: Default::default(),
            current: Default::default(),
            progress: 0,
            source,
        }
    }
}
