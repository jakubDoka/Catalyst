pub mod control_flow;
pub mod pat;

use diags::*;
use packaging_t::Source;

use super::*;

impl<'ctx, 'arena, M: TokenMeta> Parser<'ctx, 'arena, M> {
    pub fn expr(&mut self) -> Option<ExprAst<'arena, M>> {
        let unit = self
            .unit_expr()
            .map(|e| self.arena.alloc(e))
            .map(ExprAst::Unit)?;
        self.binary_expr(unit, u8::MAX)
    }

    fn binary_expr(
        &mut self,
        mut lhs: ExprAst<'arena, M>,
        prev_precedence: u8,
    ) -> Option<ExprAst<'arena, M>> {
        Some(loop {
            let Tk::Operator(precedence) = self.state.current.kind else {
                break lhs;
            };

            if prev_precedence > precedence {
                let op = self.name_unchecked();
                self.skip(Tk::NewLine);
                let rhs = self
                    .unit_expr()
                    .map(|e| self.arena.alloc(e))
                    .map(ExprAst::Unit)?;
                let rhs = self.binary_expr(rhs, precedence)?;
                lhs = ExprAst::Binary(self.arena.alloc(BinaryExprAst { lhs, op, rhs }));
            } else {
                break lhs;
            }
        })
    }

    pub fn unit_expr(&mut self) -> Option<UnitExprAst<'arena, M>> {
        let mut unit = branch!(self => {
            Ident => self.path(None).map(UnitExprAst::Path),
            BackSlash => {
                if self.next_at(Tk::LeftBrace) {
                    let slash = self.advance();
                    self.struct_ctor(None, slash)
                        .map(UnitExprAst::StructCtor)
                } else {
                    let slash = self.advance();
                    self.path(Some(slash)).map(UnitExprAst::Path)
                }
            },
            Return => self.r#return().map(UnitExprAst::Return),
            Int => Some(UnitExprAst::Int(self.advance())),
            Float => Some(UnitExprAst::Float(self.advance())),
            Char => Some(UnitExprAst::Char(self.advance())),
            Bool => Some(UnitExprAst::Bool(self.advance())),
            Match => self.r#match().map(UnitExprAst::Match),
            If => self.r#if().map(UnitExprAst::If),
            Loop => self.r#loop().map(UnitExprAst::Loop),
            Break => self.r#break().map(UnitExprAst::Break),
            Continue => self.r#continue().map(UnitExprAst::Continue),
            Let => self.r#let().map(UnitExprAst::Let),
            Operator(_ = 0) => branch! {str self => {
                "*" => Some(UnitExprAst::Deref(self.advance(), self.unit_expr().map(|e| self.arena.alloc(e))?)),
                "^" => Some(UnitExprAst::Ref(self.advance(), self.mutability()?, self.unit_expr().map(|e| self.arena.alloc(e))?)),
                @ => self.workspace.push(TodoSnippet {
                    message: "unary operators are not yet implemented".into(),
                    loc: SourceLoc { origin: self.source, span: self.state.current.span}
                })?,
            }},
            LeftBrace => self.block("code block", Self::expr).map(UnitExprAst::Block),
            LeftBracket => self.array("array", Self::expr).map(UnitExprAst::Array),
            @"expression",
        });

        loop {
            if self.reduce_repetition(Tk::NewLine) && self.next_at(Tk::Dot) {
                self.advance();
            }
            unit = branch!(self => {
                LeftParen => self.call(unit?)
                    .map(|call| self.arena.alloc(call))
                    .map(UnitExprAst::Call),
                Dot => self.dot_expr(unit?)
                    .map(|path| self.arena.alloc(path))
                    .map(UnitExprAst::DotExpr),
                Tilde => self.enum_ctor(unit?)
                    .map(UnitExprAst::EnumCtor),
                BackSlash => {
                    let slash = self.advance();
                    branch!(self => {
                        LeftBrace => self.struct_ctor(Some(unit?), slash)
                            .map(UnitExprAst::StructCtor),
                        @"backslash expression",
                    })
                },
                @ => break unit,
            });
        }
    }

    fn r#let(&mut self) -> Option<LetAst<'arena, M>> {
        Some(LetAst {
            keyword: self.advance(),
            pat: self.pat(None)?,
            ty: self
                .try_advance(Tk::Colon)
                .map(|colon| self.ty().map(|ty| (colon, ty)))
                .transpose()?,
            equal: self.expect("=", |s| ExpectedAssignEqual {
                got: s.state.current.kind,
                loc: s.loc(),
                something: "let",
            })?,
            value: self.expr()?,
        })
    }

    fn enum_ctor(&mut self, path: UnitExprAst<'arena, M>) -> Option<EnumCtorAst<'arena, M>> {
        Some(EnumCtorAst {
            path: match path {
                UnitExprAst::Path(path) => path,
                path => self.workspace.push(ExpectedEnumCtorName {
                    span: path.span(),
                    source: self.source,
                })?,
            },
            value: self
                .try_advance(Tk::Tilde)
                .map(|tilde| self.expr().map(|value| (tilde, value)))
                .transpose()?,
        })
    }

    fn struct_ctor(
        &mut self,
        ty: Option<UnitExprAst<'arena, M>>,
        slash: SourceInfo<M>,
    ) -> Option<StructCtorAst<'arena, M>> {
        let path = ty
            .map(|ty| match ty {
                UnitExprAst::Path(path) => Some(path),
                ty => self.workspace.push(ExpectedStructName {
                    span: ty.span(),
                    source: self.source,
                })?,
            })
            .transpose()?;

        Some(StructCtorAst {
            path,
            slash,
            body: self.object("struct constructor body", Self::struct_ctor_field)?,
        })
    }

    fn struct_ctor_field(&mut self) -> Option<StructCtorFieldAst<'arena, M>> {
        Some(StructCtorFieldAst {
            name: self.name("struct constructor field")?,
            value: self
                .try_advance(Tk::Colon)
                .map(|colon| self.expr().map(|value| (colon, value)))
                .transpose()?,
        })
    }

    fn dot_expr(&mut self, lhs: UnitExprAst<'arena, M>) -> Option<DotExprAst<'arena, M>> {
        Some(DotExprAst {
            lhs,
            infix: self.advance(),
            rhs: self.path(None)?,
        })
    }

    fn call(&mut self, callable: UnitExprAst<'arena, M>) -> Option<CallAst<'arena, M>> {
        Some(CallAst {
            callable,
            args: self.tuple("function arguments", Self::expr)?,
        })
    }
}

ctl_errors! {
    #[err => "expected enum constructor name"]
    #[info => "enum constructor name must with either '\\<ident>' of type path"]
    error ExpectedEnumCtorName: fatal {
        #[err source, span, "here"]
        span: Span,
        source: VRef<Source>,
    }

    #[err => "expected '=' since {something} statement must be always initialized"]
    #[info => "this may change in the future"]
    pub error ExpectedAssignEqual: fatal {
        #[err loc]
        got: Tk,
        loc: SourceLoc,
        something: &'static str,
    }

    #[err => "expected struct name"]
    #[info => "struct constructor can either start with '\\' or type path"]
    error ExpectedStructName: fatal {
        #[err source, span, "here"]
        span: Span,
        source: VRef<Source>,
    }
}
