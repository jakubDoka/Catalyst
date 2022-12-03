pub mod control_flow;
pub mod pat;

use super::*;

list_meta!(BlockMeta LeftCurly NewLine RightCurly);
pub type BlockAst<'a> = ListAst<'a, ExprAst<'a>, BlockMeta>;
list_meta!(CallArgsMeta LeftParen Comma RightParen);
pub type CallArgsAst<'a> = ListAst<'a, ExprAst<'a>, CallArgsMeta>;
pub type StructCtorBodyAst<'a> = ListAst<'a, StructCtorFieldAst<'a>, BlockMeta>;

#[derive(Debug, Clone, Copy)]
pub enum ExprAst<'a> {
    Unit(&'a UnitExprAst<'a>),
    Binary(&'a BinaryExprAst<'a>),
}

impl<'a> Ast<'a> for ExprAst<'a> {
    type Args = ();

    const NAME: &'static str = "expr";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        let unit = ctx.parse_alloc().map(ExprAst::Unit)?;
        BinaryExprAst::try_parse_binary(ctx, unit, u8::MAX)
    }

    fn span(&self) -> Span {
        match *self {
            ExprAst::Unit(unit) => unit.span(),
            ExprAst::Binary(binary) => binary.span(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BinaryExprAst<'a> {
    pub lhs: ExprAst<'a>,
    pub op: NameAst,
    pub rhs: ExprAst<'a>,
}

impl<'a> BinaryExprAst<'a> {
    fn try_parse_binary(
        ctx: &mut ParsingCtx<'_, 'a, '_>,
        mut lhs: ExprAst<'a>,
        prev_precedence: u8,
    ) -> Option<ExprAst<'a>> {
        Some(loop {
            let TokenKind::Operator(precedence) = ctx.state.current.kind else {
                break lhs;
            };

            if prev_precedence > precedence {
                let op = ctx.name_unchecked();
                ctx.skip(TokenKind::NewLine);
                let rhs = ctx.parse_alloc().map(ExprAst::Unit)?;
                let rhs = Self::try_parse_binary(ctx, rhs, precedence)?;
                lhs = ExprAst::Binary(ctx.arena.alloc(Self { lhs, op, rhs }));
            } else {
                break lhs;
            }
        })
    }

    pub fn span(&self) -> Span {
        self.lhs.span().joined(self.rhs.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnitExprAst<'a> {
    StructCtor(StructCtorAst<'a>),
    EnumCtor(EnumCtorAst<'a>),
    DotExpr(&'a DotExprAst<'a>),
    Call(&'a CallExprAst<'a>),
    Path(PathExprAst<'a>),
    PathInstance(PathInstanceAst<'a>),
    TypedPath(TypedPathAst<'a>),
    Return(ReturnExprAst<'a>),
    Int(Span),
    Char(Span),
    Bool(Span),
    Match(MatchExprAst<'a>),
    If(IfAst<'a>),
    Loop(LoopAst<'a>),
    Let(LetAst<'a>),
    Deref(Span, &'a UnitExprAst<'a>),
    Ref(Span, MutabilityAst<'a>, &'a UnitExprAst<'a>),
}

impl<'a> Ast<'a> for UnitExprAst<'a> {
    type Args = ();

    const NAME: &'static str = "unit expr";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        let mut unit = branch!(ctx => {
            Ident => ctx.parse().map(Self::Path),
            BackSlash => {
                if ctx.at_next_tok(TokenKind::LeftCurly) {
                    let slash = ctx.advance().span;
                    ctx.parse_args((None, slash))
                        .map(Self::StructCtor)
                } else {
                    ctx.parse().map(Self::Path)
                }
            },
            Return => ctx.parse().map(Self::Return),
            Int => Some(Self::Int(ctx.advance().span)),
            Char => Some(Self::Char(ctx.advance().span)),
            Bool => Some(Self::Bool(ctx.advance().span)),
            Match => ctx.parse().map(Self::Match),
            If => ctx.parse().map(Self::If),
            Loop => ctx.parse().map(Self::Loop),
            Let => ctx.parse().map(Self::Let),
            Operator(_ = 0) => branch! {str ctx => {
                "*" => Some(Self::Deref(ctx.advance().span, ctx.parse_alloc()?)),
                "^" => Some(Self::Ref(ctx.advance().span, ctx.parse()?, ctx.parse_alloc()?)),
            }},
        });

        loop {
            if ctx.reduce_repetition(TokenKind::NewLine) && ctx.at_next_tok(TokenKind::Dot) {
                ctx.advance();
            }
            unit = branch!(ctx => {
                LeftParen => ctx.parse_args((unit?, ))
                    .map(|call| ctx.arena.alloc(call))
                    .map(Self::Call),
                Dot => ctx.parse_args((unit?, ))
                    .map(|path| ctx.arena.alloc(path))
                    .map(Self::DotExpr),
                Tilde => ctx.parse_args((unit?, ))
                    .map(Self::EnumCtor),
                BackSlash => {
                    let slash = ctx.advance().span;
                    branch!(ctx => {
                        LeftCurly => ctx.parse_args((Some(unit?), slash))
                            .map(Self::StructCtor),
                        LeftBracket => ctx.parse_args((unit?, Some(slash)))
                            .map(Self::PathInstance),
                        Ident => ctx.parse_args((unit?, slash))
                            .map(Self::TypedPath),
                    })
                },
                _ => break unit,
            });
        }
    }

    fn span(&self) -> Span {
        use UnitExprAst::*;
        match *self {
            StructCtor(block) => block.span(),
            DotExpr(dot) => dot.span(),
            Call(call) => call.span(),
            Path(path) => path.span(),
            PathInstance(instance) => instance.span(),
            Return(ret) => ret.span(),
            Int(span) | Char(span) | Bool(span) => span,
            TypedPath(path) => path.span(),
            Match(r#match) => r#match.span(),
            EnumCtor(ctor) => ctor.span(),
            If(r#if) => r#if.span(),
            Loop(r#loop) => r#loop.span(),
            Let(r#let) => r#let.span(),
            Deref(span, expr) | Ref(span, .., expr) => span.joined(expr.span()),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LetAst<'a> {
    pub r#let: Span,
    pub pat: PatAst<'a>,
    pub ty: Option<(Span, TyAst<'a>)>,
    pub equal: Span,
    pub value: ExprAst<'a>,
}

impl<'a> Ast<'a> for LetAst<'a> {
    type Args = ();

    const NAME: &'static str = "let";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        Some(Self {
            r#let: ctx.advance().span,
            pat: ctx.parse()?,
            ty: ctx
                .try_advance(TokenKind::Colon)
                .map(|colon| ctx.parse().map(|ty| (colon.span, ty)))
                .transpose()?,
            equal: ctx.expect_advance("=")?.span,
            value: ctx.parse()?,
        })
    }

    fn span(&self) -> Span {
        self.r#let.joined(self.value.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct EnumCtorAst<'a> {
    pub path: PathInstanceAst<'a>,
    pub value: Option<(Span, ExprAst<'a>)>,
}

impl<'a> Ast<'a> for EnumCtorAst<'a> {
    type Args = (UnitExprAst<'a>,);

    const NAME: &'static str = "enum ctor";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a, '_>, (path,): Self::Args) -> Option<Self> {
        Some(Self {
            path: match path {
                UnitExprAst::PathInstance(path) => path,
                UnitExprAst::Path(path) => PathInstanceAst { path, params: None },
                _ => todo!(),
            },
            value: ctx
                .try_advance(TokenKind::Tilde)
                .map(|tilde| ctx.parse().map(|value| (tilde.span, value)))
                .transpose()?,
        })
    }

    fn span(&self) -> Span {
        self.value.map_or(self.path.span(), |(.., value)| {
            self.path.span().joined(value.span())
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TypedPathAst<'a> {
    pub ty: TyAst<'a>,
    pub slash: Span,
    pub path: PathInstanceAst<'a>,
}

impl<'a> Ast<'a> for TypedPathAst<'a> {
    type Args = (UnitExprAst<'a>, Span);

    const NAME: &'static str = "typed path";

    fn parse_args_internal(
        ctx: &mut ParsingCtx<'_, 'a, '_>,
        (unit, slash): Self::Args,
    ) -> Option<Self> {
        let ty = match unit {
            UnitExprAst::PathInstance(PathInstanceAst {
                path, params: None, ..
            })
            | UnitExprAst::Path(path) => TyAst::Path(path),
            UnitExprAst::PathInstance(PathInstanceAst {
                path,
                params: Some((.., params)),
                ..
            }) => TyAst::Instance(TyInstanceAst { path, params }),
            _ => ctx.invalid_typed_path(unit.span())?,
        };

        let path = ctx.parse()?;

        Some(Self {
            ty,
            slash,
            path: ctx.parse_args((UnitExprAst::Path(path), None))?,
        })
    }

    fn span(&self) -> Span {
        self.ty.span().joined(self.slash)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct PathInstanceAst<'a> {
    pub path: PathExprAst<'a>,
    pub params: Option<(Span, TyGenericsAst<'a>)>,
}

impl<'a> PathInstanceAst<'a> {
    pub fn params(&self) -> impl Iterator<Item = TyAst<'a>> + '_ {
        self.params
            .as_ref()
            .map(|(_, params)| params.iter().copied())
            .into_iter()
            .flatten()
    }
}

impl<'a> Ast<'a> for PathInstanceAst<'a> {
    type Args = (UnitExprAst<'a>, Option<Span>);

    const NAME: &'static str = "path instance";

    fn parse_args_internal(
        ctx: &mut ParsingCtx<'_, 'a, '_>,
        (path, slash): Self::Args,
    ) -> Option<Self> {
        let UnitExprAst::Path(path) = path else {
            ctx.invalid_instance_path(path.span())?;
        };

        let params = if let Some(slash) = slash {
            Some((slash, ctx.parse()?))
        } else if ctx.at_tok(TokenKind::BackSlash) && ctx.at_next_tok(TokenKind::LeftBracket) {
            let slash = ctx.advance().span;
            Some((slash, ctx.parse()?))
        } else {
            None
        };

        Some(Self { path, params })
    }

    fn span(&self) -> Span {
        self.params.map_or_else(
            || self.path.span(),
            |(span, _)| self.path.span().joined(span),
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StructCtorAst<'a> {
    pub path: Option<PathInstanceAst<'a>>,
    pub slash: Span,
    pub body: StructCtorBodyAst<'a>,
}

impl<'a> Ast<'a> for StructCtorAst<'a> {
    type Args = (Option<UnitExprAst<'a>>, Span);

    const NAME: &'static str = "struct constructor";

    fn parse_args_internal(
        ctx: &mut ParsingCtx<'_, 'a, '_>,
        (ty, slash): Self::Args,
    ) -> Option<Self> {
        let path = match ty {
            Some(UnitExprAst::PathInstance(path)) => Some(path),
            Some(UnitExprAst::Path(path)) => Some(PathInstanceAst { path, params: None }),
            None => None,
            Some(ty) => ctx.invalid_struct_constructor_type(ty.span())?,
        };

        Some(Self {
            path,
            slash,
            body: ctx.parse()?,
        })
    }

    fn span(&self) -> Span {
        self.path
            .map(|ty| ty.span())
            .unwrap_or(self.slash)
            .joined(self.body.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DotExprAst<'a> {
    pub lhs: UnitExprAst<'a>,
    pub dot: Span,
    pub rhs: PathInstanceAst<'a>,
}

impl<'a> Ast<'a> for DotExprAst<'a> {
    type Args = (UnitExprAst<'a>,);

    const NAME: &'static str = "dot expr";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a, '_>, (lhs,): Self::Args) -> Option<Self> {
        Some(Self {
            lhs,
            dot: ctx.advance().span,
            rhs: {
                let path = ctx.parse()?;
                ctx.parse_args((UnitExprAst::Path(path), None))?
            },
        })
    }

    fn span(&self) -> Span {
        self.lhs.span().joined(self.rhs.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CallExprAst<'a> {
    pub callable: UnitExprAst<'a>,
    pub args: CallArgsAst<'a>,
}

impl<'a> Ast<'a> for CallExprAst<'a> {
    type Args = (UnitExprAst<'a>,);

    const NAME: &'static str = "call";

    fn parse_args_internal(
        ctx: &mut ParsingCtx<'_, 'a, '_>,
        (callable,): Self::Args,
    ) -> Option<Self> {
        Some(Self {
            callable,
            args: ctx.parse_args(())?,
        })
    }

    fn span(&self) -> Span {
        self.callable.span().joined(self.args.span())
    }
}
