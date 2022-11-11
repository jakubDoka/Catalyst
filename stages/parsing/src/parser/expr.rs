use super::*;

list_meta!(BlockMeta LeftCurly NewLine RightCurly);
pub type BlockAst<'a> = ListAst<'a, ExprAst<'a>, BlockMeta>;
pub type MatchBodyAst<'a> = ListAst<'a, MatchArmAst<'a>, BlockMeta>;
list_meta!(StructCtorPatBodyMeta LeftCurly [Comma NewLine] RightCurly);
pub type StructCtorPatBodyAst<'a> = ListAst<'a, StructCtorPatFieldAst<'a>, StructCtorPatBodyMeta>;
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
    Const(ConstAst<'a>),
    Match(MatchExprAst<'a>),
    If(IfAst<'a>),
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
            Const => ctx.parse().map(Self::Const),
            Match => ctx.parse().map(Self::Match),
            If => ctx.parse().map(Self::If),
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
            Const(run) => run.span(),
            TypedPath(path) => path.span(),
            Match(r#match) => r#match.span(),
            EnumCtor(ctor) => ctor.span(),
            If(r#if) => r#if.span(),
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
pub struct IfAst<'a> {
    pub r#if: Span,
    pub cond: ExprAst<'a>,
    pub body: IfBlockAst<'a>,
    pub elifs: &'a [ElifAst<'a>],
    pub r#else: Option<(Span, IfBlockAst<'a>)>,
}

impl<'a> Ast<'a> for IfAst<'a> {
    type Args = ();

    const NAME: &'static str = "if";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        Some(Self {
            r#if: ctx.advance().span,
            cond: ctx.parse()?,
            body: ctx.parse()?,
            elifs: {
                let mut else_ifs = bumpvec![];
                while let Some(elif) = ctx.try_advance_ignore_lines(TokenKind::Elif) {
                    else_ifs.push(ctx.parse_args((elif.span,))?);
                }
                ctx.arena.alloc_iter(else_ifs)
            },
            r#else: ctx
                .try_advance_ignore_lines(TokenKind::Else)
                .map(|r#else| ctx.parse().map(|body| (r#else.span, body)))
                .transpose()?,
        })
    }

    fn span(&self) -> Span {
        let mut span = self.cond.span().joined(self.body.span());
        if let Some((_, body)) = self.r#else {
            span = span.joined(body.span());
        }
        span
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ElifAst<'a> {
    pub elif: Span,
    pub cond: ExprAst<'a>,
    pub body: IfBlockAst<'a>,
}

impl<'a> Ast<'a> for ElifAst<'a> {
    type Args = (Span,);

    const NAME: &'static str = "else if";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a, '_>, (elif,): Self::Args) -> Option<Self> {
        Some(Self {
            elif,
            cond: ctx.parse()?,
            body: ctx.parse()?,
        })
    }

    fn span(&self) -> Span {
        self.elif.joined(self.body.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum IfBlockAst<'a> {
    Block(BlockAst<'a>),
    Arrow(Span, ExprAst<'a>),
}

impl<'a> Ast<'a> for IfBlockAst<'a> {
    type Args = ();

    const NAME: &'static str = "if block";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        branch!(ctx => {
            LeftCurly => ctx.parse().map(Self::Block),
            ThickRightArrow => Some(Self::Arrow(ctx.advance().span, {
                ctx.skip(TokenKind::NewLine);
                ctx.parse()?
            })),
        })
    }

    fn span(&self) -> Span {
        use IfBlockAst::*;
        match *self {
            Block(block) => block.span(),
            Arrow(.., expr) => expr.span(),
        }
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
pub struct MatchExprAst<'a> {
    pub r#match: Span,
    pub expr: ExprAst<'a>,
    pub body: MatchBodyAst<'a>,
}

impl<'a> Ast<'a> for MatchExprAst<'a> {
    type Args = ();

    const NAME: &'static str = "match expr";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        Some(Self {
            r#match: ctx.advance().span,
            expr: ctx.parse()?,
            body: ctx.parse()?,
        })
    }

    fn span(&self) -> Span {
        self.r#match.joined(self.body.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MatchArmAst<'a> {
    pub pattern: PatAst<'a>,
    pub body: IfBlockAst<'a>,
}

impl<'a> Ast<'a> for MatchArmAst<'a> {
    type Args = ();

    const NAME: &'static str = "match arm";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        Some(Self {
            pattern: ctx.parse()?,
            body: ctx.parse()?,
        })
    }

    fn span(&self) -> Span {
        self.pattern.span().joined(self.body.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PatAst<'a> {
    Binding(Option<Span>, NameAst),
    Wildcard(Span),
    StructCtor(StructCtorPatAst<'a>),
    EnumCtor(&'a EnumCtorPatAst<'a>),
    Int(Span),
}

impl<'a> Ast<'a> for PatAst<'a> {
    type Args = Option<Span>;

    const NAME: &'static str = "pattern";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a, '_>, mutable: Self::Args) -> Option<Self> {
        branch!(ctx => {
            Mut => {
                if let Some(_mutable) = mutable {
                    todo!();
                }

                let mutable = ctx.advance().span;
                ctx.parse_args(Some(mutable))
            },
            Ident => match ctx.current_token_str() {
                "_" => Some(Self::Wildcard(ctx.advance().span)),
                _ => ctx.parse().map(|b| Self::Binding(mutable, b)),
            },
            BackSlash => {
                if ctx.at_next_tok(TokenKind::Ident) {
                    ctx.parse_args_alloc(mutable).map(Self::EnumCtor)
                } else {
                    ctx.parse_args(mutable).map(Self::StructCtor)
                }
            },
            Int => Some(Self::Int(ctx.advance().span)),
        })
    }

    fn span(&self) -> Span {
        use PatAst::*;
        match *self {
            Binding(mutable, name) => {
                mutable.map_or(name.span(), |mutable| mutable.joined(name.span()))
            }
            StructCtor(ctor) => ctor.span(),
            Wildcard(span) | Int(span) => span,
            EnumCtor(ctor) => ctor.span(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct EnumCtorPatAst<'a> {
    pub slash: Span,
    pub name: NameAst,
    pub value: Option<(Span, PatAst<'a>)>,
}

impl<'a> Ast<'a> for EnumCtorPatAst<'a> {
    type Args = Option<Span>;

    const NAME: &'static str = "enum pattern";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a, '_>, mutable: Self::Args) -> Option<Self> {
        Some(Self {
            slash: ctx.advance().span,
            name: ctx.parse()?,

            value: ctx
                .try_advance(TokenKind::Tilde)
                .map(|tilde| ctx.parse_args(mutable).map(|value| (tilde.span, value)))
                .transpose()?,
        })
    }

    fn span(&self) -> Span {
        self.slash.joined(
            self.value
                .map_or(self.name.span(), |(.., body)| body.span()),
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StructCtorPatAst<'a> {
    pub slash: Span,
    pub fields: StructCtorPatBodyAst<'a>,
}

impl<'a> Ast<'a> for StructCtorPatAst<'a> {
    type Args = Option<Span>;

    const NAME: &'static str = "struct ctor pattern";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a, '_>, mutable: Self::Args) -> Option<Self> {
        Some(Self {
            slash: ctx.advance().span,
            fields: ctx.parse_args(mutable)?,
        })
    }

    fn span(&self) -> Span {
        self.slash.joined(self.fields.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum StructCtorPatFieldAst<'a> {
    Simple {
        mutable: Option<Span>,
        name: NameAst,
    },
    Named {
        name: NameAst,
        colon: Span,
        pat: PatAst<'a>,
    },
    DoubleDot(Span),
}

impl<'a> Ast<'a> for StructCtorPatFieldAst<'a> {
    type Args = Option<Span>;

    const NAME: &'static str = "struct ctor pattern field";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a, '_>, mutable: Self::Args) -> Option<Self> {
        Some(branch! {ctx => {
            Mut => {
                if let Some(_mutable) = mutable {
                    todo!();
                }

                let mutable = ctx.advance().span;
                ctx.parse_args(Some(mutable))?
            },
            Ident => {
                let name = ctx.parse()?;
                branch! {ctx => {
                    Colon => Self::Named { name, colon: ctx.advance().span, pat: ctx.parse_args(mutable)? },
                    _ => Self::Simple { name, mutable },
                }}
            },
            DoubleDot => Self::DoubleDot(ctx.advance().span),
        }})
    }

    fn span(&self) -> Span {
        use StructCtorPatFieldAst::*;
        match *self {
            Simple { name, mutable } => {
                mutable.map_or(name.span(), |mutable| mutable.joined(name.span()))
            }
            Named { name, pat, .. } => name.span().joined(pat.span()),
            DoubleDot(span) => span,
        }
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
pub struct ConstAst<'a> {
    pub r#const: Span,
    pub value: ExprAst<'a>,
}

impl<'a> Ast<'a> for ConstAst<'a> {
    type Args = ();

    const NAME: &'static str = "comp time run";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        Some(Self {
            r#const: ctx.advance().span,
            value: ctx.parse()?,
        })
    }

    fn span(&self) -> Span {
        self.r#const.joined(self.value.span())
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

#[derive(Debug, Clone, Copy)]
pub struct ReturnExprAst<'a> {
    pub return_span: Span,
    pub expr: Option<ExprAst<'a>>,
}

impl<'a> Ast<'a> for ReturnExprAst<'a> {
    type Args = ();

    const NAME: &'static str = "return";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        Some(Self {
            return_span: ctx.advance().span,
            expr: if ctx.at_tok(TokenKind::NewLine) {
                None
            } else {
                Some(ctx.parse()?)
            },
        })
    }

    fn span(&self) -> Span {
        self.expr
            .map_or(self.return_span, |e| self.return_span.joined(e.span()))
    }
}
