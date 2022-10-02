use super::*;

list_meta!(BlockMeta LeftCurly NewLine RightCurly);
pub type BlockAst<'a> = ListAst<'a, ExprAst<'a>, BlockMeta>;

list_meta!(CallArgsMeta LeftParen Comma RightParen);
pub type CallArgsAst<'a> = ListAst<'a, ExprAst<'a>, CallArgsMeta>;

pub type StructConstructorBodyAst<'a> = ListAst<'a, StructConstructorFieldAst<'a>, BlockMeta>;

#[derive(Debug, Clone, Copy)]
pub enum ExprAst<'a> {
    Unit(&'a UnitExprAst<'a>),
    Binary(&'a BinaryExprAst<'a>),
}

impl<'a> Ast<'a> for ExprAst<'a> {
    type Args = ();

    const NAME: &'static str = "expr";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
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
    pub op: PathExprAst<'a>,
    pub rhs: ExprAst<'a>,
}

impl<'a> BinaryExprAst<'a> {
    fn try_parse_binary(
        ctx: &mut ParsingCtx<'_, 'a>,
        mut lhs: ExprAst<'a>,
        prev_precedence: u8,
    ) -> Option<ExprAst<'a>> {
        Some(loop {
            let TokenKind::Operator(precedence) = ctx.state.current.kind else {
                break lhs;
            };

            if prev_precedence > precedence {
                let op = ctx.parse::<PathExprAst>()?;
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
    StructConstructor(StructConstructorAst<'a>),
    DotExpr(&'a DotExprAst<'a>),
    Call(&'a CallExprAst<'a>),
    Path(PathExprAst<'a>),
    Return(ReturnExprAst<'a>),
    Int(Span),
    Char(Span),
    Const(ConstAst<'a>),
}

impl<'a> Ast<'a> for UnitExprAst<'a> {
    type Args = ();

    const NAME: &'static str = "unit expr";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
        let mut unit = branch!(ctx => {
            Ident => ctx.parse().map(Self::Path),
            BackSlash => ctx.parse().map(Self::Path),
            Return => ctx.parse().map(Self::Return),
            Int => Some(Self::Int(ctx.advance().span)),
            Char => Some(Self::Char(ctx.advance().span)),
            Const => ctx.parse().map(Self::Const),
        });

        loop {
            unit = branch!(ctx => {
                LeftParen => ctx.parse_args((unit?, ))
                    .map(|call| ctx.arena.alloc(call))
                    .map(Self::Call),
                Dot => ctx.parse_args((unit?, ))
                    .map(|path| ctx.arena.alloc(path))
                    .map(Self::DotExpr),
                BackSlash => {
                    let slash = ctx.advance().span;
                    branch!(ctx => {
                        LeftCurly => ctx.parse_args((Some(unit?), slash))
                            .map(Self::StructConstructor),
                    })
                },
                _ => break unit,
            });
        }
    }

    fn span(&self) -> Span {
        use UnitExprAst::*;
        match *self {
            StructConstructor(block) => block.span(),
            DotExpr(dot) => dot.span(),
            Call(call) => call.span(),
            Path(path) => path.span(),
            Return(ret) => ret.span(),
            Int(span) | Char(span) => span,
            Const(run) => run.span(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StructConstructorAst<'a> {
    pub path: Option<PathExprAst<'a>>,
    pub generics: Option<GenericsAst<'a>>,
    pub slash: Span,
    pub body: StructConstructorBodyAst<'a>,
}

impl<'a> Ast<'a> for StructConstructorAst<'a> {
    type Args = (Option<UnitExprAst<'a>>, Span);

    const NAME: &'static str = "struct constructor";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (ty, slash): Self::Args) -> Option<Self> {
        let (path, generics) = match ty {
            Some(UnitExprAst::Path(path)) => (Some(path), None),
            None => (None, None),
            Some(ty) => ctx.invalid_struct_constructor_type(ty.span())?,
        };

        Some(Self {
            path,
            generics,
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
    pub rhs: PathExprAst<'a>,
}

impl<'a> Ast<'a> for DotExprAst<'a> {
    type Args = (UnitExprAst<'a>,);

    const NAME: &'static str = "dot expr";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (lhs,): Self::Args) -> Option<Self> {
        Some(Self {
            lhs,
            dot: ctx.advance().span,
            rhs: ctx.parse()?,
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

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
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

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (callable,): Self::Args) -> Option<Self> {
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

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
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
