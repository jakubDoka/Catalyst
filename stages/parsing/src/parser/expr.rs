use super::*;

list_meta!(BlockMeta LeftCurly NewLine RightCurly);
pub type BlockAst<'a> = ListAst<'a, ExprAst<'a>, BlockMeta>;

list_meta!(CallArgsMeta LeftParen Comma RightParen);
pub type CallArgsAst<'a> = ListAst<'a, ExprAst<'a>, CallArgsMeta>;

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
    Call(&'a CallExprAst<'a>),
    Path(PathExprAst<'a>),
    Return(ReturnExprAst<'a>),
    Int(Span),
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
            Const => ctx.parse().map(Self::Const),
        });

        loop {
            unit = branch!(ctx => {
                LeftParen => ctx.parse_args((unit?, ))
                    .map(|call| ctx.arena.alloc(call))
                    .map(Self::Call),
                _ => break unit,
            });
        }
    }

    fn span(&self) -> Span {
        match *self {
            UnitExprAst::Call(call) => call.span(),
            UnitExprAst::Path(path) => path.span(),
            UnitExprAst::Return(ret) => ret.span(),
            UnitExprAst::Int(span) => span,
            UnitExprAst::Const(run) => run.span(),
        }
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
