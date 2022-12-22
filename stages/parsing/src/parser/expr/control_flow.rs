use std::ops::Not;

use super::*;

impl<'a> Ast<'a> for LoopAst<'a> {
    type Args = ();

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        Some(Self {
            r#loop: ctx.advance().span,
            label: ctx
                .try_advance(TokenKind::Label)
                .map(|tok| NameAst::new(ctx, tok.span)),
            body: ctx.parse()?,
        })
    }

    fn span(&self) -> Span {
        self.r#loop.joined(self.body.span())
    }
}

impl<'a> Ast<'a> for BreakAst<'a> {
    type Args = ();

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        Some(Self {
            r#break: ctx.advance().span,
            label: ctx
                .try_advance(TokenKind::Label)
                .map(|tok| NameAst::new(ctx, tok.span)),
            value: ctx
                .at([TokenKind::NewLine, TokenKind::Else])
                .not()
                .then(|| ctx.parse())
                .transpose()?,
        })
    }

    fn span(&self) -> Span {
        self.value
            .map(|v| v.span())
            .or_else(|| self.label.map(|l| l.span()))
            .map_or(self.r#break, |s| self.r#break.joined(s))
    }
}

impl Ast<'_> for ContinueAst {
    type Args = ();

    fn parse_args(ctx: &mut ParsingCtx<'_, '_, '_>, (): Self::Args) -> Option<Self> {
        Some(Self {
            r#continue: ctx.advance().span,
            label: ctx
                .try_advance(TokenKind::Label)
                .map(|tok| NameAst::new(ctx, tok.span)),
        })
    }

    fn span(&self) -> Span {
        self.label.map(|l| l.span()).unwrap_or(self.r#continue)
    }
}

impl<'a> Ast<'a> for IfAst<'a> {
    type Args = ();

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
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

impl<'a> Ast<'a> for ElifAst<'a> {
    type Args = (Span,);

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (elif,): Self::Args) -> Option<Self> {
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

impl<'a> Ast<'a> for IfBlockAst<'a> {
    type Args = ();

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        branch!(ctx => {
            LeftCurly => ctx.parse_args(BLOCK_SYNTAX.into()).map(Self::Block),
            ThickRightArrow => Some(Self::Arrow(ctx.advance().span, {
                ctx.skip(TokenKind::NewLine);
                ctx.parse()?
            })),
            @"if block",
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

impl<'a> Ast<'a> for MatchExprAst<'a> {
    type Args = ();

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        Some(Self {
            r#match: ctx.advance().span,
            expr: ctx.parse()?,
            body: ctx.parse_args(BLOCK_SYNTAX.into())?,
        })
    }

    fn span(&self) -> Span {
        self.r#match.joined(self.body.span())
    }
}

impl<'a> Ast<'a> for MatchArmAst<'a> {
    type Args = ();

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        Some(Self {
            pattern: ctx.parse()?,
            body: ctx.parse()?,
        })
    }

    fn span(&self) -> Span {
        self.pattern.span().joined(self.body.span())
    }
}

impl<'a> Ast<'a> for ReturnExprAst<'a> {
    type Args = ();

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        Some(Self {
            return_span: ctx.advance().span,
            expr: ctx
                .at([TokenKind::NewLine, TokenKind::Else])
                .not()
                .then(|| ctx.parse())
                .transpose()?,
        })
    }

    fn span(&self) -> Span {
        self.expr
            .map_or(self.return_span, |e| self.return_span.joined(e.span()))
    }
}
