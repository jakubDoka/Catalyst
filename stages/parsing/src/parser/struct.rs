use super::*;

list_meta!(StructBodyMeta ? LeftCurly NewLine RightCurly);
pub type StructBodyAst<'a> = ListAst<'a, StructFieldAst<'a>, StructBodyMeta>;

#[derive(Clone, Copy, Debug)]
pub struct StructAst<'a> {
    pub vis: Vis,
    pub generics: GenericsAst<'a>,
    pub name: NameAst,
    pub body: StructBodyAst<'a>,
    pub span: Span,
}

impl<'a> Ast<'a> for StructAst<'a> {
    type Args = (Vis, Span);

    const NAME: &'static str = "struct";

    fn parse_args_internal(
        ctx: &mut ParsingCtx<'_, 'a, '_>,
        (vis, start): Self::Args,
    ) -> Option<Self> {
        ctx.advance();
        let generics = GenericsAst::parse(ctx)?;
        let name = NameAst::parse(ctx)?;
        let body = StructBodyAst::parse(ctx)?;

        Some(StructAst {
            vis,
            generics,
            name,
            body,
            span: start.joined(body.span()),
        })
    }

    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Copy, Debug)]
pub struct StructFieldAst<'a> {
    pub start: Span,
    pub vis: Vis,
    pub used: bool,
    pub mutable: bool,
    pub name: NameAst,
    pub ty: TyAst<'a>,
}

impl<'a> Ast<'a> for StructFieldAst<'a> {
    type Args = ();

    const NAME: &'static str = "struct field";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        Some(StructFieldAst {
            start: ctx.state.current.span,
            vis: ctx.visibility(),
            used: ctx.try_advance(TokenKind::Use).is_some(),
            mutable: ctx.try_advance(TokenKind::Mut).is_some(),
            name: ctx.parse()?,
            ty: {
                ctx.expect_advance(TokenKind::Colon)?;
                TyAst::parse(ctx)?
            },
        })
    }

    fn span(&self) -> Span {
        self.start.joined(self.ty.span())
    }
}

#[derive(Clone, Copy, Debug)]
pub struct StructCtorFieldAst<'a> {
    pub name: NameAst,
    pub expr: Option<ExprAst<'a>>,
}

impl<'a> Ast<'a> for StructCtorFieldAst<'a> {
    type Args = ();

    const NAME: &'static str = "struct constructor field";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        let name = ctx.parse()?;
        let expr = if ctx.try_advance(TokenKind::Colon).is_some() {
            Some(ctx.parse()?)
        } else {
            None
        };

        Some(StructCtorFieldAst { name, expr })
    }

    fn span(&self) -> Span {
        self.expr
            .map_or(self.name.span(), |e| self.name.span().joined(e.span()))
    }
}
