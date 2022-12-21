use diags::*;

use super::{items::ITEM_BODY_SYNTAX, *};

#[derive(Clone, Copy, Debug)]
pub struct StructAst<'a> {
    pub vis: Vis,
    pub generics: ListAst<'a, GenericParamAst<'a>>,
    pub name: NameAst,
    pub body: ListAst<'a, StructFieldAst<'a>>,
    pub span: Span,
}

impl<'a> Ast<'a> for StructAst<'a> {
    type Args = (Vis, Span);

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (vis, start): Self::Args) -> Option<Self> {
        ctx.advance();
        let generics = ctx.parse_args(GENERICS_SYNTAX.into())?;
        let name = NameAst::parse(ctx)?;
        let body = ctx.parse_args(ITEM_BODY_SYNTAX.into())?;

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

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        Some(StructFieldAst {
            start: ctx.state.current.span,
            vis: ctx.visibility(),
            used: ctx.try_advance(TokenKind::Use).is_some(),
            mutable: ctx.try_advance(TokenKind::Mut).is_some(),
            name: ctx.parse()?,
            ty: {
                ctx.expect_advance(TokenKind::Colon, |ctx| MissingFieldColon {
                    found: ctx.state.current.kind,
                    loc: ctx.loc(),
                })?;
                TyAst::parse(ctx)?
            },
        })
    }

    fn span(&self) -> Span {
        self.start.joined(self.ty.span())
    }
}

ctl_errors! {
    #[err => "expected ':' but found '{found}' when parsing struct field"]
    #[info => "struct fields have colon after name for consistency reasons"]
    error MissingFieldColon: fatal {
        #[err loc]
        found: TokenKind,
        loc: SourceLoc,
    }
}

#[derive(Clone, Copy, Debug)]
pub struct StructCtorFieldAst<'a> {
    pub name: NameAst,
    pub expr: Option<ExprAst<'a>>,
}

impl<'a> Ast<'a> for StructCtorFieldAst<'a> {
    type Args = ();

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
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
