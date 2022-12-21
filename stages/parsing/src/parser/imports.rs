use std::default::default;

use diags::*;

use super::{expr::BLOCK_SYNTAX, *};

#[derive(Clone, Copy, Debug)]
pub struct UseAstSkip;

impl<'a> Ast<'a> for UseAstSkip {
    type Args = ();

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        ctx.skip(TokenKind::NewLine);
        if !ctx.at(TokenKind::Use) {
            return Some(UseAstSkip);
        }

        while !matches!(ctx.advance().kind, TokenKind::RightCurly | TokenKind::Eof) {}

        Some(UseAstSkip)
    }

    fn span(&self) -> Span {
        Span::default()
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct UseAst<'a> {
    pub use_span: Span,
    pub items: ListAst<'a, ImportAst>,
}

impl<'a> Ast<'a> for UseAst<'a> {
    type Args = ();

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        ctx.skip(TokenKind::NewLine);

        if !ctx.at(TokenKind::Use) {
            return Some(default());
        }

        Some(UseAst {
            use_span: ctx.advance().span,
            items: ctx.parse_args(BLOCK_SYNTAX.into())?,
        })
    }

    fn span(&self) -> Span {
        self.use_span.joined(self.items.span())
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ImportAst {
    pub vis: Vis,
    pub name: NameAst,
    pub path: Span,
    pub span: Span,
}

impl<'a> Ast<'a> for ImportAst {
    type Args = ();

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        let start = ctx.state.current.span;
        let vis = ctx.visibility();
        let name = ctx.parse_args((true, "import name"));
        let path = ctx
            .expect_advance(TokenKind::String, |ctx| ExpectedModStringPath {
                found: ctx.state.current.kind,
                loc: ctx.loc(),
            })?
            .span;
        let span = start.joined(path);
        let path = path.shrink(1);
        let name = name.unwrap_or_else(|| NameAst::from_path(ctx, path));
        Some(ImportAst {
            vis,
            name,
            path,
            span,
        })
    }

    fn span(&self) -> Span {
        self.span
    }
}

ctl_errors! {
    #[err => "module path must be a string literal, but got {found}"]
    error ExpectedModStringPath: fatal {
        #[err loc]
        found: TokenKind,
        loc: SourceLoc,
    }
}
