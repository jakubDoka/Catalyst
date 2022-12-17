use diags::{ctl_errors, SourceLoc};

use super::*;

list_meta!(FuncArgMeta ? LeftParen Comma RightParen);
pub type FuncArgsAst<'a> = ListAst<'a, FuncArgAst<'a>, FuncArgMeta>;

#[derive(Clone, Copy, Debug)]
pub struct FuncDefAst<'a> {
    pub vis: Vis,
    pub signature: FuncSigAst<'a>,
    pub body: FuncBodyAst<'a>,
    pub span: Span,
}

impl<'a> Ast<'a> for FuncDefAst<'a> {
    type Args = (Vis, Span);

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (vis, start): Self::Args) -> Option<Self> {
        let signature = ctx.parse()?;
        let body = ctx.parse::<FuncBodyAst>()?;
        let span = start.joined(body.span());

        Some(Self {
            vis,
            signature,
            body,
            span,
        })
    }

    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Copy, Debug)]
pub struct FuncSigAst<'a> {
    pub fn_span: Span,
    pub cc: Option<NameAst>,
    pub generics: GenericsAst<'a>,
    pub name: NameAst,
    pub args: FuncArgsAst<'a>,
    pub ret: Option<TyAst<'a>>,
}

impl<'a> Ast<'a> for FuncSigAst<'a> {
    type Args = ();

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        Some(Self {
            fn_span: ctx.advance().span,
            cc: ctx
                .try_advance(TokenKind::String)
                .map(|tok| NameAst::new(ctx, tok.span.shrink(1))),
            generics: ctx.parse()?,
            name: ctx.parse()?,
            args: ctx.parse()?,
            ret: ctx
                .try_advance(TokenKind::RightArrow)
                .and_then(|_| ctx.parse()),
        })
    }

    fn span(&self) -> Span {
        self.generics
            .span()
            .joined(self.ret.map_or(self.args.span(), |r| r.span()))
    }
}

#[derive(Clone, Copy, Debug)]
pub struct FuncArgAst<'a> {
    pub name: NameAst,
    pub ty: TyAst<'a>,
}

impl<'a> Ast<'a> for FuncArgAst<'a> {
    type Args = ();

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        Some(Self {
            name: ctx.parse()?,
            ty: {
                ctx.expect_advance(TokenKind::Colon, |ctx| MissingFunctionArgColon {
                    got: ctx.state.current.kind,
                    loc: ctx.loc(),
                })?;
                ctx.parse()?
            },
        })
    }

    fn span(&self) -> Span {
        self.name.span().joined(self.ty.span())
    }
}

ctl_errors! {
    #[err => "expected ':' after function argument name but got {got}"]
    #[info => "colon is required for consistency and readability"]
    fatal struct MissingFunctionArgColon {
        #[err loc]
        got: TokenKind,
        loc: SourceLoc,
    }
}

#[derive(Clone, Copy, Debug)]
pub enum FuncBodyAst<'a> {
    Arrow(Span, ExprAst<'a>),
    Block(BlockAst<'a>),
    Extern(Span),
}

impl<'a> Ast<'a> for FuncBodyAst<'a> {
    type Args = ();

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        branch! {ctx => {
            ThickRightArrow => {
                let arrow = ctx.advance().span;
                ctx.skip(TokenKind::NewLine);
                ctx.parse().map(|e| Self::Arrow(arrow, e))
            },
            LeftCurly => ctx.parse().map(Self::Block),
            Extern => Some(Self::Extern(ctx.advance().span)),
            @"function body",
        }}
    }

    fn span(&self) -> Span {
        match self {
            Self::Arrow(span, expr) => span.joined(expr.span()),
            Self::Block(block) => block.span(),
            Self::Extern(span) => *span,
        }
    }
}
