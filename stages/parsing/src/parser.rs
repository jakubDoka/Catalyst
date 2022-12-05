pub mod expr;
pub mod func;
pub mod imports;
pub mod items;
pub mod manifest;
pub mod spec;
pub mod r#struct;
pub mod ty;

use crate::*;
use lexing::*;
use lexing_t::*;
use parsing_t::*;

use storage::*;

list_meta!(GenericsMeta ? LeftBracket Comma RightBracket);
pub type GenericsAst<'a> = ListAst<'a, GenericParamAst<'a>, GenericsMeta>;

list_meta!(BoundsMeta ? Colon "+" none);
pub type ParamSpecsAst<'a> = ListAst<'a, SpecExprAst<'a>, BoundsMeta>;

list_meta!(TupleCtorMeta ? LeftParen Comma RightParen);
pub type TupleCtorAst<'a> = ListAst<'a, ExprAst<'a>, TupleCtorMeta>;

#[derive(Clone, Copy, Debug)]
pub struct GenericParamAst<'a> {
    pub name: NameAst,
    pub bounds: ParamSpecsAst<'a>,
}

impl<'a> Ast<'a> for GenericParamAst<'a> {
    type Args = ();

    const NAME: &'static str = "generic param";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        Some(Self {
            name: ctx.parse()?,
            bounds: ctx.parse()?,
        })
    }

    fn span(&self) -> Span {
        self.name.span().joined(self.bounds.span())
    }
}

#[derive(Clone, Copy, Debug)]
pub struct PathAst<'a> {
    pub slash: Option<Span>,
    pub start: PathItemAst<'a>,
    pub segments: &'a [PathItemAst<'a>],
}

impl<'a> Ast<'a> for PathAst<'a> {
    type Args = ();

    const NAME: &'static str = "ident chain";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        let slash = ctx.try_advance(TokenKind::BackSlash).map(|t| t.span);
        let start = if ctx.at_next_tok(TokenKind::Ident) {
            PathItemAst::Ident(ctx.name_unchecked())
        } else {
            PathItemAst::Params(ctx.parse()?)
        };
        let mut segments = bumpvec![];
        while ctx.at_tok(TokenKind::BackSlash) {
            if ctx.at_next_tok(TokenKind::Ident) {
                ctx.advance();
                segments.push(PathItemAst::Ident(ctx.name_unchecked()));
            } else if ctx.at_next_tok(TokenKind::LeftBracket) {
                ctx.advance();
                segments.push(PathItemAst::Params(ctx.parse()?));
            } else {
                break;
            }
        }
        let segments = ctx.arena.alloc_slice(&segments);
        Some(Self {
            slash,
            start,
            segments,
        })
    }

    fn span(&self) -> Span {
        self.segments.last().map_or(self.start.span(), |last| {
            self.start.span().joined(last.span())
        })
    }
}

#[derive(Clone, Copy, Debug)]
pub enum PathItemAst<'a> {
    Ident(NameAst),
    Params(TyGenericsAst<'a>),
}

impl PathItemAst<'_> {
    pub fn span(&self) -> Span {
        match self {
            Self::Ident(name) => name.span(),
            Self::Params(params) => params.span(),
        }
    }
}
