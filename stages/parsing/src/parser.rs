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

list_meta!(TupleConstructorMeta ? LeftParen Comma RightParen);
pub type TupleConstructorAst<'a> = ListAst<'a, ExprAst<'a>, TupleConstructorMeta>;

#[derive(Clone, Copy, Debug)]
pub struct GenericParamAst<'a> {
    pub name: NameAst,
    pub bounds: ParamSpecsAst<'a>,
}

impl<'a> Ast<'a> for GenericParamAst<'a> {
    type Args = ();

    const NAME: &'static str = "generic param";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
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
pub struct PathExprAst<'a> {
    pub start: NameAst,
    pub segments: &'a [NameAst],
}

impl<'a> Ast<'a> for PathExprAst<'a> {
    type Args = ();

    const NAME: &'static str = "ident chain";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
        let start = ctx.name_unchecked();
        let mut segments = bumpvec![];
        while ctx.at_tok(TokenKind::BackSlash) && ctx.at_next_tok(TokenKind::Ident) {
            ctx.advance();
            segments.push(ctx.name_unchecked());
        }
        let segments = ctx.arena.alloc_slice(&segments);
        Some(Self { start, segments })
    }

    fn span(&self) -> Span {
        self.segments.last().map_or(self.start.span(), |last| {
            self.start.span().joined(last.span())
        })
    }
}
