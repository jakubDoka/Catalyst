pub mod bound;
pub mod expr;
// pub mod r#fn;
pub mod imports;
pub mod items;
pub mod manifest;
pub mod r#struct;
pub mod ty;

use crate::*;
use lexing::*;
use lexing_t::*;
use parsing_t::*;
use scope::*;
use storage::*;

list_meta!(GenericsMeta ? LeftBracket Comma RightBracket);
pub type GenericsAst<'a> = ListAst<'a, GenericParamAst<'a>, GenericsMeta>;

list_meta!(BoundsMeta ? Colon "+" none);
pub type ParamBoundsAst<'a> = ListAst<'a, BoundExprAst<'a>, BoundsMeta>;

list_meta!(TupleConstructorMeta ? LeftParen Comma RightParen);
pub type TupleConstructorAst<'a> = ListAst<'a, ExprAst<'a>, TupleConstructorMeta>;

list_meta!(StructConstructorMeta ? LeftCurly Comma RightCurly);
pub type StructConstructorAst<'a> =
    ListAst<'a, StructConstructorFieldAst<'a>, StructConstructorMeta>;

#[derive(Clone, Copy, Debug)]
pub struct GenericParamAst<'a> {
    pub name: NameAst,
    pub bounds: ParamBoundsAst<'a>,
}

impl<'a> Ast<'a> for GenericParamAst<'a> {
    type Args = ();

    const NAME: &'static str = "generic param";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Result<Self, ()> {
        Ok(Self {
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
    pub segments: &'a [PathSegmentAst<'a>],
    pub span: Span,
}

impl PathAst<'_> {
    pub fn needs_front_slash(&self) -> bool {
        false
    }
}

impl<'a> Ast<'a> for PathAst<'a> {
    type Args = ();

    const NAME: &'static str = "ident chain";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Result<Self, ()> {
        let start = ctx.state.current.span;
        if ctx.at_tok(TokenKind::BackSlash) {
            ctx.advance();
        }

        let mut segments = bumpvec![];
        loop {
            let segment = PathSegmentAst::parse(ctx)?;
            segments.push(segment);

            if !ctx.at_tok(TokenKind::BackSlash) {
                break;
            }

            ctx.advance();
        }
        let segments = ctx.arena.alloc_slice(&segments);
        let span = start.joined(segments.last().unwrap().span());

        Ok(Self { segments, span })
    }

    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Copy, Debug)]
pub enum PathSegmentAst<'a> {
    Name(NameAst),
    Generics(TyGenericsAst<'a>),
    Tuple(TupleConstructorAst<'a>),
    Struct(StructConstructorAst<'a>),
}

impl<'a> Ast<'a> for PathSegmentAst<'a> {
    type Args = ();

    const NAME: &'static str = "ident chain segment";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Result<Self, ()> {
        branch! {ctx => {
            Ident => Ok(Self::Name(ctx.name_unchecked())),
            LeftBracket => ctx.parse().map(Self::Generics),
            LeftParen => ctx.parse().map(Self::Tuple),
            LeftCurly => ctx.parse().map(Self::Struct),
        }}
    }

    fn span(&self) -> Span {
        match self {
            Self::Name(name) => name.span(),
            Self::Generics(generics) => generics.span(),
            Self::Tuple(tuple) => tuple.span(),
            Self::Struct(r#struct) => r#struct.span(),
        }
    }
}
