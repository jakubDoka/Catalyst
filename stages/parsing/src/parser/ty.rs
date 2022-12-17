use super::*;

list_meta!(TyGenericsMeta ? LeftBracket Comma RightBracket);
pub type TyGenericsAst<'a> = ListAst<'a, TyAst<'a>, TyGenericsMeta>;

list_meta!(TyTupleMeta LeftParen Comma RightParen);
pub type TyTupleAst<'a> = ListAst<'a, TyAst<'a>, TyTupleMeta>;

#[derive(Clone, Copy, Debug)]
pub enum TyAst<'a> {
    Path(PathAst<'a>),
    Pointer(&'a TyPointerAst<'a>),
    Tuple(TyTupleAst<'a>),
    Wildcard(Span),
}

impl<'a> Ast<'a> for TyAst<'a> {
    type Args = ();

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        branch! {ctx => {
            Ident => {
                if ctx.current_token_str() == "_" {
                    return Some(Self::Wildcard(ctx.advance().span));
                }

                ctx.parse().map(TyAst::Path)
            },
            LeftParen => ctx.parse().map(TyAst::Tuple),
            Operator(_ = 0) => branch!(str ctx => {
                "^" => ctx.parse()
                    .map(|p| ctx.arena.alloc(p))
                    .map(TyAst::Pointer),
                @"prefixed type",
            }),
            @"type",
        }}
    }

    fn span(&self) -> Span {
        match *self {
            TyAst::Path(ident) => ident.span(),
            TyAst::Pointer(pointer) => pointer.span(),
            TyAst::Tuple(tuple) => tuple.span(),
            TyAst::Wildcard(span) => span,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct TyPointerAst<'a> {
    pub carrot: Span,
    pub mutability: MutabilityAst<'a>,
    pub ty: TyAst<'a>,
}

impl<'a> Ast<'a> for TyPointerAst<'a> {
    type Args = ();

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        Some(TyPointerAst {
            carrot: ctx.advance().span,
            mutability: ctx.parse()?,
            ty: ctx.parse()?,
        })
    }

    fn span(&self) -> Span {
        self.carrot.joined(self.ty.span())
    }
}

#[derive(Clone, Copy, Debug)]
pub enum MutabilityAst<'a> {
    Mut(Span),
    None,
    Generic(Span, PathAst<'a>),
}

impl<'a> Ast<'a> for MutabilityAst<'a> {
    type Args = ();

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        Some(branch! {ctx => {
            Mut => Self::Mut(ctx.advance().span),
            Use => Self::Generic(ctx.advance().span, ctx.parse()?),
            @ => Self::None,
        }})
    }

    fn span(&self) -> Span {
        match self {
            MutabilityAst::Mut(span) => *span,
            MutabilityAst::None => Span::default(),
            MutabilityAst::Generic(span, _) => *span,
        }
    }
}
