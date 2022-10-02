use super::*;

list_meta!(TyGenericsMeta ? LeftBracket Comma RightBracket);
pub type TyGenericsAst<'a> = ListAst<'a, TyAst<'a>, TyGenericsMeta>;

list_meta!(TyTupleMeta LeftParen Comma RightParen);
pub type TyTupleAst<'a> = ListAst<'a, TyAst<'a>, TyTupleMeta>;

#[derive(Clone, Copy, Debug)]
pub enum TyAst<'a> {
    Path(PathExprAst<'a>),
    Instance(TyInstanceAst<'a>),
    Pointer(&'a TyPointerAst<'a>),
    Tuple(TyTupleAst<'a>),
}

impl<'a> Ast<'a> for TyAst<'a> {
    type Args = ();

    const NAME: &'static str = "type";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
        branch! {ctx => {
            Ident => {
                let ident = ctx.parse();

                if ctx.at_tok(TokenKind::LeftBracket) {
                    Ast::parse_args(ctx, (ident?,)).map(TyAst::Instance)
                } else {
                    ident.map(TyAst::Path)
                }
            },
            LeftParen => ctx.parse().map(TyAst::Tuple),
            Operator(_ = 0) => branch!(str ctx => {
                "*" => ctx.parse()
                    .map(|p| ctx.arena.alloc(p))
                    .map(TyAst::Pointer),
            }),
        }}
    }

    fn span(&self) -> Span {
        match *self {
            TyAst::Path(ident) => ident.span(),
            TyAst::Instance(instance) => instance.span(),
            TyAst::Pointer(pointer) => pointer.span(),
            TyAst::Tuple(tuple) => tuple.span(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct TyInstanceAst<'a> {
    pub ident: PathExprAst<'a>,
    pub params: TyGenericsAst<'a>,
}

impl<'a> Ast<'a> for TyInstanceAst<'a> {
    type Args = (PathExprAst<'a>,);

    const NAME: &'static str = "type instance";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (ident,): Self::Args) -> Option<Self> {
        Some(TyInstanceAst {
            ident,
            params: ctx.parse()?,
        })
    }

    fn span(&self) -> Span {
        self.ident.span().joined(self.params.span())
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

    const NAME: &'static str = "pointer type";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
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
    Generic(Span, PathExprAst<'a>),
}

impl<'a> Ast<'a> for MutabilityAst<'a> {
    type Args = ();

    const NAME: &'static str = "mutability";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
        Some(branch! {ctx => {
            Mut => Self::Mut(ctx.advance().span),
            Use => Self::Generic(ctx.advance().span, ctx.parse()?),
            _ => Self::None,
        }})
    }

    fn span(&self) -> Span {
        todo!()
    }
}
