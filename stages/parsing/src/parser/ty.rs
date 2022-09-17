use super::*;

list_meta!(TyGenericsMeta ? LeftBracket Comma RightBracket);
pub type TyGenericsAst<'a> = ListAst<'a, TyAst<'a>, TyGenericsMeta>;

list_meta!(TyTupleMeta LeftParen Comma RightParen);
pub type TyTupleAst<'a> = ListAst<'a, TyAst<'a>, TyTupleMeta>;

#[derive(Clone, Copy, Debug)]
pub enum TyAst<'a> {
    Path(PathAst<'a>),
    Instance(TyInstanceAst<'a>),
    Pointer(&'a TyPointerAst<'a>),
    Tuple(TyTupleAst<'a>),
}

impl<'a> Ast<'a> for TyAst<'a> {
    type Args = ();

    const NAME: &'static str = "type";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Result<Self, ()> {
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
                "^" => ctx.parse()
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
    pub ident: PathAst<'a>,
    pub params: TyGenericsAst<'a>,
}

impl<'a> Ast<'a> for TyInstanceAst<'a> {
    type Args = (PathAst<'a>,);

    const NAME: &'static str = "type instance";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (ident,): Self::Args) -> Result<Self, ()> {
        Ok(TyInstanceAst {
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

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Result<Self, ()> {
        Ok(TyPointerAst {
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

    const NAME: &'static str = "mutability";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Result<Self, ()> {
        branch! {ctx => {
            Mut => Ok(Self::Mut(ctx.advance().span)),
            Use => Ok(Self::Generic(ctx.advance().span, ctx.parse()?)),
            _ => Ok(Self::None),
        }}
    }

    fn span(&self) -> Span {
        todo!()
    }
}

// impl Parser<'_> {
//     pub fn ty(&mut self) -> errors::Result {
//         self.ty_low().map(|_| ())
//     }

//     pub fn ty_low(&mut self) -> errors::Result<bool> {
//         branch! { self => {
//             Ident => if self.next(TokenKind::Colon) { self.field_ty()?; return Ok(true) }
//                 else { self.ident_ty()? },
//             Operator(_ = 0) => branch!{str self => {
//                 "^" => self.pointer_ty(),
//             }},
//         }}

//         Ok(false)
//     }

//     fn field_ty(&mut self) -> errors::Result {
//         let start = self.start();
//         self.capture(AstKind::Ident);
//         self.advance();
//         self.ty()?;
//         self.finish_last(AstKind::FieldTy, start);
//         Ok(())
//     }

//     fn pointer_ty(&mut self) -> errors::Result {
//         let start = self.start();
//         self.advance();
//         if self.at(TokenKind::Mut) {
//             self.capture(AstKind::PointerMut);
//         } else if self.at(TokenKind::Use) {
//             self.ty()?;
//         } else {
//             self.ast_data.cache(Ast::none());
//         }
//         self.ty()?;
//         self.finish_last(AstKind::PointerTy, start);
//         Ok(())
//     }

//     fn ident_ty(&mut self) -> errors::Result {
//         let start = self.start();
//         self.ident_chain()?;
//         if self.at(TokenKind::LeftBracket) {
//             let mut has_fields = false;
//             let end = list!(
//                 self,
//                 LeftBracket,
//                 Comma,
//                 RightBracket,
//                 exp | s | s.ty_low().map(|val| has_fields |= val)
//             )?;
//             if has_fields {
//                 self.finish(AstKind::BoundInstance, start.joined(end));
//             } else {
//                 self.finish(AstKind::TyInstance, start.joined(end));
//             }
//         } else {
//             self.join_frames();
//         }
//         Ok(())
//     }
// }
