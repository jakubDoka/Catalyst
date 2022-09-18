use super::*;

#[derive(Clone, Copy, Debug)]
pub enum BoundExprAst<'a> {
    Path(PathAst<'a>),
}

impl<'a> Ast<'a> for BoundExprAst<'a> {
    type Args = ();

    const NAME: &'static str = "bound expr";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
        let chain = PathAst::parse(ctx)?;
        Some(Self::Path(chain))
    }

    fn span(&self) -> Span {
        todo!()
    }
}

// impl Parser<'_> {
//     pub fn bound(&mut self) -> Option<()> {
//         let start = self.start();
//         self.advance();
//         let vis = self.visibility();
//         self.generics()?;
//         self.ident()?;

//         self.start();
//         let end = list!(self, LeftCurly, NewLine, RightCurly, bound_item)?;
//         self.finish(AstKind::BoundBody, end);

//         self.finish(AstKind::Bound { vis }, start.joined(end));
//         Ok(())
//     }

//     fn bound_item(&mut self) -> Option<()> {
//         branch! { self => {
//             Func => self.signature()?,
//             Type => self.bound_type()?,
//         }};
//         Ok(())
//     }

//     fn bound_type(&mut self) -> Option<()> {
//         let start = self.start();
//         self.advance();
//         let vis = self.visibility();
//         self.generics()?;
//         self.ident()?;

//         self.finish_last(AstKind::BoundType { vis }, start);

//         Ok(())
//     }

//     pub fn r#impl(&mut self) -> Option<()> {
//         let start = self.start();
//         self.advance();
//         let vis = self.visibility();
//         self.generics()?;
//         self.ty()?;

//         let is_bound_impl = self.at(TokenKind::For);

//         if is_bound_impl {
//             self.advance();
//             self.ty()?;
//         }

//         self.start();
//         if let Some(span) = opt_list!(self, LeftCurly, NewLine, RightCurly, impl_item)? {
//             self.finish(AstKind::ImplBody, span);
//         } else {
//             self.ast_data.cache(Ast::none());
//         }

//         let kind = if is_bound_impl {
//             AstKind::BoundImpl { vis }
//         } else {
//             AstKind::Impl { vis }
//         };
//         self.finish_last(kind, start);

//         Ok(())
//     }

//     fn impl_item(&mut self) -> Option<()> {
//         branch! { self => {
//             Func => self.r#fn()?,
//             Type => self.impl_type()?,
//             Use => self.impl_use()?,
//         }};
//         Ok(())
//     }

//     fn impl_type(&mut self) -> Option<()> {
//         let start = self.start();
//         self.advance();
//         self.generics()?;
//         self.ident()?;

//         self.expect_ctx_keyword("=")?;

//         self.ty()?;

//         self.finish_last(AstKind::ImplType, start);

//         Ok(())
//     }

//     fn impl_use(&mut self) -> Option<()> {
//         let start = self.start();
//         self.advance();
//         self.ty()?;
//         self.expect(TokenKind::As)?;
//         self.advance();
//         self.ident()?;
//         self.finish_last(AstKind::ImplUse, start);

//         Ok(())
//     }
// }
