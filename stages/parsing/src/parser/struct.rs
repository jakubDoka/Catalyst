use super::*;

list_meta!(StructBodyMeta ? LeftCurly NewLine RightCurly);
pub type StructBodyAst<'a> = ListAst<'a, StructFieldAst<'a>, StructBodyMeta>;

#[derive(Clone, Copy, Debug)]
pub struct StructAst<'a> {
    pub vis: Vis,
    pub generics: GenericsAst<'a>,
    pub name: NameAst,
    pub body: StructBodyAst<'a>,
    pub span: Span,
}

impl<'a> Ast<'a> for StructAst<'a> {
    type Args = (Vis, Span);

    const NAME: &'static str = "struct";

    fn parse_args_internal(
        ctx: &mut ParsingCtx<'_, 'a>,
        (vis, start): Self::Args,
    ) -> Result<Self, ()> {
        ctx.advance();
        let generics = GenericsAst::parse(ctx)?;
        let name = NameAst::parse(ctx)?;
        let body = StructBodyAst::parse(ctx)?;

        Ok(StructAst {
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

    const NAME: &'static str = "struct field";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Result<Self, ()> {
        Ok(StructFieldAst {
            start: ctx.state.current.span,
            vis: ctx.visibility(),
            used: ctx.try_advance(TokenKind::Use),
            mutable: ctx.try_advance(TokenKind::Mut),
            name: Ast::parse(ctx)?,
            ty: {
                ctx.expect_advance(TokenKind::Colon)?;
                TyAst::parse(ctx)?
            },
        })
    }

    fn span(&self) -> Span {
        self.start.joined(self.ty.span())
    }
}

// impl Parser<'_> {
//     pub fn r#struct(&mut self) -> errors::Result {
//         let start = self.start();
//         self.advance();
//         let vis = self.visibility();
//         self.generics()?;
//         self.ident()?;

//         self.start();
//         if let Some(span) = opt_list!(self, LeftCurly, NewLine, RightCurly, struct_field)? {
//             self.finish(AstKind::StructBody, span);
//         } else {
//             self.ast_data.cache(Ast::none());
//             self.join_frames();
//         }

//         self.finish_last(AstKind::Struct { vis }, start);

//         Ok(())
//     }

//     pub fn struct_field(&mut self) -> errors::Result {
//         let start = self.start();
//         let vis = self.visibility();
//         let exported = self.advance_if(TokenKind::Use);
//         let mutable = self.advance_if(TokenKind::Mut);
//         self.ident()?;
//         self.expect(TokenKind::Colon)?;
//         self.advance();
//         self.ty()?;
//         self.finish_last(
//             AstKind::StructField {
//                 vis,
//                 mutable,
//                 exported,
//             },
//             start,
//         );
//         Ok(())
//     }
// }
