use super::*;

impl Parser<'_> {
    pub fn r#struct(&mut self) -> errors::Result {
        let start = self.start();
        self.advance();
        let vis = self.visibility();
        self.generics()?;
        self.ident()?;

        self.start();
        if let Some(span) = opt_list!(self, LeftCurly, NewLine, RightCurly, struct_field)? {
            self.finish(AstKind::StructBody, span);
        } else {
            self.ast_data.cache(Ast::none());
            self.join_frames();
        }

        self.finish_last(AstKind::Struct { vis }, start);

        Ok(())
    }

    pub fn struct_field(&mut self) -> errors::Result {
        let start = self.start();
        let vis = self.visibility();
        let exported = self.advance_if(TokenKind::Use);
        let mutable = self.advance_if(TokenKind::Mut);
        self.ident()?;
        self.expect(TokenKind::Colon)?;
        self.advance();
        self.ty()?;
        self.finish_last(
            AstKind::StructField {
                vis,
                mutable,
                exported,
            },
            start,
        );
        Ok(())
    }
}
