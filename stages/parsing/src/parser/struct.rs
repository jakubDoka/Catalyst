use super::*;

impl Parser<'_> {
    pub fn r#struct(&mut self) -> errors::Result {
        self.start();
        self.advance();
        let vis = self.visibility();
        self.generics()?;
        self.ident()?;

        self.start();
        opt_list!(self, LeftCurly, NewLine, RightCurly, struct_field)?;
        self.finish(AstKind::StructBody);

        self.finish(AstKind::Struct { vis });

        Ok(())
    }

    pub fn struct_field(&mut self) -> errors::Result {
        self.start();
        let vis = self.visibility();
        let exported = self.advance_if(TokenKind::Use);
        let mutable = self.advance_if(TokenKind::Mut);
        self.ident()?;
        self.expect(TokenKind::Colon)?;
        self.advance();
        self.ty()?;
        self.finish(AstKind::StructField {
            vis,
            mutable,
            exported,
        });
        Ok(())
    }
}
