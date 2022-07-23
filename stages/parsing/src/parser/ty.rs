use super::*;

impl Parser<'_> {
    pub fn ty(&mut self) -> errors::Result {
        branch! { self => {
            Ident => self.ident_ty()?,
            Operator(_ = 0) => branch!{str self => {
                "^" => self.pointer_ty(),
            }},
        }}

        Ok(())
    }

    pub fn pointer_ty(&mut self) -> errors::Result {
        self.start();
        self.advance();
        let mutable = self.advance_if(TokenKind::Mut);
        self.ty()?;
        self.finish(AstKind::PtrTy { mutable });
        Ok(())
    }

    pub fn ident_ty(&mut self) -> errors::Result {
        self.start();
        self.ident_chain()?;
        if self.at(TokenKind::LeftBracket) {
            list!(self, LeftBracket, Comma, RightBracket, ty)?;
            self.finish(AstKind::TyInstance);
        } else {
            self.join_frames();
        }
        Ok(())
    }
}
