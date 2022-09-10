use super::*;

impl Parser<'_> {
    pub fn ty(&mut self) -> errors::Result {
        self.ty_low().map(|_| ())
    }

    pub fn ty_low(&mut self) -> errors::Result<bool> {
        branch! { self => {
            Ident => if self.next(TokenKind::Colon) { self.field_ty()?; return Ok(true) }
                else { self.ident_ty()? },
            Operator(_ = 0) => branch!{str self => {
                "^" => self.pointer_ty(),
            }},
        }}

        Ok(false)
    }

    fn field_ty(&mut self) -> errors::Result {
        let start = self.start();
        self.capture(AstKind::Ident);
        self.advance();
        self.ty()?;
        self.finish_last(AstKind::FieldTy, start);
        Ok(())
    }

    fn pointer_ty(&mut self) -> errors::Result {
        let start = self.start();
        self.advance();
        if self.at(TokenKind::Mut) {
            self.capture(AstKind::PointerMut);
        } else if self.at(TokenKind::Use) {
            self.ty()?;
        } else {
            self.ast_data.cache(Ast::none());
        }
        self.ty()?;
        self.finish_last(AstKind::PointerTy, start);
        Ok(())
    }

    fn ident_ty(&mut self) -> errors::Result {
        let start = self.start();
        self.ident_chain()?;
        if self.at(TokenKind::LeftBracket) {
            let mut has_fields = false;
            let end = list!(
                self,
                LeftBracket,
                Comma,
                RightBracket,
                exp | s | s.ty_low().map(|val| has_fields |= val)
            )?;
            if has_fields {
                self.finish(AstKind::BoundInstance, start.joined(end));
            } else {
                self.finish(AstKind::TyInstance, start.joined(end));
            }
        } else {
            self.join_frames();
        }
        Ok(())
    }
}
