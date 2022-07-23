use super::*;

impl Parser<'_> {
    pub fn expr(&mut self) -> errors::Result {
        branch! { self => {
            Return => self.r#return()?,
            Int => self.capture(AstKind::Int),
        }};

        Ok(())
    }

    pub fn r#return(&mut self) -> errors::Result {
        self.start();
        self.advance();
        if self.at(TokenKind::NewLine) {
            self.ast_data.cache(AstEnt::none());
        } else {
            self.expr()?;
        }
        self.finish(AstKind::Return);
        Ok(())
    }
}
