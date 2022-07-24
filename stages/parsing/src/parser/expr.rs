use super::*;

impl Parser<'_> {
    pub fn expr(&mut self) -> errors::Result {
        self.start();
        self.unit_expr()?;
        if let TokenKind::Operator(precedence) = self.state.current.kind {
            self.op()?;
            self.binary_expr(precedence)?;
        } else {
            self.join_frames();
        }

        Ok(())
    }

    pub fn binary_expr(&mut self, precedence: u8) -> errors::Result {
        loop {
            self.expr()?;

            let TokenKind::Operator(next_precedence) = self.state.current.kind else {
                self.finish(AstKind::Binary);
                break;
            };

            if precedence < next_precedence {
                self.finish(AstKind::Binary);
                self.start_with(1);
                self.op()?;
            } else {
                self.op()?;
                self.binary_expr(next_precedence)?;
                self.finish(AstKind::Binary);
                break;
            }
        }

        Ok(())
    }

    pub fn unit_expr(&mut self) -> errors::Result {
        branch! { self => {
            Return => self.r#return()?,
            Int => self.capture(AstKind::Int),
            Ident => self.capture(AstKind::Ident),
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

    pub fn op(&mut self) -> errors::Result {
        self.start();
        self.capture(AstKind::Operator);
        if self.at(TokenKind::Tick) {
            self.advance();
            self.capture(AstKind::Ident);
            self.finish(AstKind::OperatorWithModule);
        } else {
            self.join_frames();
        }

        Ok(())
    }
}
