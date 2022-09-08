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

    fn unit_expr(&mut self) -> errors::Result {
        branch! { self => {
            Return => self.r#return()?,
            Int => self.capture(AstKind::Int),
            Ident => self.ident_expr(true)?,
        }};

        Ok(())
    }

    fn ident_expr(&mut self, has_tail: bool) -> errors::Result {
        self.ident_chain()?;

        if self.at(TokenKind::Tick) && self.next(TokenKind::LeftBracket) {
            self.instance_expr()?;
        }

        if has_tail {
            if self.at(TokenKind::Tick) && self.next(TokenKind::LeftCurly) {
                self.struct_expr()?;
                return Ok(());
            }

            loop {
                branch! {self => {
                    LeftBracket => self.index_expr()?,
                    LeftParen => self.call_expr()?,
                    Dot => self.dot_expr()?,
                    _ => {
                        if self.reduce_repetition(TokenKind::NewLine) && self.next(TokenKind::Dot) {
                            self.advance();
                            self.dot_expr()?;
                        } else {
                            break;
                        }
                    },
                }};
            }
        }

        Ok(())
    }

    fn dot_expr(&mut self) -> errors::Result {
        self.start_with(1);
        self.advance();
        self.ident_expr(false)?;
        self.finish(AstKind::DotExpr);

        Ok(())
    }

    fn call_expr(&mut self) -> errors::Result {
        self.start_with(1);
        list!(self, LeftParen, Comma, RightParen, expr)?;
        self.finish(AstKind::Call);

        Ok(())
    }

    fn index_expr(&mut self) -> errors::Result {
        self.start_with(1);
        self.advance();
        self.expr()?;
        self.expect(TokenKind::RightBracket)?;
        self.advance();
        self.finish(AstKind::Index);
        Ok(())
    }

    fn instance_expr(&mut self) -> errors::Result {
        self.start_with(1);
        list!(self, LeftBracket, Comma, RightBracket, ty)?;
        self.finish(AstKind::InstanceExpr);
        Ok(())
    }

    fn struct_expr(&mut self) -> errors::Result {
        self.start_with(1);
        list!(self, LeftBracket, Comma, RightBracket, struct_expr_field)?;
        self.finish(AstKind::StructExprBody);
        self.finish(AstKind::StructExpr);
        Ok(())
    }

    fn struct_expr_field(&mut self) -> errors::Result {
        self.start();
        self.ident()?;
        self.expect(TokenKind::Colon)?;
        self.advance();
        self.expr()?;
        self.finish(AstKind::StructExprField);
        Ok(())
    }

    fn r#return(&mut self) -> errors::Result {
        self.start();
        self.advance();
        if self.at(TokenKind::NewLine) {
            self.ast_data.cache(Ast::none());
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
