use super::*;

impl Parser<'_> {
    pub fn bound(&mut self) -> errors::Result {
        self.start();
        self.advance();
        let vis = self.visibility();
        self.generics(false)?;
        self.ident()?;
        self.start();
        list!(self, LeftCurly, NewLine, RightCurly, bound_item)?;
        self.finish(AstKind::BoundBody);
        self.finish(AstKind::Bound { vis });
        Ok(())
    }

    fn bound_item(&mut self) -> errors::Result {
        branch! { self => {
            Fn => self.signature()?,
            Type => self.bound_type()?,
        }};
        Ok(())
    }

    fn bound_type(&mut self) -> errors::Result {
        self.start();
        self.advance();
        let vis = self.visibility();
        self.generics(false)?;
        self.ident()?;

        if self.at(TokenKind::Colon) {
            self.advance();
            self.ty()?;
        } else {
            self.ast_data.cache(AstEnt::none());
        }

        self.finish(AstKind::BoundType { vis });

        Ok(())
    }

    pub fn r#impl(&mut self) -> errors::Result {
        self.start();
        self.advance();
        let vis = self.visibility();
        self.generics(true)?;
        self.ty()?;

        let is_bound_impl = self.at(TokenKind::For);

        if is_bound_impl {
            self.advance();
            self.ty()?;
        }

        self.start();
        opt_list!(self, LeftCurly, NewLine, RightCurly, impl_item)?;
        self.finish(AstKind::ImplBody);

        if is_bound_impl {
            self.finish(AstKind::BoundImpl { vis });
        } else {
            self.finish(AstKind::Impl { vis });
        }

        Ok(())
    }

    fn impl_item(&mut self) -> errors::Result {
        branch! { self => {
            Fn => self.r#fn()?,
            Type => self.impl_type()?,
            Use => self.impl_use()?,
        }};
        Ok(())
    }

    fn impl_type(&mut self) -> errors::Result {
        self.start();
        self.advance();
        self.generics(true)?;
        self.ident()?;

        self.expect_ctx_keyword("=")?;
        self.advance();

        self.ty()?;

        self.finish(AstKind::ImplType);

        Ok(())
    }

    fn impl_use(&mut self) -> errors::Result {
        self.start();
        self.advance();
        self.ty()?;
        self.expect(TokenKind::As)?;
        self.advance();
        self.ident()?;
        self.finish(AstKind::ImplUse);

        Ok(())
    }
}
