use scope::Vis;

use super::*;

impl Parser<'_> {
    pub fn r#fn(&mut self) -> errors::Result {
        let vis = self.signature_unfinished()?;

        if self.at(TokenKind::Extern) {
            self.advance();
            self.ast_data.cache(Ast::none());
        } else {
            self.start();
            list!(self, LeftCurly, NewLine, RightCurly, expr)?;
            self.finish(AstKind::FuncBody);
        }

        self.finish(AstKind::Func { vis });

        Ok(())
    }

    fn signature_unfinished(&mut self) -> errors::Result<Vis> {
        self.start();
        self.advance();
        let vis = self.visibility();
        self.call_conv();
        self.generics()?;
        self.ident()?;
        list!(self, LeftParen, Comma, RightParen, fn_arg)?;
        if self.at(TokenKind::RightArrow) {
            self.advance();
            self.ty()?;
        } else {
            self.ast_data.cache(Ast::none());
        }
        Ok(vis)
    }

    pub fn signature(&mut self) -> errors::Result {
        let vis = self.signature_unfinished()?;
        self.finish(AstKind::FuncSignature { vis });
        Ok(())
    }

    fn call_conv(&mut self) {
        if self.at(TokenKind::String) {
            self.capture(AstKind::String);
        } else {
            self.ast_data.cache(Ast::none());
        }
    }

    fn fn_arg(&mut self) -> errors::Result {
        self.start();
        let mutable = self.advance_if(TokenKind::Mut);
        self.ident()?;
        self.expect(TokenKind::Colon)?;
        self.advance();
        self.ty()?;
        self.finish(AstKind::FuncArg { mutable });
        Ok(())
    }
}
