use scope::Vis;

use super::*;

impl Parser<'_> {
    pub fn r#fn(&mut self) -> errors::Result {
        let start = self.state.current.span;
        let vis = self.signature_unfinished()?;

        if self.at(TokenKind::Extern) {
            self.capture(AstKind::None);
        } else {
            self.start();
            let span = list!(self, LeftCurly, NewLine, RightCurly, expr)?;
            self.finish(AstKind::FuncBody, span);
        }

        self.finish_last(AstKind::Func { vis }, start);

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
        let start = self.state.current.span;
        let vis = self.signature_unfinished()?;
        self.finish_last(AstKind::FuncSignature { vis }, start);
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
        let start = self.start();
        let mutable = self.advance_if(TokenKind::Mut);
        self.ident()?;
        self.expect(TokenKind::Colon)?;
        self.advance();
        self.ty()?;
        self.finish_last(AstKind::FuncArg { mutable }, start);
        Ok(())
    }
}
