use super::*;

impl Parser<'_> {
    pub fn skip_imports(&mut self) {
        self.skip_newlines();
        if self.at(TokenKind::Use) {
            self.advance();
            while self.state.current.kind != TokenKind::RightCurly
                && self.state.current.kind != TokenKind::Eof
            {
                self.advance();
            }
            self.advance();
        }
    }

    pub fn parse_imports(&mut self) -> Option<Ast> {
        let (list, ..) = self.parse_with(Self::take_imports);
        self.ast_data.get(list).last().copied()
    }

    pub(crate) fn take_imports(&mut self) -> errors::Result {
        self.skip_newlines();
        self.optional(TokenKind::Use, Self::imports)
    }

    fn imports(&mut self) -> errors::Result {
        self.start();
        self.advance();
        list!(self, LeftCurly, NewLine, RightCurly, import)?;
        self.finish(AstKind::Imports);
        Ok(())
    }

    fn import(&mut self) -> errors::Result {
        self.start();
        if self.state.current.kind == TokenKind::Ident {
            self.capture(AstKind::Ident);
        } else {
            self.ast_data.cache(Ast::none());
        }
        self.capture(AstKind::String);
        self.finish(AstKind::Import);
        Ok(())
    }
}
