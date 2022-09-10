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
        let start = self.start();
        self.advance();
        let end = list!(self, LeftCurly, NewLine, RightCurly, import)?;
        self.finish(AstKind::Imports, start.joined(end));
        Ok(())
    }

    fn import(&mut self) -> errors::Result {
        let start = self.start();
        if self.state.current.kind == TokenKind::Ident {
            self.capture(AstKind::Ident);
        } else {
            self.ast_data.cache(Ast::none());
        }
        self.capture(AstKind::String);
        self.finish_last(AstKind::Import, start);
        Ok(())
    }
}
