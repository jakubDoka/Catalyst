use super::*;

impl Parser<'_> {
    pub fn parse_imports(&mut self) -> Option<AstEnt> {
        let (list, ..) = self.parse_with(Self::take_imports);

        let Some(list) = list.expand() else {
            return None;
        };

        let Some(&last) = self.ast_data.get(list).last() else {
            unreachable!();
        };

        Some(last)
    }

    pub(crate) fn take_imports(&mut self) -> errors::Result {
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
            self.ast_data.cache(AstEnt::none());
        }
        self.capture(AstKind::String);
        self.finish(AstKind::Import);
        Ok(())
    }
}
