use super::*;

impl Parser<'_> {
    pub fn parse_manifest(&mut self) -> Maybe<AstList> {
        self.parse_with(Self::take_manifest).0
    }

    fn take_manifest(&mut self) -> errors::Result {
        list!(self, none, NewLine, none, manifest_field)
    }

    fn manifest_field(&mut self) -> errors::Result {
        self.start();
        let name = self.state.current.span;
        self.expect(TokenKind::Ident)?;
        self.capture(AstKind::Ident);
        branch! { self => {
            Colon => {
                self.advance();
                self.expect(TokenKind::String)?;
                self.capture(AstKind::String);
                self.finish(AstKind::ManifestField);
            },
            LeftCurly => {
                let parser = match self.lexer.display(name) {
                    "deps" => Self::dep,
                    _ => Self::manifest_field,
                };
                self.start();
                list!(self, LeftCurly, NewLine, RightCurly, exp parser)?;
                self.finish(AstKind::ManifestSection);

                self.finish(AstKind::ManifestImports);
            },
        }}
        Ok(())
    }

    fn dep(&mut self) -> errors::Result {
        self.start();
        self.optional(TokenKind::Ident, |s| Ok(s.capture(AstKind::Ident)))?;
        let use_git = self.ctx_keyword("git");
        self.expect(TokenKind::String)?;
        self.capture(AstKind::String);
        self.finish(AstKind::ManifestImport { use_git });
        Ok(())
    }
}