use super::*;

impl Parser<'_> {
    pub fn parse_manifest(&mut self) -> VSlice<Ast> {
        self.parse_with(Self::take_manifest).0
    }

    fn take_manifest(&mut self) -> errors::Result {
        list!(self, none, NewLine, Eof, manifest_field).map(|_| ())
    }

    fn manifest_field(&mut self) -> errors::Result {
        let start = self.start();
        let name = self.state.current.span;
        self.expect(TokenKind::Ident)?;
        self.capture(AstKind::Ident);
        branch! { self => {
            Colon => {
                self.advance();
                self.expect(TokenKind::String)?;
                self.capture(AstKind::String);
                self.finish_last(AstKind::ManifestField, start);
            },
            LeftCurly => {
                let parser = match self.lexer.inner_span_str(name) {
                    "deps" => Self::dep,
                    _ => Self::manifest_field,
                };
                self.start();
                let span = list!(self, LeftCurly, NewLine, RightCurly, exp parser)?;
                self.finish_last(AstKind::ManifestSection, span);

                self.finish_last(AstKind::ManifestImports, start);
            },
        }}
        Ok(())
    }

    fn dep(&mut self) -> errors::Result {
        let start = self.start();
        self.optional(TokenKind::Ident, |s| Ok(s.capture(AstKind::Ident)))?;
        let use_git = self.ctx_keyword("git");
        self.expect(TokenKind::String)?;
        self.capture(AstKind::String);

        if self.state.current.kind == TokenKind::String {
            self.capture(AstKind::String);
        } else {
            self.ast_data.cache(Ast::none());
        }

        self.finish_last(AstKind::ManifestImport { use_git }, start);
        Ok(())
    }
}
