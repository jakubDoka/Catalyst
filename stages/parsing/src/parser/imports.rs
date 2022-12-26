use diags::*;

use super::*;

type ImportsAstResult<'arena, M> = Option<(Option<ImportsAst<'arena, M>>, Option<SourceInfo<M>>)>;

impl<'ctx, 'arena, M: TokenMeta> Parser<'ctx, 'arena, M> {
    pub fn skip_imports(&mut self) -> Option<()> {
        self.skip(Tk::NewLine);

        self.try_advance(Tk::Use)?;

        while !self.at([Tk::RightBrace, Tk::Eof]) {
            self.advance();
        }

        self.advance();

        Some(())
    }

    pub fn imports(&mut self) -> ImportsAstResult<'arena, M> {
        let header = self.skip(Tk::NewLine);
        let Some(keyword) = self.try_advance(Tk::Use) else {
            return Some((None, header));
        };

        Some((
            Some(ImportsAst {
                keyword,
                items: self.object("import list", Self::import)?,
            }),
            header,
        ))
    }

    fn import(&mut self) -> Option<ImportAst<M>> {
        Some(ImportAst {
            vis: self.vis(),
            name: self.at(Tk::Ident).then(|| self.name_unchecked()),
            path: self.expect(Tk::Str, |s| ExpectedModStringPath {
                found: s.state.current.kind,
                loc: s.loc(),
            })?,
        })
    }
}

ctl_errors! {
    #[err => "module path must be a string literal, but got {found}"]
    error ExpectedModStringPath: fatal {
        #[err loc]
        found: TokenKind,
        loc: SourceLoc,
    }
}
