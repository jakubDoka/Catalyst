use diags::{ctl_errors, SourceLoc};

use super::*;

impl<'ctx, 'arena, M: TokenMeta> Parser<'ctx, 'arena, M> {
    pub fn manifest(&mut self) -> Option<ManifestAst<'arena, M>> {
        let mut fields = bumpvec![];
        let mut deps = None;
        loop {
            self.skip(TokenKind::NewLine);

            if self.at(TokenKind::Eof) {
                break;
            }

            if let Some(deps_span) = self.try_advance("deps") && deps.is_none() {
                deps = Some(ManifestDepsAst {
                    deps: deps_span,
                    list: self.object("dependency list", Self::manifest_dep)?,
                });
                continue;
            }

            fields.push(self.manifest_field()?);
        }

        Some(ManifestAst {
            fields: self.arena.alloc_slice(&fields),
            deps,
        })
    }

    fn manifest_field(&mut self) -> Option<ManifestFieldAst<'arena, M>> {
        Some(ManifestFieldAst {
            name: self.name("manifest field")?,
            colon: self.expect(Tk::Colon, |s| MissingColon {
                loc: s.loc(),
                something: "manifest field",
                found: s.state.current.kind,
            })?,
            value: self.manifest_value()?,
        })
    }

    fn manifest_value(&mut self) -> Option<ManifestValueAst<'arena, M>> {
        branch! {self => {
            Str => Some(ManifestValueAst::String(self.advance())),
            LeftBracket => self.array("manifest list", Self::manifest_value)
                .map(ManifestValueAst::Array),
            LeftBrace => self.list("manifest object", Self::manifest_field, Tk::LeftBrace, Tk::NewLine, Tk::RightBrace)
                .map(ManifestValueAst::Object),
            @"manifest value",
        }}
    }

    fn manifest_dep(&mut self) -> Option<ManifestDepAst<M>> {
        Some(ManifestDepAst {
            git: self.try_advance("git"),
            name: self.at(Tk::Ident).then(|| self.name_unchecked()),
            path: self.expect(TokenKind::Str, |s| ExpectedDepStringPath {
                got: s.state.current.kind,
                loc: s.loc(),
            })?,
            version: self.try_advance(TokenKind::Str),
        })
    }
}

ctl_errors! {
    #[err => "expected string path for dependency but got {got}"]
    error ExpectedDepStringPath: fatal {
        #[err loc]
        got: TokenKind,
        loc: SourceLoc,
    }
}
