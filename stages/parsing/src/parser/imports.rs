use diags::*;

use super::*;

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

    pub fn imports(&mut self) -> Option<Option<ImportsAst<'arena, M>>> {
        let Some(r#use) = self.try_advance(Tk::Use) else {
            return Some(None);
        };

        Some(Some(ImportsAst {
            r#use,
            items: self.object("import list", Self::import)?,
        }))
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

// impl<'a> Ast<'a> for UseAst<'a> {
//     type Args = ();

//     fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
//         ctx.skip(TokenKind::NewLine);

//         if !ctx.at(TokenKind::Use) {
//             return Some(default());
//         }

//         Some(UseAst {
//             use_span: ctx.advance().span,
//             items: ctx.parse_args(BLOCK_SYNTAX.into())?,
//         })
//     }

//     fn span(&self) -> Span {
//         self.use_span.joined(self.items.span())
//     }
// }

// impl<'a> Ast<'a> for ImportAst {
//     type Args = ();

//     fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
//         let start = ctx.state.current.span;
//         let vis = ctx.visibility();
//         let name = ctx.parse_args((true, "import name"));
//         let path = ctx
//             .expect_advance(TokenKind::String, |ctx| ExpectedModStringPath {
//                 found: ctx.state.current.kind,
//                 loc: ctx.loc(),
//             })?
//             .span;
//         let span = start.joined(path);
//         let path = path.shrink(1);
//         let name = name.unwrap_or_else(|| NameAst::from_path(ctx, path));
//         Some(ImportAst {
//             vis,
//             name,
//             path,
//             span,
//         })
//     }

//     fn span(&self) -> Span {
//         self.span
//     }
// }

ctl_errors! {
    #[err => "module path must be a string literal, but got {found}"]
    error ExpectedModStringPath: fatal {
        #[err loc]
        found: TokenKind,
        loc: SourceLoc,
    }
}
