use super::*;

list_meta!(ImportsMeta LeftCurly NewLine RightCurly);
pub type ImportsAst<'a> = ListAst<'a, ImportAst, ImportsMeta>;

#[derive(Clone, Copy, Debug)]
pub struct UseAstSkip;

impl<'a> Ast<'a> for UseAstSkip {
    type Args = ();

    const NAME: &'static str = "use skip";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
        ctx.skip(TokenKind::NewLine);
        if !ctx.at_tok(TokenKind::Use) {
            return Some(UseAstSkip);
        }

        while matches!(ctx.advance().kind, TokenKind::RightCurly | TokenKind::Eof) {}

        Some(UseAstSkip)
    }

    fn span(&self) -> Span {
        Span::default()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct UseAst<'a> {
    pub use_span: Span,
    pub items: ImportsAst<'a>,
}

impl<'a> Ast<'a> for UseAst<'a> {
    type Args = ();

    const NAME: &'static str = "use";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
        ctx.skip(TokenKind::NewLine);

        if !ctx.at_tok(TokenKind::Use) {
            return None;
        }

        Some(UseAst {
            use_span: ctx.advance().span,
            items: ImportsAst::parse(ctx)?,
        })
    }

    fn span(&self) -> Span {
        self.use_span.joined(self.items.span())
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ImportAst {
    pub vis: Vis,
    pub name: NameAst,
    pub path: Span,
    pub span: Span,
}

impl<'a> Ast<'a> for ImportAst {
    type Args = ();

    const NAME: &'static str = "import";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
        let start = ctx.state.current.span;
        let vis = ctx.visibility();
        let name = ctx.parse_args((true,));
        let path = ctx.expect_advance(TokenKind::String)?.span;
        let span = start.joined(path);
        let path = path.shrink(1);
        let name = name.unwrap_or_else(|| NameAst::from_path(ctx, path));
        Some(ImportAst {
            vis,
            name,
            path,
            span,
        })
    }

    fn span(&self) -> Span {
        self.span
    }
}

// impl Parser<'_> {
//     pub fn skip_imports(&mut self) {
//         self.skip_newlines();
//         if self.at(TokenKind::Use) {
//             self.advance();
//             while self.state.current.kind != TokenKind::RightCurly
//                 && self.state.current.kind != TokenKind::Eof
//             {
//                 self.advance();
//             }
//             self.state.last_break = Some(self.state.current.span.end());
//             self.advance();
//         }
//     }

//     pub fn parse_imports(&mut self) -> Option<Ast> {
//         let (list, ..) = self.parse_with(Self::take_imports);
//         self.ast_data.get(list).last().copied()
//     }

//     pub(crate) fn take_imports(&mut self) -> Option<()> {
//         self.skip_newlines();
//         self.optional(TokenKind::Use, Self::imports)
//     }

//     fn imports(&mut self) -> Option<()> {
//         let start = self.start();
//         self.advance();
//         let end = list!(self, LeftCurly, NewLine, RightCurly, import)?;
//         self.finish(AstKind::Imports, start.joined(end));
//         self.state.last_break = Some(end.end());
//         Ok(())
//     }

//     fn import(&mut self) -> Option<()> {
//         let start = self.start();
//         if self.state.current.kind == TokenKind::Ident {
//             self.capture(AstKind::Ident);
//         } else {
//             self.ast_data.cache(Ast::none());
//         }
//         self.capture(AstKind::String);
//         self.finish_last(AstKind::Import, start);
//         Ok(())
//     }
// }
