use super::*;

list_meta!(DepsMeta LeftBracket NewLine RightBracket);
pub type ManifestDepsAst<'a> = ListAst<'a, ManifestDepAst, DepsMeta>;

list_meta!(ManifestObjectMeta LeftBracket NewLine RightBracket);
pub type ManifestObjectAst<'a> = ListAst<'a, ManifestFieldAst<'a>, ManifestObjectMeta>;

list_meta!(ManifestListMeta LeftBracket Comma RightBracket);
pub type ManifestListAst<'a> = ListAst<'a, ManifestValueAst<'a>, ManifestListMeta>;

#[derive(Clone, Copy, Debug)]
pub struct ManifestAst<'a> {
    pub fields: &'a [ManifestFieldAst<'a>],
    pub deps_span: Maybe<Span>,
    pub deps: ManifestDepsAst<'a>,
}

impl ManifestAst<'_> {
    pub fn find_field(&self, name: Ident) -> Option<ManifestFieldAst> {
        self.fields
            .iter()
            .find(|field| field.name.ident == name)
            .copied()
    }
}

impl<'a> Ast<'a> for ManifestAst<'a> {
    type Args = ();

    const NAME: &'static str = "manifest";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Result<Self, ()> {
        let mut fields = bumpvec![];
        let mut deps = None;
        let mut deps_span = None;
        loop {
            ctx.skip(TokenKind::NewLine);

            if ctx.at_tok(TokenKind::Eof) {
                break;
            }

            if let Some(token) = ctx.optional_advance("deps") && deps.is_none() {
                deps_span = Some(token.span);
                deps = Some(ManifestDepsAst::parse(ctx)?);
                continue;
            }

            fields.push(ManifestFieldAst::parse(ctx)?);
        }

        Ok(Self {
            fields: ctx.arena.alloc_slice(&fields),
            deps_span: deps_span.into(),
            deps: deps.unwrap_or_default(),
        })
    }

    fn span(&self) -> Span {
        todo!()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ManifestFieldAst<'a> {
    pub name: NameAst,
    pub value: ManifestValueAst<'a>,
}

impl<'a> Ast<'a> for ManifestFieldAst<'a> {
    type Args = ();

    const NAME: &'static str = "manifest field";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Result<Self, ()> {
        Ok(ManifestFieldAst {
            name: ctx.parse()?,
            value: {
                ctx.expect_advance(TokenKind::Colon)?;
                ManifestValueAst::parse(ctx)?
            },
        })
    }

    fn span(&self) -> Span {
        todo!()
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ManifestValueAst<'a> {
    String(Span),
    Object(ManifestObjectAst<'a>),
    Array(ManifestListAst<'a>),
}

impl<'a> Ast<'a> for ManifestValueAst<'a> {
    type Args = ();

    const NAME: &'static str = "manifest value";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Result<Self, ()> {
        branch! {ctx => {
            String => Ok(ManifestValueAst::String(ctx.advance().span)),
            LeftBracket => ctx.parse().map(ManifestValueAst::Array),
            LeftCurly => ctx.parse().map(ManifestValueAst::Object),
        }}
    }

    fn span(&self) -> Span {
        todo!()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ManifestDepAst {
    pub git: bool,
    pub name: NameAst,
    pub path: Span,
    pub version: Maybe<Span>,
    pub span: Span,
}

impl<'a> Ast<'a> for ManifestDepAst {
    type Args = ();

    const NAME: &'static str = "manifest dependency";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Result<Self, ()> {
        let start = ctx.state.current.span;
        let git = ctx.optional_advance("git").is_some();
        let name = ctx.parse().ok();
        let path = ctx.expect_advance(TokenKind::String)?.span;
        let version = ctx.optional_advance(TokenKind::String).map(|t| t.span);
        let span = start.joined(version.unwrap_or(path));
        let path = path.shrink(1);
        let version = version.map(|v| v.shrink(1)).into();
        let name = name.unwrap_or_else(|| NameAst::from_path(ctx, path));

        Ok(Self {
            git,
            name,
            path,
            version,
            span,
        })
    }

    fn span(&self) -> Span {
        self.span
    }
}

// impl Parser<'_> {
//     pub fn parse_manifest(&mut self) -> VSlice<Ast> {
//         self.parse_with(Self::take_manifest).0
//     }

//     fn take_manifest(&mut self) -> errors::Result {
//         list!(self, none, NewLine, Eof, manifest_field).map(|_| ())
//     }

//     fn manifest_field(&mut self) -> errors::Result {
//         let start = self.start();
//         let name = self.state.current.span;
//         self.expect(TokenKind::Ident)?;
//         self.capture(AstKind::Ident);
//         branch! { self => {
//             Colon => {
//                 self.advance();
//                 self.expect(TokenKind::String)?;
//                 self.capture(AstKind::String);
//                 self.finish_last(AstKind::ManifestField, start);
//             },
//             LeftCurly => {
//                 let parser = match self.lexer.inner_span_str(name) {
//                     "deps" => Self::dep,
//                     _ => Self::manifest_field,
//                 };
//                 self.start();
//                 let span = list!(self, LeftCurly, NewLine, RightCurly, exp parser)?;
//                 self.finish_last(AstKind::ManifestSection, span);

//                 self.finish_last(AstKind::ManifestImports, start);
//             },
//         }}
//         Ok(())
//     }

//     fn dep(&mut self) -> errors::Result {
//         let start = self.start();
//         self.optional(TokenKind::Ident, |s| Ok(s.capture(AstKind::Ident)))?;
//         let use_git = self.ctx_keyword("git");
//         self.expect(TokenKind::String)?;
//         self.capture(AstKind::String);

//         if self.state.current.kind == TokenKind::String {
//             self.capture(AstKind::String);
//         } else {
//             self.ast_data.cache(Ast::none());
//         }

//         self.finish_last(AstKind::ManifestImport { use_git }, start);
//         Ok(())
//     }
// }
