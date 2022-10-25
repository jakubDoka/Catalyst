use super::*;

list_meta!(DepsMeta LeftCurly NewLine RightCurly);
pub type ManifestDepsAst<'a> = ListAst<'a, ManifestDepAst, DepsMeta>;

list_meta!(ManifestObjectMeta LeftCurly NewLine RightCurly);
pub type ManifestObjectAst<'a> = ListAst<'a, ManifestFieldAst<'a>, ManifestObjectMeta>;

list_meta!(ManifestListMeta LeftBracket Comma RightBracket);
pub type ManifestListAst<'a> = ListAst<'a, ManifestValueAst<'a>, ManifestListMeta>;

#[derive(Clone, Copy, Debug)]
pub struct ManifestAst<'a> {
    pub fields: &'a [ManifestFieldAst<'a>],
    pub deps_span: Option<Span>,
    pub deps: ManifestDepsAst<'a>,
}

impl ManifestAst<'_> {
    pub fn find_field(&self, name: VRef<str>) -> Option<ManifestFieldAst> {
        self.fields
            .iter()
            .find(|field| field.name.ident == name)
            .copied()
    }
}

impl<'a> Ast<'a> for ManifestAst<'a> {
    type Args = ();

    const NAME: &'static str = "manifest";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
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

        Some(Self {
            fields: ctx.arena.alloc_slice(&fields),
            deps_span,
            deps: deps.unwrap_or_default(),
        })
    }

    fn span(&self) -> Span {
        Span::default()
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

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        Some(ManifestFieldAst {
            name: ctx.parse()?,
            value: {
                ctx.expect_advance(TokenKind::Colon)?;
                ManifestValueAst::parse(ctx)?
            },
        })
    }

    fn span(&self) -> Span {
        self.name.span().joined(self.value.span())
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

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        branch! {ctx => {
            String => Some(ManifestValueAst::String(ctx.advance().span)),
            LeftBracket => ctx.parse().map(ManifestValueAst::Array),
            LeftCurly => ctx.parse().map(ManifestValueAst::Object),
        }}
    }

    fn span(&self) -> Span {
        match self {
            ManifestValueAst::String(s) => *s,
            ManifestValueAst::Object(o) => o.span(),
            ManifestValueAst::Array(a) => a.span(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ManifestDepAst {
    pub git: bool,
    pub name: NameAst,
    pub path: Span,
    pub version: Option<Span>,
    pub span: Span,
}

impl<'a> Ast<'a> for ManifestDepAst {
    type Args = ();

    const NAME: &'static str = "manifest dependency";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        let start = ctx.state.current.span;
        let git = ctx.optional_advance("git").is_some();
        let name = ctx.parse_args((true,));
        let path = ctx.expect_advance(TokenKind::String)?.span;
        let version = ctx.optional_advance(TokenKind::String).map(|t| t.span);
        let span = start.joined(version.unwrap_or(path));
        let path = path.shrink(1);
        let version = version.map(|v| v.shrink(1));
        let name = name.unwrap_or_else(|| NameAst::from_path(ctx, path));

        Some(Self {
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
