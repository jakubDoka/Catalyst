use diags::{ctl_errors, SourceLoc};

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
    pub fn find_field(&self, name: Ident) -> Option<ManifestFieldAst> {
        self.fields
            .iter()
            .find(|field| field.name.ident == name)
            .copied()
    }
}

impl<'a> Ast<'a> for ManifestAst<'a> {
    type Args = ();

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        let mut fields = bumpvec![];
        let mut deps = None;
        let mut deps_span = None;
        loop {
            ctx.skip(TokenKind::NewLine);

            if ctx.at(TokenKind::Eof) {
                break;
            }

            if let Some(token) = ctx.try_advance("deps") && deps.is_none() {
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

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        Some(ManifestFieldAst {
            name: ctx.parse()?,
            value: {
                ctx.expect_advance(TokenKind::Colon, |ctx| MissingManifestFieldColon {
                    got: ctx.state.current.kind,
                    loc: ctx.loc(),
                })?;
                ManifestValueAst::parse(ctx)?
            },
        })
    }

    fn span(&self) -> Span {
        self.name.span().joined(self.value.span())
    }
}

ctl_errors! {
    #[err => "expected ':' but got {got} when parsing manifest field"]
    #[info => "colon is required for readability"]
    fatal struct MissingManifestFieldColon {
        #[err loc]
        got: TokenKind,
        loc: SourceLoc,
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

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        branch! {ctx => {
            String => Some(ManifestValueAst::String(ctx.advance().span)),
            LeftBracket => ctx.parse().map(ManifestValueAst::Array),
            LeftCurly => ctx.parse().map(ManifestValueAst::Object),
            @options => ctx.workspace.push(ExpectedManifestValue {
                expected: options.to_str(ctx),
                got: ctx.state.current.kind,
                loc: ctx.loc(),
            })?,
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

ctl_errors! {
    #[err => "expected {expected} but got {got} when parsing manifest value"]
    #[info => "manifest values are strings, objects, or arrays"]
    fatal struct ExpectedManifestValue {
        #[err loc]
        expected ref: String,
        got: TokenKind,
        loc: SourceLoc,
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

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        let start = ctx.state.current.span;
        let git = ctx.try_advance("git").is_some();
        let name = ctx.parse_args((true, "dependency name"));
        let path = ctx
            .expect_advance(TokenKind::String, |ctx| ExpectedDepStringPath {
                got: ctx.state.current.kind,
                loc: ctx.loc(),
            })?
            .span;
        let version = ctx.try_advance(TokenKind::String).map(|t| t.span);
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

ctl_errors! {
    #[err => "expected string path for dependency but got {got}"]
    fatal struct ExpectedDepStringPath {
        #[err loc]
        got: TokenKind,
        loc: SourceLoc,
    }
}
