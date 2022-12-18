use super::*;

list_meta!(StructCtorPatBodyMeta LeftCurly [Comma NewLine] RightCurly);
pub type StructCtorPatBodyAst<'a> = ListAst<'a, StructCtorPatFieldAst<'a>, StructCtorPatBodyMeta>;

#[derive(Debug, Clone, Copy)]
pub enum PatAst<'a> {
    Binding(Option<Span>, NameAst),
    Wildcard(Span),
    StructCtor(StructCtorPatAst<'a>),
    EnumCtor(&'a EnumCtorPatAst<'a>),
    Int(Span),
}

impl<'a> Ast<'a> for PatAst<'a> {
    type Args = Option<Span>;

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, mutable: Self::Args) -> Option<Self> {
        branch!(ctx => {
            Mut => {
                if let Some(mutable) = mutable {
                    ctx.workspace.push(UselessMut {
                        prev: mutable,
                        loc: ctx.loc(),
                    });
                }

                let mutable = ctx.advance().span;
                ctx.parse_args(Some(mutable))
            },
            Ident => match ctx.current_token_str() {
                "_" => Some(Self::Wildcard(ctx.advance().span)),
                _ => ctx.parse().map(|b| Self::Binding(mutable, b)),
            },
            BackSlash => {
                if ctx.at_next(TokenKind::Ident) {
                    ctx.parse_args_alloc(mutable).map(Self::EnumCtor)
                } else {
                    ctx.parse_args(mutable).map(Self::StructCtor)
                }
            },
            Int => Some(Self::Int(ctx.advance().span)),
            @"pattern",
        })
    }

    fn span(&self) -> Span {
        use PatAst::*;
        match *self {
            Binding(mutable, name) => {
                mutable.map_or(name.span(), |mutable| mutable.joined(name.span()))
            }
            StructCtor(ctor) => ctor.span(),
            Wildcard(span) | Int(span) => span,
            EnumCtor(ctor) => ctor.span(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct EnumCtorPatAst<'a> {
    pub slash: Span,
    pub name: NameAst,
    pub value: Option<(Span, PatAst<'a>)>,
}

impl<'a> Ast<'a> for EnumCtorPatAst<'a> {
    type Args = Option<Span>;

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, mutable: Self::Args) -> Option<Self> {
        Some(Self {
            slash: ctx.advance().span,
            name: ctx.parse()?,

            value: ctx
                .try_advance(TokenKind::Tilde)
                .map(|tilde| ctx.parse_args(mutable).map(|value| (tilde.span, value)))
                .transpose()?,
        })
    }

    fn span(&self) -> Span {
        self.slash.joined(
            self.value
                .map_or(self.name.span(), |(.., body)| body.span()),
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StructCtorPatAst<'a> {
    pub slash: Span,
    pub fields: StructCtorPatBodyAst<'a>,
}

impl<'a> Ast<'a> for StructCtorPatAst<'a> {
    type Args = Option<Span>;

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, mutable: Self::Args) -> Option<Self> {
        Some(Self {
            slash: ctx.advance().span,
            fields: ctx.parse_args(mutable)?,
        })
    }

    fn span(&self) -> Span {
        self.slash.joined(self.fields.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum StructCtorPatFieldAst<'a> {
    Simple {
        mutable: Option<Span>,
        name: NameAst,
    },
    Named {
        name: NameAst,
        colon: Span,
        pat: PatAst<'a>,
    },
    DoubleDot(Span),
}

impl<'a> Ast<'a> for StructCtorPatFieldAst<'a> {
    type Args = Option<Span>;

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, mutable: Self::Args) -> Option<Self> {
        Some(branch! {ctx => {
            Mut => {
                if let Some(mutable) = mutable {
                    ctx.workspace.push(UselessMut {
                        prev: mutable,
                        loc: ctx.loc(),
                    });
                }

                let mutable = ctx.advance().span;
                ctx.parse_args(Some(mutable))?
            },
            Ident => {
                let name = ctx.parse()?;
                branch! {ctx => {
                    Colon => Self::Named { name, colon: ctx.advance().span, pat: ctx.parse_args(mutable)? },
                    @ => Self::Simple { name, mutable },
                }}
            },
            DoubleDot => Self::DoubleDot(ctx.advance().span),
            @"struct field pattern",
        }})
    }

    fn span(&self) -> Span {
        use StructCtorPatFieldAst::*;
        match *self {
            Simple { name, mutable } => {
                mutable.map_or(name.span(), |mutable| mutable.joined(name.span()))
            }
            Named { name, pat, .. } => name.span().joined(pat.span()),
            DoubleDot(span) => span,
        }
    }
}

ctl_errors! {
    #[warn => "useless 'mut' in pattern"]
    #[info => "'mut' is recursive"]
    error UselessMut {
        #[info loc.origin, prev, "this already makes the pattern mutable"]
        #[warn loc]
        prev: Span,
        loc: SourceLoc,
    }
}
