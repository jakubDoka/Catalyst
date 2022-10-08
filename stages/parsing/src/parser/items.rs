use super::*;

list_meta!(ItemsMeta none NewLine [Break Eof]);
pub type ItemsAst<'a> = ListAst<'a, ItemAst<'a>, ItemsMeta>;
pub type GroupedItemSlice<'a, T> = &'a [(T, &'a [TopLevelAttributeAst])];
list_meta!(SpecBodyMeta ?LeftCurly NewLine RightCurly);
pub type SpecBodyAst<'a> = ListAst<'a, FuncSigAst<'a>, SpecBodyMeta>;
list_meta!(ImplBodyMeta ?LeftCurly NewLine RightCurly);
pub type ImplBodyAst<'a> = ListAst<'a, ImplItemAst<'a>, SpecBodyMeta>;

#[derive(Clone, Copy)]
pub struct GroupedItemsAst<'a> {
    pub structs: GroupedItemSlice<'a, StructAst<'a>>,
    pub funcs: GroupedItemSlice<'a, FuncDefAst<'a>>,
    pub specs: GroupedItemSlice<'a, SpecAst<'a>>,
    pub impls: GroupedItemSlice<'a, ImplAst<'a>>,
    pub last: bool,
    pub span: Span,
}

impl<'a> Ast<'a> for GroupedItemsAst<'a> {
    type Args = ();

    const NAME: &'static str = "grouped items";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
        let items = ctx.parse::<ItemsAst>()?;

        let last = items.end.is_empty();
        let span = items.span();

        macro_rules! gen_groups {
            (
                $(
                    $name:ident: $enum_name:ident,
                )*
            ) => {
                let mut attrs = bumpvec![];
                $(
                    let mut $name = bumpvec![];
                )*

                for &item in items.iter() {
                    match item {
                        $(
                            ItemAst::$enum_name(&s) => $name.push((s, ctx.arena.alloc_iter(attrs.drain(..)))),
                        )*
                        ItemAst::Attribute(&a) => attrs.push(a),
                    }
                }

                $(
                    let $name = ctx.arena.alloc_slice($name.as_slice());
                )*

                Some(Self {
                    $(
                        $name,
                    )*
                    last,
                    span,
                })
            };
        }

        gen_groups! {
            structs: Struct,
            funcs: Func,
            specs: Spec,
            impls: Impl,
        }
    }

    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ItemAst<'a> {
    Struct(&'a StructAst<'a>),
    Func(&'a FuncDefAst<'a>),
    Spec(&'a SpecAst<'a>),
    Impl(&'a ImplAst<'a>),
    Attribute(&'a TopLevelAttributeAst),
}

impl<'a> Ast<'a> for ItemAst<'a> {
    type Args = ();

    const NAME: &'static str = "item";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
        let start = ctx.state.current.span;
        let vis = ctx.visibility();
        branch! { ctx => {
            Struct => ctx.parse_args((vis, start))
                .map(|s| ctx.arena.alloc(s))
                .map(ItemAst::Struct),
            Func => ctx.parse_args((vis, start))
                .map(|s| ctx.arena.alloc(s))
                .map(ItemAst::Func),
            Spec => ctx.parse_args((vis, start))
                .map(|s| ctx.arena.alloc(s))
                .map(ItemAst::Spec),
            Impl => ctx.parse_args((vis, start))
                .map(|s| ctx.arena.alloc(s))
                .map(ItemAst::Impl),
            Hash => ctx.parse()
                .map(|s| ctx.arena.alloc(s))
                .map(ItemAst::Attribute),
        }}
    }

    fn span(&self) -> Span {
        match self {
            ItemAst::Struct(s) => s.span(),
            ItemAst::Func(f) => f.span(),
            ItemAst::Spec(s) => s.span(),
            ItemAst::Impl(i) => i.span(),
            ItemAst::Attribute(a) => a.span(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ImplAst<'a> {
    pub start: Span,
    pub vis: Vis,
    pub r#impl: Span,
    pub generics: GenericsAst<'a>,
    pub target: ImplTarget<'a>,
    pub body: ImplBodyAst<'a>,
}

impl<'a> Ast<'a> for ImplAst<'a> {
    type Args = (Vis, Span);

    const NAME: &'static str = "impl";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (vis, start): Self::Args) -> Option<Self> {
        Some(Self {
            start,
            vis,
            r#impl: ctx.advance().span,
            generics: ctx.parse()?,
            target: ctx.parse()?,
            body: ctx.parse()?,
        })
    }

    fn span(&self) -> Span {
        self.start.joined(self.body.span())
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ImplTarget<'a> {
    Direct(TyAst<'a>),
    Spec(SpecExprAst<'a>, Span, TyAst<'a>),
}

impl<'a> Ast<'a> for ImplTarget<'a> {
    type Args = ();

    const NAME: &'static str = "impl target";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
        ctx.skip(TokenKind::NewLine);
        let ty = ctx.parse()?;
        if let Some(tok) = ctx.try_advance(TokenKind::For) {
            Some(Self::Spec(
                match ty {
                    TyAst::Path(path) => SpecExprAst::Path(path),
                    TyAst::Instance(instance) => SpecExprAst::Instance(instance),
                    _ => ctx.invalid_spec_syntax(ty.span())?,
                },
                tok.span,
                ctx.parse()?,
            ))
        } else {
            Some(Self::Direct(ty))
        }
    }

    fn span(&self) -> Span {
        match self {
            ImplTarget::Direct(ty) => ty.span(),
            ImplTarget::Spec(ty, .., spec) => ty.span().joined(spec.span()),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ImplItemAst<'a> {
    Func(&'a FuncDefAst<'a>),
}

impl<'a> Ast<'a> for ImplItemAst<'a> {
    type Args = ();

    const NAME: &'static str = "impl item";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
        let start = ctx.state.current.span;
        let vis = ctx.visibility();
        branch! { ctx => {
            Func => ctx.parse_args((vis, start))
                .map(|s| ctx.arena.alloc(s))
                .map(ImplItemAst::Func),
        }}
    }

    fn span(&self) -> Span {
        match self {
            ImplItemAst::Func(f) => f.span(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct SpecAst<'a> {
    pub start: Span,
    pub vis: Vis,
    pub spec: Span,
    pub generics: GenericsAst<'a>,
    pub name: NameAst,
    pub body: SpecBodyAst<'a>,
}

impl<'a> Ast<'a> for SpecAst<'a> {
    type Args = (Vis, Span);

    const NAME: &'static str = "spec";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (vis, start): Self::Args) -> Option<Self> {
        Some(Self {
            start,
            vis,
            spec: ctx.advance().span,
            generics: ctx.parse()?,
            name: ctx.parse()?,
            body: ctx.parse()?,
        })
    }

    fn span(&self) -> Span {
        self.start.joined(self.body.span())
    }
}

#[derive(Clone, Copy, Debug)]
pub struct TopLevelAttributeAst {
    pub hash: Span,
    pub value: WrappedAst<TopLevelAttributeKindAst>,
}

impl<'a> Ast<'a> for TopLevelAttributeAst {
    type Args = ();

    const NAME: &'static str = "top level attribute";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
        Some(Self {
            hash: ctx.advance().span,
            value: ctx.parse_args((
                TokenKind::LeftBracket.into(),
                TokenKind::RightBracket.into(),
            ))?,
        })
    }

    fn span(&self) -> Span {
        self.hash.joined(self.value.span())
    }
}

#[derive(Clone, Copy, Debug)]
pub enum TopLevelAttributeKindAst {
    Entry(Span),
    Inline(Option<WrappedAst<InlineModeAst>>),
}

impl<'a> Ast<'a> for TopLevelAttributeKindAst {
    type Args = ();

    const NAME: &'static str = "top level attribute";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
        branch! {str ctx => {
            "entry" => Some(TopLevelAttributeKindAst::Entry(ctx.advance().span)),
            "inline" => {
                if ctx.at_tok(TokenKind::LeftParen) {
                    Some(TopLevelAttributeKindAst::Inline(None))
                } else {
                    ctx.parse_args((TokenKind::LeftParen.into(), TokenKind::RightParen.into()))
                        .map(Some)
                        .map(TopLevelAttributeKindAst::Inline)
                }
            },
        }}
    }

    fn span(&self) -> Span {
        match *self {
            TopLevelAttributeKindAst::Entry(span) => span,
            TopLevelAttributeKindAst::Inline(mode) => mode.map_or(Span::default(), |m| m.span()),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum InlineModeAst {
    Always(Span),
    Never(Span),
}

impl<'a> Ast<'a> for InlineModeAst {
    type Args = ();

    const NAME: &'static str = "inline mode";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
        branch! {str ctx => {
            "always" => Some(InlineModeAst::Always(ctx.advance().span)),
            "never" => Some(InlineModeAst::Never(ctx.advance().span)),
        }}
    }

    fn span(&self) -> Span {
        match *self {
            InlineModeAst::Always(span) => span,
            InlineModeAst::Never(span) => span,
        }
    }
}
