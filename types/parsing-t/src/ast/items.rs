use super::*;

#[derive(Clone, Copy, Debug)]
pub struct ImportsAst<'a, M = NoTokenMeta> {
    pub keyword: SourceInfo<M>,
    pub items: ListAst<'a, ImportAst<M>, M>,
}

impl<'a, M> Spanned for ImportsAst<'a, M> {
    fn span(&self) -> Span {
        self.keyword.span.joined(self.items.span())
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ImportAst<M> {
    pub vis: Option<VisAst<M>>,
    pub name: Option<NameAst<M>>,
    pub path: SourceInfo<M>,
}

impl<M> ImportAst<M> {
    pub fn span(&self) -> Span {
        let vis = self.vis.as_ref().map(|vis| vis.source_meta.span);
        let name = self.name.as_ref().map(|name| name.span());
        let path = self.path.span;
        let start = vis.or(name).unwrap_or(path);
        start.joined(path)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct StructAst<'a, M = NoTokenMeta> {
    pub vis: Option<VisAst<M>>,
    pub keyword: SourceInfo<M>,
    pub generics: Option<ListAst<'a, ParamAst<'a, M>, M>>,
    pub name: NameAst<M>,
    pub body: Option<ListAst<'a, StructFieldAst<'a, M>, M>>,
}

impl<'a, M> Spanned for StructAst<'a, M> {
    fn span(&self) -> Span {
        let vis = self.vis.as_ref().map(|vis| vis.source_meta.span);
        let r#struct = self.keyword.span;
        let name = self.name.span();
        let body = self.body.as_ref().map(|body| body.span());
        let start = vis.unwrap_or(r#struct);
        let end = body.unwrap_or(name);
        start.joined(end)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct StructFieldAst<'a, M = NoTokenMeta> {
    pub vis: Option<VisAst<M>>,
    pub used: Option<SourceInfo<M>>,
    pub mutable: Option<SourceInfo<M>>,
    pub name: NameAst<M>,
    pub colon: SourceInfo<M>,
    pub ty: TyAst<'a, M>,
}

impl<'a, M> Spanned for StructFieldAst<'a, M> {
    fn span(&self) -> Span {
        let vis = self.vis.as_ref().map(|vis| vis.source_meta.span);
        let used = self.used.as_ref().map(|used| used.span);
        let mutable = self.mutable.as_ref().map(|mutable| mutable.span);
        let name = self.name.span();
        let ty = self.ty.span();
        let start = vis.or(used).or(mutable).unwrap_or(name);
        let end = ty;
        start.joined(end)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct EnumAst<'a, M = NoTokenMeta> {
    pub vis: Option<VisAst<M>>,
    pub keyword: SourceInfo<M>,
    pub generics: Option<ListAst<'a, ParamAst<'a, M>, M>>,
    pub name: NameAst<M>,
    pub body: Option<ListAst<'a, EnumVariantAst<'a, M>, M>>,
}

impl<'a, M> Spanned for EnumAst<'a, M> {
    fn span(&self) -> Span {
        let vis = self.vis.as_ref().map(|vis| vis.source_meta.span);
        let r#enum = self.keyword.span;
        let name = self.name.span();
        let body = self.body.as_ref().map(|body| body.span());
        let start = vis.unwrap_or(r#enum);
        let end = body.unwrap_or(name);
        start.joined(end)
    }
}

pub type GroupedItemSlice<'a, T> = &'a [(T, &'a [TopLevelAttrAst])];

#[derive(Clone, Copy)]
pub struct GroupedItemsAst<'a> {
    pub structs: GroupedItemSlice<'a, StructAst<'a>>,
    pub funcs: GroupedItemSlice<'a, FuncDefAst<'a>>,
    pub specs: GroupedItemSlice<'a, SpecAst<'a>>,
    pub impls: GroupedItemSlice<'a, ImplAst<'a>>,
    pub enums: GroupedItemSlice<'a, EnumAst<'a>>,
    pub last: bool,
}

#[derive(Clone, Copy, Debug)]
pub enum ItemAst<'a, M = NoTokenMeta> {
    Struct(&'a StructAst<'a, M>),
    Func(&'a FuncDefAst<'a, M>),
    Spec(&'a SpecAst<'a, M>),
    Impl(&'a ImplAst<'a, M>),
    Enum(&'a EnumAst<'a, M>),
    Attribute(&'a TopLevelAttrAst<M>),
}

impl<'a, M> ItemAst<'a, M> {
    pub fn span(&self) -> Span {
        match self {
            ItemAst::Struct(item) => item.span(),
            ItemAst::Func(item) => item.span(),
            ItemAst::Spec(item) => item.span(),
            ItemAst::Impl(item) => item.span(),
            ItemAst::Enum(item) => item.span(),
            ItemAst::Attribute(item) => item.span(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct EnumVariantAst<'a, M = NoTokenMeta> {
    pub name: NameAst<M>,
    pub ty: Option<(SourceInfo<M>, TyAst<'a, M>)>,
}

impl<'a, M> Spanned for EnumVariantAst<'a, M> {
    fn span(&self) -> Span {
        let name = self.name.span();
        let ty = self.ty.as_ref().map(|(_, ty)| ty.span());
        let end = ty.unwrap_or(name);
        name.joined(end)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct SpecAst<'a, M = NoTokenMeta> {
    pub vis: Option<VisAst<M>>,
    pub keyword: SourceInfo<M>,
    pub generics: Option<ListAst<'a, ParamAst<'a, M>, M>>,
    pub name: NameAst<M>,
    pub inherits: Option<ParamSpecsAst<'a, M>>,
    pub body: Option<ListAst<'a, FuncSigAst<'a, M>, M>>,
}

impl<'a, M> Spanned for SpecAst<'a, M> {
    fn span(&self) -> Span {
        let vis = self.vis.as_ref().map(|vis| vis.source_meta.span);
        let spec = self.keyword.span;
        let name = self.name.span();
        let body = self.body.as_ref().map(|body| body.span());
        let start = vis.unwrap_or(spec);
        let end = body.unwrap_or(name);
        start.joined(end)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ImplAst<'a, M = NoTokenMeta> {
    pub vis: Option<VisAst<M>>,
    pub keyword: SourceInfo<M>,
    pub generics: Option<ListAst<'a, ParamAst<'a, M>, M>>,
    pub target: ImplTargetAst<'a, M>,
    pub body: Option<ListAst<'a, ImplItemAst<'a, M>, M>>,
}

impl<'a, M> Spanned for ImplAst<'a, M> {
    fn span(&self) -> Span {
        let vis = self.vis.as_ref().map(|vis| vis.source_meta.span);
        let r#impl = self.keyword.span;
        let target = self.target.span();
        let body = self.body.as_ref().map(|body| body.span());
        let start = vis.unwrap_or(r#impl);
        let end = body.unwrap_or(target);
        start.joined(end)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ImplTargetAst<'a, M = NoTokenMeta> {
    Direct(TyAst<'a, M>),
    Spec(SpecExprAst<'a, M>, SourceInfo<M>, TyAst<'a, M>),
}

impl<'a, M> Spanned for ImplTargetAst<'a, M> {
    fn span(&self) -> Span {
        match self {
            ImplTargetAst::Direct(ty) => ty.span(),
            ImplTargetAst::Spec(spec, _, ty) => spec.span().joined(ty.span()),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ImplItemAst<'a, M = NoTokenMeta> {
    Func(&'a FuncDefAst<'a, M>),
}

impl<'a, M> Spanned for ImplItemAst<'a, M> {
    fn span(&self) -> Span {
        match self {
            ImplItemAst::Func(func) => func.span(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum InlineModeAst<M> {
    Always(SourceInfo<M>),
    Never(SourceInfo<M>),
}

impl<M> Spanned for InlineModeAst<M> {
    fn span(&self) -> Span {
        match self {
            InlineModeAst::Always(info) => info.span,
            InlineModeAst::Never(info) => info.span,
        }
    }
}

#[derive(Clone, Copy, Debug)]
// #[repr(C, u8)]
pub enum TopLevelAttrKindAst<M> {
    Entry(SourceInfo<M>),
    WaterDrop(SourceInfo<M>),
    CompileTime(SourceInfo<M>),
    NoMoves(SourceInfo<M>),
    Macro(SourceInfo<M>, NameAst<M>),
    Inline(Option<WrappedAst<InlineModeAst<M>, M>>),
}

impl<M> Spanned for TopLevelAttrKindAst<M> {
    fn span(&self) -> Span {
        match self {
            TopLevelAttrKindAst::Entry(info) => info.span,
            TopLevelAttrKindAst::WaterDrop(info) => info.span,
            TopLevelAttrKindAst::CompileTime(info) => info.span,
            TopLevelAttrKindAst::NoMoves(info) => info.span,
            TopLevelAttrKindAst::Macro(info, _) => info.span,
            TopLevelAttrKindAst::Inline(mode) => mode
                .as_ref()
                .map(|mode| mode.span())
                .unwrap_or(Span::default()),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct TopLevelAttrAst<M = NoTokenMeta> {
    pub hash: SourceInfo<M>,
    pub value: WrappedAst<TopLevelAttrKindAst<M>, M>,
}

impl<M> Spanned for TopLevelAttrAst<M> {
    fn span(&self) -> Span {
        self.hash.span.joined(self.value.span())
    }
}
