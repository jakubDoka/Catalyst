use super::*;

#[derive(Clone, Copy, Debug)]
pub struct ImportsAst<'a, M = NoTokenMeta> {
    pub r#use: SourceInfo<M>,
    pub items: ListAst<'a, ImportAst<M>, M>,
}

#[derive(Clone, Copy, Debug)]
pub struct ImportAst<M> {
    pub vis: Option<VisAst<M>>,
    pub name: Option<NameAst<M>>,
    pub path: SourceInfo<M>,
}

impl<M> ImportAst<M> {
    pub fn span(&self) -> Span {
        None.or_else(|| self.vis.as_ref().map(|vis| vis.source_meta.span))
            .or_else(|| self.name.as_ref().map(|name| name.source_info.span))
            .map_or(self.path.span, |span| span.joined(self.path.span))
    }
}

#[derive(Clone, Copy, Debug)]
pub struct StructAst<'a, M = NoTokenMeta> {
    pub vis: Option<VisAst<M>>,
    pub r#struct: SourceInfo<M>,
    pub generics: Option<ListAst<'a, ParamAst<'a, M>, M>>,
    pub name: NameAst<M>,
    pub body: Option<ListAst<'a, StructFieldAst<'a, M>, M>>,
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

#[derive(Clone, Copy, Debug)]
pub struct EnumAst<'a, M = NoTokenMeta> {
    pub vis: Option<VisAst<M>>,
    pub r#enum: SourceInfo<M>,
    pub generics: Option<ListAst<'a, ParamAst<'a, M>, M>>,
    pub name: NameAst<M>,
    pub body: Option<ListAst<'a, EnumVariantAst<'a, M>, M>>,
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

#[derive(Clone, Copy, Debug)]
pub struct EnumVariantAst<'a, M = NoTokenMeta> {
    pub name: NameAst<M>,
    pub ty: Option<(SourceInfo<M>, TyAst<'a, M>)>,
}

#[derive(Clone, Copy, Debug)]
pub struct SpecAst<'a, M = NoTokenMeta> {
    pub vis: Option<VisAst<M>>,
    pub spec: SourceInfo<M>,
    pub generics: Option<ListAst<'a, ParamAst<'a, M>, M>>,
    pub name: NameAst<M>,
    pub inherits: Option<ParamSpecsAst<'a, M>>,
    pub body: Option<ListAst<'a, FuncSigAst<'a, M>, M>>,
}

#[derive(Clone, Copy, Debug)]
pub struct ImplAst<'a, M = NoTokenMeta> {
    pub vis: Option<VisAst<M>>,
    pub r#impl: SourceInfo<M>,
    pub generics: Option<ListAst<'a, ParamAst<'a, M>, M>>,
    pub target: ImplTargetAst<'a, M>,
    pub body: Option<ListAst<'a, ImplItemAst<'a, M>, M>>,
}

#[derive(Clone, Copy, Debug)]
pub enum ImplTargetAst<'a, M = NoTokenMeta> {
    Direct(TyAst<'a, M>),
    Spec(SpecExprAst<'a, M>, SourceInfo<M>, TyAst<'a, M>),
}

impl<'a, M> ImplTargetAst<'a, M> {
    pub fn span(&self) -> Span {
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

#[derive(Clone, Copy, Debug)]
pub enum InlineModeAst<M> {
    Always(SourceInfo<M>),
    Never(SourceInfo<M>),
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

#[derive(Clone, Copy, Debug)]
pub struct TopLevelAttrAst<M = NoTokenMeta> {
    pub hash: SourceInfo<M>,
    pub value: WrappedAst<TopLevelAttrKindAst<M>, M>,
}

impl<M> TopLevelAttrAst<M> {
    pub fn span(&self) -> Span {
        self.hash.span.joined(self.value.span())
    }
}
