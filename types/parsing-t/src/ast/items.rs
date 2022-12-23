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
pub struct StructAst<'a, M> {
    pub vis: Option<VisAst<M>>,
    pub r#struct: SourceInfo<M>,
    pub generics: Option<ListAst<'a, ParamAst<'a, M>, M>>,
    pub name: NameAst<M>,
    pub body: Option<ListAst<'a, StructFieldAst<'a, M>, M>>,
}

#[derive(Clone, Copy, Debug)]
pub struct StructFieldAst<'a, M> {
    pub vis: Option<VisAst<M>>,
    pub used: Option<SourceInfo<M>>,
    pub mutable: Option<SourceInfo<M>>,
    pub name: NameAst<M>,
    pub colon: SourceInfo<M>,
    pub ty: TyAst<'a, M>,
}

#[derive(Clone, Copy, Debug)]
pub struct EnumAst<'a, M> {
    pub vis: Option<VisAst<M>>,
    pub r#enum: SourceInfo<M>,
    pub generics: Option<ListAst<'a, ParamAst<'a, M>, M>>,
    pub name: NameAst<M>,
    pub body: Option<ListAst<'a, EnumVariantAst<'a, M>, M>>,
}
pub type GroupedItemSlice<'a, T, M> = &'a [(T, &'a [TopLevelAttrAst<M>])];

#[derive(Clone, Copy)]
pub struct GroupedItemsAst<'a, M = NoTokenMeta> {
    pub structs: GroupedItemSlice<'a, StructAst<'a, M>, M>,
    pub funcs: GroupedItemSlice<'a, FuncDefAst<'a, M>, M>,
    pub specs: GroupedItemSlice<'a, SpecAst<'a, M>, M>,
    pub impls: GroupedItemSlice<'a, ImplAst<'a, M>, M>,
    pub enums: GroupedItemSlice<'a, EnumAst<'a, M>, M>,
    pub last: bool,
}

#[derive(Clone, Copy, Debug)]
pub enum ItemAst<'a, M> {
    Struct(&'a StructAst<'a, M>),
    Func(&'a FuncDefAst<'a, M>),
    Spec(&'a SpecAst<'a, M>),
    Impl(&'a ImplAst<'a, M>),
    Enum(&'a EnumAst<'a, M>),
    Attribute(&'a TopLevelAttrAst<M>),
}

#[derive(Clone, Copy, Debug)]
pub struct EnumVariantAst<'a, M> {
    pub name: NameAst<M>,
    pub ty: Option<(SourceInfo<M>, TyAst<'a, M>)>,
}

#[derive(Clone, Copy, Debug)]
pub struct SpecAst<'a, M> {
    pub vis: Option<VisAst<M>>,
    pub spec: SourceInfo<M>,
    pub generics: Option<ListAst<'a, ParamAst<'a, M>, M>>,
    pub name: NameAst<M>,
    pub inherits: Option<ParamSpecsAst<'a, M>>,
    pub body: Option<ListAst<'a, FuncSigAst<'a, M>, M>>,
}

#[derive(Clone, Copy, Debug)]
pub struct ImplAst<'a, M> {
    pub vis: Option<VisAst<M>>,
    pub r#impl: SourceInfo<M>,
    pub generics: Option<ListAst<'a, ParamAst<'a, M>, M>>,
    pub target: ImplTargetAst<'a, M>,
    pub body: Option<ListAst<'a, ImplItemAst<'a, M>, M>>,
}

#[derive(Clone, Copy, Debug)]
pub enum ImplTargetAst<'a, M> {
    Direct(TyAst<'a, M>),
    Spec(SpecExprAst<'a, M>, SourceInfo<M>, TyAst<'a, M>),
}

#[derive(Clone, Copy, Debug)]
pub enum ImplItemAst<'a, M> {
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
pub struct TopLevelAttrAst<M> {
    pub hash: SourceInfo<M>,
    pub value: WrappedAst<TopLevelAttrKindAst<M>, M>,
}

impl<M> TopLevelAttrAst<M> {
    pub fn span(&self) -> Span {
        self.hash.span.joined(self.value.span())
    }
}
