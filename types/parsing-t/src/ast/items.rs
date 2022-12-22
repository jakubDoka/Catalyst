use super::*;

#[derive(Clone, Copy, Debug)]
pub struct ImportsAst<'a, M> {
    pub r#use: SourceMeta<M>,
    pub items: ListAst<'a, ImportAst<M>, M>,
}

#[derive(Clone, Copy, Debug)]
pub struct ImportAst<M> {
    pub vis: Option<VisAst<M>>,
    pub name: Option<NameAst<M>>,
    pub path: SourceMeta<M>,
}

#[derive(Clone, Copy, Debug)]
pub struct StructAst<'a, M> {
    pub vis: Option<VisAst<M>>,
    pub r#struct: SourceMeta<M>,
    pub generics: Option<ListAst<'a, ParamAst<'a, M>, M>>,
    pub name: NameAst<M>,
    pub body: Option<ListAst<'a, StructFieldAst<'a, M>, M>>,
}

#[derive(Clone, Copy, Debug)]
pub struct StructFieldAst<'a, M> {
    pub vis: Option<VisAst<M>>,
    pub used: Option<SourceMeta<M>>,
    pub mutable: Option<SourceMeta<M>>,
    pub name: NameAst<M>,
    pub colon: SourceMeta<M>,
    pub ty: TyAst<'a, M>,
}
