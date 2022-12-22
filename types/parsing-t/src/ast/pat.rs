use super::*;

#[derive(Debug, Clone, Copy)]
pub enum StructCtorPatFieldAst<'a, M> {
    Simple {
        mutable: Option<SourceMeta<M>>,
        name: NameAst<M>,
    },
    Named {
        name: NameAst<M>,
        colon: SourceMeta<M>,
        pat: PatAst<'a, M>,
    },
    DoubleDot(SourceMeta<M>),
}

#[derive(Debug, Clone, Copy)]
pub struct StructCtorPatAst<'a, M> {
    pub slash: SourceMeta<M>,
    pub fields: ListAst<'a, StructCtorPatFieldAst<'a, M>, M>,
}

#[derive(Debug, Clone, Copy)]
pub struct EnumCtorPatAst<'a, M> {
    pub slash: SourceMeta<M>,
    pub name: NameAst<M>,
    pub value: Option<(SourceMeta<M>, PatAst<'a, M>)>,
}

#[derive(Debug, Clone, Copy)]
pub enum PatAst<'a, M> {
    Binding(Option<SourceMeta<M>>, NameAst<M>),
    Wildcard(SourceMeta<M>),
    StructCtor(StructCtorPatAst<'a, M>),
    EnumCtor(&'a EnumCtorPatAst<'a, M>),
    Int(SourceMeta<M>),
}
