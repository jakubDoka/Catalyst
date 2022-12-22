use super::*;

#[derive(Clone, Copy, Debug)]
pub struct ManifestAst<'a, M> {
    pub fields: &'a [ManifestFieldAst<'a, M>],
    pub deps: Option<DepsAst<'a, M>>,
}

#[derive(Clone, Copy, Debug)]
pub struct DepsAst<'a, M> {
    pub deps: SourceMeta<M>,
    pub list: ListAst<'a, ManifestDepAst<M>, M>,
}

#[derive(Clone, Copy, Debug)]
pub struct ManifestDepAst<M> {
    pub git: Option<SourceMeta<M>>,
    pub name: Option<NameAst<M>>,
    pub path: SourceMeta<M>,
    pub version: Option<SourceMeta<M>>,
}

#[derive(Clone, Copy, Debug)]
pub struct ManifestFieldAst<'a, M> {
    pub name: NameAst<M>,
    pub colon: SourceMeta<M>,
    pub value: ManifestValueAst<'a, M>,
}

#[derive(Clone, Copy, Debug)]
pub enum ManifestValueAst<'a, M> {
    String(SourceMeta<M>),
    Object(ListAst<'a, ManifestFieldAst<'a, M>, M>),
    Array(ListAst<'a, ManifestValueAst<'a, M>, M>),
}

impl<M: Copy> ManifestAst<'_, M> {
    pub fn find_field(&self, name: Ident) -> Option<ManifestFieldAst<M>> {
        self.fields
            .iter()
            .find(|field| field.name.ident == name)
            .copied()
    }
}
