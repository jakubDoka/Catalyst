use super::*;

#[derive(Clone, Copy, Debug)]
pub struct ManifestAst<'a, M = NoTokenMeta> {
    pub fields: &'a [ManifestFieldAst<'a, M>],
    pub deps: Option<ManifestDepsAst<'a, M>>,
}

#[derive(Clone, Copy, Debug)]
pub struct ManifestDepsAst<'a, M = NoTokenMeta> {
    pub deps: SourceInfo<M>,
    pub list: ListAst<'a, ManifestDepAst<M>, M>,
}

#[derive(Clone, Copy, Debug)]
pub struct ManifestDepAst<M> {
    pub git: Option<SourceInfo<M>>,
    pub name: Option<NameAst<M>>,
    pub path: SourceInfo<M>,
    pub version: Option<SourceInfo<M>>,
}

#[derive(Clone, Copy, Debug)]
pub struct ManifestFieldAst<'a, M> {
    pub name: NameAst<M>,
    pub colon: SourceInfo<M>,
    pub value: ManifestValueAst<'a, M>,
}

#[derive(Clone, Copy, Debug)]
pub enum ManifestValueAst<'a, M> {
    String(SourceInfo<M>),
    Object(ListAst<'a, ManifestFieldAst<'a, M>, M>),
    Array(ListAst<'a, ManifestValueAst<'a, M>, M>),
}

impl<'a, M> ManifestValueAst<'a, M> {
    pub fn span(&self) -> Span {
        match self {
            ManifestValueAst::String(s) => s.span,
            ManifestValueAst::Object(o) => o.span(),
            ManifestValueAst::Array(a) => a.span(),
        }
    }
}

impl<M: Copy> ManifestAst<'_, M> {
    pub fn find_field(&self, name: Ident) -> Option<ManifestFieldAst<M>> {
        self.fields
            .iter()
            .find(|field| field.name.ident == name)
            .copied()
    }
}
