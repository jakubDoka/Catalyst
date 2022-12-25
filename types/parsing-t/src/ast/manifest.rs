use super::*;

#[derive(Clone, Copy, Debug)]
pub struct ManifestAst<'a, M = NoTokenMeta> {
    pub header: Option<SourceInfo<M>>,
    pub items: &'a [(ManifestItemAst<'a, M>, Option<SourceInfo<M>>)],
}

#[derive(Clone, Copy, Debug)]
pub enum ManifestItemAst<'a, M = NoTokenMeta> {
    Field(ManifestFieldAst<'a, M>),
    Deps(ManifestDepsAst<'a, M>),
}

impl<'a, M> Spanned for ManifestItemAst<'a, M> {
    fn span(&self) -> Span {
        match self {
            ManifestItemAst::Field(field) => field.span(),
            ManifestItemAst::Deps(deps) => deps.span(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ManifestDepsAst<'a, M = NoTokenMeta> {
    pub deps: SourceInfo<M>,
    pub list: ListAst<'a, ManifestDepAst<M>, M>,
}

impl<'a, M> Spanned for ManifestDepsAst<'a, M> {
    fn span(&self) -> Span {
        self.deps.span.joined(self.list.span())
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ManifestDepAst<M = NoTokenMeta> {
    pub git: Option<SourceInfo<M>>,
    pub name: Option<NameAst<M>>,
    pub path: SourceInfo<M>,
    pub version: Option<SourceInfo<M>>,
}

impl<M> Spanned for ManifestDepAst<M> {
    fn span(&self) -> Span {
        let git = self.git.as_ref().map(|e| e.span);
        let name = self.name.as_ref().map(|e| e.span);
        let path = self.path.span;
        let version = self.version.as_ref().map(|e| e.span);
        let start = git.or(name).unwrap_or(path);
        let end = version.unwrap_or(path);
        start.joined(end)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ManifestFieldAst<'a, M = NoTokenMeta> {
    pub name: NameAst<M>,
    pub colon: SourceInfo<M>,
    pub value: ManifestValueAst<'a, M>,
}

impl<'a, M> Spanned for ManifestFieldAst<'a, M> {
    fn span(&self) -> Span {
        self.name.span().joined(self.value.span())
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ManifestValueAst<'a, M = NoTokenMeta> {
    String(SourceInfo<M>),
    Object(ListAst<'a, ManifestFieldAst<'a, M>, M>),
    Array(ListAst<'a, ManifestValueAst<'a, M>, M>),
}

impl<'a, M> Spanned for ManifestValueAst<'a, M> {
    fn span(&self) -> Span {
        match self {
            ManifestValueAst::String(s) => s.span,
            ManifestValueAst::Object(o) => o.span(),
            ManifestValueAst::Array(a) => a.span(),
        }
    }
}

impl<M: Copy> ManifestAst<'_, M> {
    pub fn find_field(&self, name: Ident) -> Option<ManifestFieldAst<M>> {
        self.items.iter().find_map(|&(field, ..)| match field {
            ManifestItemAst::Field(field) if field.name.ident == name => Some(field),
            _ => None,
        })
    }

    pub fn find_deps(&self) -> impl Iterator<Item = ManifestDepsAst<M>> {
        self.items.iter().filter_map(|&(field, ..)| match field {
            ManifestItemAst::Deps(deps) => Some(deps),
            _ => None,
        })
    }
}
