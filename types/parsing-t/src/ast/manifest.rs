use super::*;

#[derive(Clone, Copy, Debug)]
pub struct ManifestAst<'a, M = NoTokenMeta> {
    pub fields: &'a [ManifestFieldAst<'a, M>],
    pub deps: Option<ManifestDepsAst<'a, M>>,
}

impl<'a, M> Spanned for ManifestAst<'a, M> {
    fn span(&self) -> Span {
        let fields = self.fields.iter().map(|e| e.span()).reduce(Span::joined);
        let deps = self.deps.as_ref().map(|e| e.span());
        match (fields, deps) {
            (Some(f), Some(d)) => f.joined(d),
            (Some(f), None) => f,
            (None, Some(d)) => d,
            (None, None) => Span::default(),
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
pub struct ManifestDepAst<M> {
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
        self.fields
            .iter()
            .find(|field| field.name.ident == name)
            .copied()
    }
}
