use super::*;

#[derive(Debug, Clone, Copy)]
pub enum StructCtorPatFieldAst<'a, M = NoTokenMeta> {
    Simple {
        mutable: Option<SourceInfo<M>>,
        name: NameAst<M>,
    },
    Named {
        name: NameAst<M>,
        colon: SourceInfo<M>,
        pat: PatAst<'a, M>,
    },
    DoubleDot(SourceInfo<M>),
}

impl<'a, M> Spanned for StructCtorPatFieldAst<'a, M> {
    fn span(&self) -> Span {
        match self {
            StructCtorPatFieldAst::Simple { mutable, name } => mutable
                .as_ref()
                .map_or(name.span, |e| e.span.joined(name.span)),
            StructCtorPatFieldAst::Named { name, pat, .. } => name.span.joined(pat.span()),
            StructCtorPatFieldAst::DoubleDot(e) => e.span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StructCtorPatAst<'a, M = NoTokenMeta> {
    pub slash: SourceInfo<M>,
    pub fields: ListAst<'a, StructCtorPatFieldAst<'a, M>, M>,
}

impl<'a, M> Spanned for StructCtorPatAst<'a, M> {
    fn span(&self) -> Span {
        self.slash.span.joined(self.fields.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct EnumCtorPatAst<'a, M = NoTokenMeta> {
    pub slash: SourceInfo<M>,
    pub name: NameAst<M>,
    pub value: Option<(SourceInfo<M>, PatAst<'a, M>)>,
}

impl<'a, M> Spanned for EnumCtorPatAst<'a, M> {
    fn span(&self) -> Span {
        self.value
            .as_ref()
            .map_or(self.slash.span, |(_, e)| self.slash.span.joined(e.span()))
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PatAst<'a, M = NoTokenMeta> {
    Binding(Option<SourceInfo<M>>, NameAst<M>),
    Wildcard(SourceInfo<M>),
    StructCtor(StructCtorPatAst<'a, M>),
    EnumCtor(&'a EnumCtorPatAst<'a, M>),
    Int(SourceInfo<M>),
}

impl<'a, M> Spanned for PatAst<'a, M> {
    fn span(&self) -> Span {
        match self {
            PatAst::Binding(_, name) => name.span,
            PatAst::Wildcard(e) => e.span,
            PatAst::StructCtor(e) => e.span(),
            PatAst::EnumCtor(e) => e.span(),
            PatAst::Int(e) => e.span,
        }
    }
}
