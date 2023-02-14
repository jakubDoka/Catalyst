use lexing::*;

use crate::*;

#[derive(Clone, Copy, Debug)]
pub enum PathSegmentAst<'a, M = NoTokenMeta> {
    Name(NameAst<M>),
    Params(ListAst<'a, TyAst<'a, M>, M>),
}

impl<'a, M> Spanned for PathSegmentAst<'a, M> {
    fn span(&self) -> Span {
        match self {
            PathSegmentAst::Name(name) => name.span(),
            PathSegmentAst::Params(params) => params.span(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct PathAst<'a, M = NoTokenMeta> {
    pub slash: Option<SourceInfo<M>>,
    pub start: PathSegmentAst<'a, M>,
    pub segments: &'a [PathSegmentAst<'a, M>],
}

impl<'a, M> Spanned for PathAst<'a, M> {
    fn span(&self) -> Span {
        let start = self.slash.as_ref().map_or(self.start.span(), |e| e.span);
        self.segments
            .last()
            .map_or(start, |e| start.joined(e.span()))
    }
}

#[derive(Clone, Copy, Debug)]
pub enum TyAst<'a, M = NoTokenMeta> {
    Path(PathAst<'a, M>),
    Pointer(&'a TyPointerAst<'a, M>),
    Tuple(ListAst<'a, TyAst<'a, M>, M>),
    Array(&'a TyArrayAst<'a, M>),
    Wildcard(SourceInfo<M>),
}

impl<'a, M> Spanned for TyAst<'a, M> {
    fn span(&self) -> Span {
        match self {
            TyAst::Path(path) => path.span(),
            TyAst::Pointer(pointer) => pointer.span(),
            TyAst::Tuple(tuple) => tuple.span(),
            TyAst::Array(array) => array.span(),
            TyAst::Wildcard(wildcard) => wildcard.span,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct TyArrayAst<'a, M = NoTokenMeta> {
    pub start: SourceInfo<M>,
    pub ty: TyAst<'a, M>,
    pub semi: SourceInfo<M>,
    pub size: ExprAst<'a, M>,
    pub end: SourceInfo<M>,
}

impl<'a, M> Spanned for TyArrayAst<'a, M> {
    fn span(&self) -> Span {
        self.start.span.joined(self.end.span)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct TyPointerAst<'a, M = NoTokenMeta> {
    pub carrot: SourceInfo<M>,
    pub mutability: Option<MutabilityAst<'a, M>>,
    pub ty: TyAst<'a, M>,
}

impl<'a, M> Spanned for TyPointerAst<'a, M> {
    fn span(&self) -> Span {
        self.carrot.span.joined(self.ty.span())
    }
}

#[derive(Clone, Copy, Debug)]
pub enum MutabilityAst<'a, M = NoTokenMeta> {
    Mut(SourceInfo<M>),
    Generic(SourceInfo<M>, PathAst<'a, M>),
}

impl<'a, M> Spanned for MutabilityAst<'a, M> {
    fn span(&self) -> Span {
        match self {
            MutabilityAst::Mut(m) => m.span,
            MutabilityAst::Generic(g, path) => g.span.joined(path.span()),
        }
    }
}
