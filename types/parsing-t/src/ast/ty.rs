use lexing::*;

use crate::{ListAst, NameAst};

#[derive(Clone, Copy, Debug)]
pub enum PathSegment<'a, M> {
    Name(NameAst<M>),
    Params(ListAst<'a, TyAst<'a, M>, M>),
}

impl<'a, M> PathSegment<'a, M> {
    pub fn span(&self) -> Span {
        match self {
            PathSegment::Name(name) => name.span,
            PathSegment::Params(params) => params.span(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct PathAst<'a, M> {
    pub leading_slash: Option<SourceInfo<M>>,
    pub start: PathSegment<'a, M>,
    pub segments: &'a [PathSegment<'a, M>],
}

impl<'a, M> PathAst<'a, M> {
    pub fn span(&self) -> Span {
        let start = self
            .leading_slash
            .as_ref()
            .map_or(self.start.span(), |e| e.span.joined(self.start.span()));
        self.segments
            .last()
            .map_or(start, |e| start.joined(e.span()))
    }
}

#[derive(Clone, Copy, Debug)]
pub enum TyAst<'a, M> {
    Path(PathAst<'a, M>),
    Pointer(&'a TyPointerAst<'a, M>),
    Tuple(ListAst<'a, TyAst<'a, M>, M>),
    Wildcard(SourceInfo<M>),
}

impl<'a, M> TyAst<'a, M> {
    pub fn span(&self) -> Span {
        match self {
            TyAst::Path(path) => path.span(),
            TyAst::Pointer(pointer) => pointer.span(),
            TyAst::Tuple(tuple) => tuple.span(),
            TyAst::Wildcard(wildcard) => wildcard.span,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct TyPointerAst<'a, M> {
    pub carrot: SourceInfo<M>,
    pub mutability: Option<MutabilityAst<'a, M>>,
    pub ty: TyAst<'a, M>,
}

impl<'a, M> TyPointerAst<'a, M> {
    pub fn span(&self) -> Span {
        self.carrot.span.joined(self.ty.span())
    }
}

#[derive(Clone, Copy, Debug)]
pub enum MutabilityAst<'a, M> {
    Mut(SourceInfo<M>),
    Generic(SourceInfo<M>, PathAst<'a, M>),
}

impl<'a, M> MutabilityAst<'a, M> {
    pub fn span(&self) -> Span {
        match self {
            MutabilityAst::Mut(m) => m.span,
            MutabilityAst::Generic(g, path) => g.span.joined(path.span()),
        }
    }
}
