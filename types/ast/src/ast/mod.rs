pub mod expr;
pub mod func;
pub mod items;
pub mod manifest;
pub mod pat;
pub mod spec;
pub mod ty;

use std::ops::Deref;

use crate::*;
use lexing::*;
use storage::*;

pub trait Spanned {
    fn span(&self) -> Span;
}

impl Spanned for Span {
    fn span(&self) -> Span {
        *self
    }
}

impl<M> Spanned for SourceInfo<M> {
    fn span(&self) -> Span {
        self.span
    }
}

impl<T> Spanned for Option<T> {
    fn span(&self) -> Span {
        unimplemented!()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ListAst<'a, T, M = NoTokenMeta> {
    pub start: SourceInfo<M>,
    pub elements: &'a [ListElemAst<T, M>],
    pub end: SourceInfo<M>,
}

impl<'a, T, M> ListAst<'a, T, M> {
    pub fn iter(&self) -> impl Iterator<Item = &'a T> {
        self.elements.iter().map(|e| &e.value)
    }
}

impl<'a, T, M> Spanned for ListAst<'a, T, M> {
    fn span(&self) -> Span {
        self.start.span.joined(self.end.span)
    }
}

impl<T, M> Deref for ListAst<'_, T, M> {
    type Target = [ListElemAst<T, M>];

    fn deref(&self) -> &Self::Target {
        self.elements
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ListElemAst<T, M> {
    pub value: T,
    pub delim: Option<SourceInfo<M>>,
}

impl<T, M> Deref for ListElemAst<T, M> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

#[derive(Clone, Copy, Debug)]
pub struct WrappedAst<T, M> {
    pub start: SourceInfo<M>,
    pub value: T,
    pub end: SourceInfo<M>,
}

impl<T, M> Spanned for WrappedAst<T, M> {
    fn span(&self) -> Span {
        self.start.span.joined(self.end.span)
    }
}

impl<T, M> Deref for WrappedAst<T, M> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

#[derive(Clone, Copy, Debug)]
pub struct NameAst<M = NoTokenMeta> {
    pub source_info: SourceInfo<M>,
    pub ident: Ident,
}

impl<M> Spanned for NameAst<M> {
    fn span(&self) -> Span {
        self.source_info.span
    }
}

impl<M> Deref for NameAst<M> {
    type Target = SourceInfo<M>;

    fn deref(&self) -> &Self::Target {
        &self.source_info
    }
}

#[derive(Clone, Copy, Debug)]
pub struct VisAst<M = NoTokenMeta> {
    pub source_meta: SourceInfo<M>,
    pub vis: Vis,
}

impl<M> Spanned for VisAst<M> {
    fn span(&self) -> Span {
        self.source_meta.span
    }
}
