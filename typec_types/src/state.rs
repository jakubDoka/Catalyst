//! This file is generated, do not edit!
use crate::*;
use errors::*;
use lexer::*;
use storage::*;

pub struct TyFactory<'a> {
    pub types: &'a mut Types,
    pub ty_lists: &'a mut TyLists,
    pub ty_instances: &'a mut TyInstances,
    pub bound_impls: &'a mut BoundImpls,
    pub vec_pool: &'a VecPool,
    pub ty_comps: &'a TyComps,
    pub sources: &'a Sources,
    pub builtin_types: &'a BuiltinTypes,
}

impl<'a> TyFactory<'a> {
    pub fn new(
        types: &'a mut Types,
        ty_lists: &'a mut TyLists,
        ty_instances: &'a mut TyInstances,
        bound_impls: &'a mut BoundImpls,
        vec_pool: &'a VecPool,
        ty_comps: &'a TyComps,
        sources: &'a Sources,
        builtin_types: &'a BuiltinTypes,
    ) -> Self {
        Self {
            types,
            ty_lists,
            ty_instances,
            bound_impls,
            vec_pool,
            ty_comps,
            sources,
            builtin_types,
        }
    }
}

#[macro_export]
macro_rules! ty_factory {
    ($self:expr) => {
        TyFactory::new(
            &mut $self.types,
            &mut $self.ty_lists,
            &mut $self.ty_instances,
            &mut $self.bound_impls,
            &$self.vec_pool,
            &$self.ty_comps,
            &$self.sources,
            &$self.builtin_types,
        )
    };
}

pub struct BoundChecker<'a> {
    pub bound_impls: &'a mut BoundImpls,
    pub diagnostics: &'a mut Diagnostics,
    pub types: &'a mut Types,
    pub vec_pool: &'a VecPool,
    pub ty_lists: &'a TyLists,
    pub builtin_types: &'a BuiltinTypes,
}

impl<'a> BoundChecker<'a> {
    pub fn new(
        bound_impls: &'a mut BoundImpls,
        diagnostics: &'a mut Diagnostics,
        types: &'a mut Types,
        vec_pool: &'a VecPool,
        ty_lists: &'a TyLists,
        builtin_types: &'a BuiltinTypes,
    ) -> Self {
        Self {
            bound_impls,
            diagnostics,
            types,
            vec_pool,
            ty_lists,
            builtin_types,
        }
    }
}

#[macro_export]
macro_rules! bound_checker {
    ($self:expr) => {
        BoundChecker::new(
            &mut $self.bound_impls,
            &mut $self.diagnostics,
            &mut $self.types,
            &$self.vec_pool,
            &$self.ty_lists,
            &$self.builtin_types,
        )
    };
}
