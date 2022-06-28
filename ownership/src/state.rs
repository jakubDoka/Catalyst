//! This file is generated, do not edit!
use crate::*;
use errors::*;
use lexer::*;
use storage::*;
use typec_types::*;

pub struct OwnershipSolver<'a> {
    pub o_ctx: &'a mut OwnershipContext,
    pub diagnostics: &'a mut Diagnostics,
    pub bound_impls: &'a mut BoundImpls,
    pub types: &'a mut Types,
    pub vec_pool: &'a VecPool,
    pub tir_data: &'a TirData,
    pub funcs: &'a Funcs,
    pub ty_comps: &'a TyComps,
    pub ty_lists: &'a TyLists,
    pub sources: &'a Sources,
    pub builtin_types: &'a BuiltinTypes,
}

impl<'a> OwnershipSolver<'a> {
    pub fn new(
        o_ctx: &'a mut OwnershipContext,
        diagnostics: &'a mut Diagnostics,
        bound_impls: &'a mut BoundImpls,
        types: &'a mut Types,
        vec_pool: &'a VecPool,
        tir_data: &'a TirData,
        funcs: &'a Funcs,
        ty_comps: &'a TyComps,
        ty_lists: &'a TyLists,
        sources: &'a Sources,
        builtin_types: &'a BuiltinTypes,
    ) -> Self {
        Self {
            o_ctx,
            diagnostics,
            bound_impls,
            types,
            vec_pool,
            tir_data,
            funcs,
            ty_comps,
            ty_lists,
            sources,
            builtin_types,
        }
    }
}

#[macro_export]
macro_rules! ownership_solver {
    ($self:expr) => {
        OwnershipSolver::new(
            &mut $self.o_ctx,
            &mut $self.diagnostics,
            &mut $self.bound_impls,
            &mut $self.types,
            &$self.vec_pool,
            &$self.tir_data,
            &$self.funcs,
            &$self.ty_comps,
            &$self.ty_lists,
            &$self.sources,
            &$self.builtin_types,
        )
    };
}

pub struct DropSolver<'a> {
    pub o_ctx: &'a mut OwnershipContext,
    pub tir_data: &'a mut TirData,
    pub scope_context: &'a mut ScopeContext,
    pub bound_impls: &'a mut BoundImpls,
    pub types: &'a mut Types,
    pub ty_lists: &'a mut TyLists,
    pub ty_instances: &'a mut TyInstances,
    pub ty_graph: &'a mut TyGraph,
    pub vec_pool: &'a VecPool,
    pub builtin_types: &'a BuiltinTypes,
    pub sources: &'a Sources,
    pub funcs: &'a Funcs,
    pub ty_comps: &'a TyComps,
}

impl<'a> DropSolver<'a> {
    pub fn new(
        o_ctx: &'a mut OwnershipContext,
        tir_data: &'a mut TirData,
        scope_context: &'a mut ScopeContext,
        bound_impls: &'a mut BoundImpls,
        types: &'a mut Types,
        ty_lists: &'a mut TyLists,
        ty_instances: &'a mut TyInstances,
        ty_graph: &'a mut TyGraph,
        vec_pool: &'a VecPool,
        builtin_types: &'a BuiltinTypes,
        sources: &'a Sources,
        funcs: &'a Funcs,
        ty_comps: &'a TyComps,
    ) -> Self {
        Self {
            o_ctx,
            tir_data,
            scope_context,
            bound_impls,
            types,
            ty_lists,
            ty_instances,
            ty_graph,
            vec_pool,
            builtin_types,
            sources,
            funcs,
            ty_comps,
        }
    }
}

#[macro_export]
macro_rules! drop_solver {
    ($self:expr) => {
        DropSolver::new(
            &mut $self.o_ctx,
            &mut $self.tir_data,
            &mut $self.scope_context,
            &mut $self.bound_impls,
            &mut $self.types,
            &mut $self.ty_lists,
            &mut $self.ty_instances,
            &mut $self.ty_graph,
            &$self.vec_pool,
            &$self.builtin_types,
            &$self.sources,
            &$self.funcs,
            &$self.ty_comps,
        )
    };
}
