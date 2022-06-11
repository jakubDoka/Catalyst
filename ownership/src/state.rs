//! This file is generated, do not edit!
use errors::*;
use lexer::*;
use ownership_types::*;
use typec_types::*;

pub struct OwnershipSolver<'a> {
    pub o_ctx: &'a mut OwnershipContext,
    pub diagnostics: &'a mut Diagnostics,
    pub tir_data: &'a TirData,
    pub funcs: &'a Funcs,
    pub types: &'a Types,
    pub ty_comps: &'a TyComps,
    pub ty_lists: &'a TyLists,
    pub sources: &'a Sources,
}

impl<'a> OwnershipSolver<'a> {
    pub fn new(
        o_ctx: &'a mut OwnershipContext,
        diagnostics: &'a mut Diagnostics,
        tir_data: &'a TirData,
        funcs: &'a Funcs,
        types: &'a Types,
        ty_comps: &'a TyComps,
        ty_lists: &'a TyLists,
        sources: &'a Sources,
    ) -> Self {
        Self {
            o_ctx,
            diagnostics,
            tir_data,
            funcs,
            types,
            ty_comps,
            ty_lists,
            sources,
        }
    }
}

#[macro_export]
macro_rules! ownership_solver {
    ($self:expr) => {
        OwnershipSolver::new(
            &mut $self.o_ctx,
            &mut $self.diagnostics,
            &$self.tir_data,
            &$self.funcs,
            &$self.types,
            &$self.ty_comps,
            &$self.ty_lists,
            &$self.sources,
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
            &$self.builtin_types,
            &$self.sources,
            &$self.funcs,
            &$self.ty_comps,
        )
    };
}
