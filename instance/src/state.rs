//! This file is generated, do not edit!
use crate::*;
use cranelift_codegen::ir::*;
use cranelift_codegen::isa::*;
use errors::*;
use instance_types::*;
use lexer::*;
use storage::*;
use typec_types::*;

pub struct MirBuilder<'a> {
    pub func: Func,
    pub ptr_ty: Type,
    pub system_call_convention: CallConv,
    pub tir_data: &'a TirData,
    pub funcs: &'a mut Funcs,
    pub func_ctx: &'a mut FuncCtx,
    pub diagnostics: &'a mut Diagnostics,
    pub mir_builder_context: &'a mut MirBuilderContext,
    pub to_compile: &'a mut ToCompile,
    pub func_instances: &'a mut FuncInstances,
    pub types: &'a mut Types,
    pub ty_instances: &'a mut TyInstances,
    pub bound_impls: &'a mut BoundImpls,
    pub ty_lists: &'a mut TyLists,
    pub reprs: &'a mut Reprs,
    pub ty_graph: &'a mut TyGraph,
    pub vec_pool: &'a VecPool,
    pub func_lists: &'a FuncLists,
    pub ty_comps: &'a TyComps,
    pub repr_fields: &'a ReprFields,
    pub builtin_types: &'a BuiltinTypes,
    pub sources: &'a Sources,
    pub return_dest: Option<mir::Value>,
}

impl<'a> MirBuilder<'a> {
    pub fn new(
        func: Func,
        ptr_ty: Type,
        system_call_convention: CallConv,
        tir_data: &'a TirData,
        funcs: &'a mut Funcs,
        func_ctx: &'a mut FuncCtx,
        diagnostics: &'a mut Diagnostics,
        mir_builder_context: &'a mut MirBuilderContext,
        to_compile: &'a mut ToCompile,
        func_instances: &'a mut FuncInstances,
        types: &'a mut Types,
        ty_instances: &'a mut TyInstances,
        bound_impls: &'a mut BoundImpls,
        ty_lists: &'a mut TyLists,
        reprs: &'a mut Reprs,
        ty_graph: &'a mut TyGraph,
        vec_pool: &'a VecPool,
        func_lists: &'a FuncLists,
        ty_comps: &'a TyComps,
        repr_fields: &'a ReprFields,
        builtin_types: &'a BuiltinTypes,
        sources: &'a Sources,
    ) -> Self {
        Self {
            func,
            ptr_ty,
            system_call_convention,
            tir_data,
            funcs,
            func_ctx,
            diagnostics,
            mir_builder_context,
            to_compile,
            func_instances,
            types,
            ty_instances,
            bound_impls,
            ty_lists,
            reprs,
            ty_graph,
            vec_pool,
            func_lists,
            ty_comps,
            repr_fields,
            builtin_types,
            sources,
            return_dest: None,
        }
    }
}

#[macro_export]
macro_rules! mir_builder {
    ($self:expr, $func:expr, $ptr_ty:expr, $system_call_convention:expr, $tir_data:expr) => {
        MirBuilder::new(
            $func,
            $ptr_ty,
            $system_call_convention,
            &$tir_data,
            &mut $self.funcs,
            &mut $self.func_ctx,
            &mut $self.diagnostics,
            &mut $self.mir_builder_context,
            &mut $self.to_compile,
            &mut $self.func_instances,
            &mut $self.types,
            &mut $self.ty_instances,
            &mut $self.bound_impls,
            &mut $self.ty_lists,
            &mut $self.reprs,
            &mut $self.ty_graph,
            &$self.vec_pool,
            &$self.func_lists,
            &$self.ty_comps,
            &$self.repr_fields,
            &$self.builtin_types,
            &$self.sources,
        )
    };
}

pub struct ReprInstancing<'a> {
    pub ptr_ty: Type,
    pub types: &'a mut Types,
    pub ty_lists: &'a mut TyLists,
    pub ty_instances: &'a mut TyInstances,
    pub repr_fields: &'a mut ReprFields,
    pub reprs: &'a mut Reprs,
    pub bound_impls: &'a mut BoundImpls,
    pub ty_graph: &'a mut TyGraph,
    pub vec_pool: &'a VecPool,
    pub sources: &'a Sources,
    pub ty_comps: &'a TyComps,
    pub builtin_types: &'a BuiltinTypes,
}

impl<'a> ReprInstancing<'a> {
    pub fn new(
        ptr_ty: Type,
        types: &'a mut Types,
        ty_lists: &'a mut TyLists,
        ty_instances: &'a mut TyInstances,
        repr_fields: &'a mut ReprFields,
        reprs: &'a mut Reprs,
        bound_impls: &'a mut BoundImpls,
        ty_graph: &'a mut TyGraph,
        vec_pool: &'a VecPool,
        sources: &'a Sources,
        ty_comps: &'a TyComps,
        builtin_types: &'a BuiltinTypes,
    ) -> Self {
        Self {
            ptr_ty,
            types,
            ty_lists,
            ty_instances,
            repr_fields,
            reprs,
            bound_impls,
            ty_graph,
            vec_pool,
            sources,
            ty_comps,
            builtin_types,
        }
    }
}

#[macro_export]
macro_rules! repr_instancing {
    ($self:expr, $ptr_ty:expr) => {
        ReprInstancing::new(
            $ptr_ty,
            &mut $self.types,
            &mut $self.ty_lists,
            &mut $self.ty_instances,
            &mut $self.repr_fields,
            &mut $self.reprs,
            &mut $self.bound_impls,
            &mut $self.ty_graph,
            &$self.vec_pool,
            &$self.sources,
            &$self.ty_comps,
            &$self.builtin_types,
        )
    };
}

pub struct LayoutBuilder<'a> {
    pub repr_fields: &'a mut ReprFields,
    pub reprs: &'a mut Reprs,
    pub vec_pool: &'a VecPool,
    pub builtin_types: &'a BuiltinTypes,
    pub types: &'a Types,
    pub sources: &'a Sources,
    pub ty_comps: &'a TyComps,
    pub ty_instances: &'a TyInstances,
    pub ty_lists: &'a TyLists,
}

impl<'a> LayoutBuilder<'a> {
    pub fn new(
        repr_fields: &'a mut ReprFields,
        reprs: &'a mut Reprs,
        vec_pool: &'a VecPool,
        builtin_types: &'a BuiltinTypes,
        types: &'a Types,
        sources: &'a Sources,
        ty_comps: &'a TyComps,
        ty_instances: &'a TyInstances,
        ty_lists: &'a TyLists,
    ) -> Self {
        Self {
            repr_fields,
            reprs,
            vec_pool,
            builtin_types,
            types,
            sources,
            ty_comps,
            ty_instances,
            ty_lists,
        }
    }
}

#[macro_export]
macro_rules! layout_builder {
    ($self:expr) => {
        LayoutBuilder::new(
            &mut $self.repr_fields,
            &mut $self.reprs,
            &$self.vec_pool,
            &$self.builtin_types,
            &$self.types,
            &$self.sources,
            &$self.ty_comps,
            &$self.ty_instances,
            &$self.ty_lists,
        )
    };
}
