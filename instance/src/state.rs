//! This file is generated, do not edit!
use crate::*;
use typec_types::*;
use lexer::*;
use instance_types::*;
use errors::*;
use cranelift_codegen::ir::*;
use cranelift_codegen::isa::*;

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
	pub reprs: &'a Reprs,
	pub types: &'a Types,
	pub ty_lists: &'a TyLists,
	pub func_lists: &'a FuncLists,
	pub ty_comps: &'a TyComps,
	pub bound_impls: &'a BoundImpls,
	pub repr_fields: &'a ReprFields,
	pub builtin_types: &'a BuiltinTypes,
	pub sources: &'a Sources,
	pub func_meta: &'a FuncMeta,
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
		reprs: &'a Reprs,
		types: &'a Types,
		ty_lists: &'a TyLists,
		func_lists: &'a FuncLists,
		ty_comps: &'a TyComps,
		bound_impls: &'a BoundImpls,
		repr_fields: &'a ReprFields,
		builtin_types: &'a BuiltinTypes,
		sources: &'a Sources,
		func_meta: &'a FuncMeta,
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
			reprs,
			types,
			ty_lists,
			func_lists,
			ty_comps,
			bound_impls,
			repr_fields,
			builtin_types,
			sources,
			func_meta,
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
			&$self.reprs,
			&$self.types,
			&$self.ty_lists,
			&$self.func_lists,
			&$self.ty_comps,
			&$self.bound_impls,
			&$self.repr_fields,
			&$self.builtin_types,
			&$self.sources,
			&$self.func_meta,
		)
	};
}

pub struct ReprInstancing<'a> {
	pub types: &'a mut Types,
	pub ty_lists: &'a mut TyLists,
	pub instances: &'a mut Instances,
	pub repr_fields: &'a mut ReprFields,
	pub reprs: &'a mut Reprs,
	pub sources: &'a Sources,
	pub ty_comps: &'a TyComps,
	pub ptr_ty: Type,
}

impl<'a> ReprInstancing<'a> {
	pub fn new(
		types: &'a mut Types,
		ty_lists: &'a mut TyLists,
		instances: &'a mut Instances,
		repr_fields: &'a mut ReprFields,
		reprs: &'a mut Reprs,
		sources: &'a Sources,
		ty_comps: &'a TyComps,
		ptr_ty: Type,
	) -> Self {
		Self {
			types,
			ty_lists,
			instances,
			repr_fields,
			reprs,
			sources,
			ty_comps,
			ptr_ty,
		}
	}
}

#[macro_export]
macro_rules! repr_instancing {
	($self:expr, $ptr_ty:expr) => {
		ReprInstancing::new(
			&mut $self.types,
			&mut $self.ty_lists,
			&mut $self.instances,
			&mut $self.repr_fields,
			&mut $self.reprs,
			&$self.sources,
			&$self.ty_comps,
			$ptr_ty,
		)
	};
}

pub struct LayoutBuilder<'a> {
	pub repr_fields: &'a mut ReprFields,
	pub reprs: &'a mut Reprs,
	pub types: &'a Types,
	pub sources: &'a Sources,
	pub ty_comps: &'a TyComps,
	pub instances: &'a Instances,
	pub ty_lists: &'a TyLists,
}

impl<'a> LayoutBuilder<'a> {
	pub fn new(
		repr_fields: &'a mut ReprFields,
		reprs: &'a mut Reprs,
		types: &'a Types,
		sources: &'a Sources,
		ty_comps: &'a TyComps,
		instances: &'a Instances,
		ty_lists: &'a TyLists,
	) -> Self {
		Self {
			repr_fields,
			reprs,
			types,
			sources,
			ty_comps,
			instances,
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
			&$self.types,
			&$self.sources,
			&$self.ty_comps,
			&$self.instances,
			&$self.ty_lists,
		)
	};
}

