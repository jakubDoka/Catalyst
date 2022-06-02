//! This file is generated, do not edit!
use module_types::*;
use typec_types::*;
use lexer::*;
use errors::*;
use ast::*;
use crate::scope::*;
use storage::*;

pub struct BoundVerifier<'a> {
	pub scope_context: &'a mut ScopeContext,
	pub ty_lists: &'a mut TyLists,
	pub modules: &'a mut Modules,
	pub scope: &'a mut Scope,
	pub diagnostics: &'a mut Diagnostics,
	pub funcs: &'a mut Funcs,
	pub func_lists: &'a mut FuncLists,
	pub bound_impls: &'a mut BoundImpls,
	pub ast_data: &'a AstData,
	pub types: &'a Types,
	pub builtin_types: &'a BuiltinTypes,
	pub sources: &'a Sources,
	pub func_meta: &'a FuncMeta,
}

impl<'a> BoundVerifier<'a> {
	pub fn new(
		scope_context: &'a mut ScopeContext,
		ty_lists: &'a mut TyLists,
		modules: &'a mut Modules,
		scope: &'a mut Scope,
		diagnostics: &'a mut Diagnostics,
		funcs: &'a mut Funcs,
		func_lists: &'a mut FuncLists,
		bound_impls: &'a mut BoundImpls,
		ast_data: &'a AstData,
		types: &'a Types,
		builtin_types: &'a BuiltinTypes,
		sources: &'a Sources,
		func_meta: &'a FuncMeta,
	) -> Self {
		Self {
			scope_context,
			ty_lists,
			modules,
			scope,
			diagnostics,
			funcs,
			func_lists,
			bound_impls,
			ast_data,
			types,
			builtin_types,
			sources,
			func_meta,
		}
	}
}

#[macro_export]
macro_rules! bound_verifier {
	($self:expr) => {
		BoundVerifier::new(
			&mut $self.scope_context,
			&mut $self.ty_lists,
			&mut $self.modules,
			&mut $self.scope,
			&mut $self.diagnostics,
			&mut $self.funcs,
			&mut $self.func_lists,
			&mut $self.bound_impls,
			&$self.ast_data,
			&$self.types,
			&$self.builtin_types,
			&$self.sources,
			&$self.func_meta,
		)
	};
}

pub struct IdentHasher<'a> {
	pub diagnostics: &'a mut Diagnostics,
	pub sources: &'a Sources,
	pub ast_data: &'a AstData,
	pub scope: &'a Scope,
	pub types: &'a Types,
}

impl<'a> IdentHasher<'a> {
	pub fn new(
		diagnostics: &'a mut Diagnostics,
		sources: &'a Sources,
		ast_data: &'a AstData,
		scope: &'a Scope,
		types: &'a Types,
	) -> Self {
		Self {
			diagnostics,
			sources,
			ast_data,
			scope,
			types,
		}
	}
}

#[macro_export]
macro_rules! ident_hasher {
	($self:expr) => {
		IdentHasher::new(
			&mut $self.diagnostics,
			&$self.sources,
			&$self.ast_data,
			&$self.scope,
			&$self.types,
		)
	};
}

pub struct TyParser<'a> {
	pub types: &'a mut Types,
	pub diagnostics: &'a mut Diagnostics,
	pub scope: &'a mut Scope,
	pub ty_lists: &'a mut TyLists,
	pub instances: &'a mut Instances,
	pub bound_impls: &'a mut BoundImpls,
	pub ast_data: &'a AstData,
	pub ty_comps: &'a TyComps,
	pub sources: &'a Sources,
	pub builtin_types: &'a BuiltinTypes,
}

impl<'a> TyParser<'a> {
	pub fn new(
		types: &'a mut Types,
		diagnostics: &'a mut Diagnostics,
		scope: &'a mut Scope,
		ty_lists: &'a mut TyLists,
		instances: &'a mut Instances,
		bound_impls: &'a mut BoundImpls,
		ast_data: &'a AstData,
		ty_comps: &'a TyComps,
		sources: &'a Sources,
		builtin_types: &'a BuiltinTypes,
	) -> Self {
		Self {
			types,
			diagnostics,
			scope,
			ty_lists,
			instances,
			bound_impls,
			ast_data,
			ty_comps,
			sources,
			builtin_types,
		}
	}
}

#[macro_export]
macro_rules! ty_parser {
	($self:expr) => {
		TyParser::new(
			&mut $self.types,
			&mut $self.diagnostics,
			&mut $self.scope,
			&mut $self.ty_lists,
			&mut $self.instances,
			&mut $self.bound_impls,
			&$self.ast_data,
			&$self.ty_comps,
			&$self.sources,
			&$self.builtin_types,
		)
	};
}

pub struct ScopeBuilder<'a> {
	pub source: Source,
	pub scope: &'a mut Scope,
	pub funcs: &'a mut Funcs,
	pub types: &'a mut Types,
	pub ty_lists: &'a mut TyLists,
	pub ty_comps: &'a mut TyComps,
	pub ty_comp_lookup: &'a mut TyCompLookup,
	pub func_lists: &'a mut FuncLists,
	pub instances: &'a mut Instances,
	pub bound_impls: &'a mut BoundImpls,
	pub modules: &'a mut Modules,
	pub diagnostics: &'a mut Diagnostics,
	pub scope_context: &'a mut ScopeContext,
	pub func_meta: &'a mut FuncMeta,
	pub to_compile: &'a mut ToCompile,
	pub func_instances: &'a mut FuncInstances,
	pub macros: &'a mut Macros,
	pub to_link: &'a mut ToLink,
	pub globals: &'a mut Globals,
	pub initializers: &'a mut Initializers,
	pub builtin_types: &'a BuiltinTypes,
	pub sources: &'a Sources,
	pub ast_data: &'a AstData,
}

impl<'a> ScopeBuilder<'a> {
	pub fn new(
		source: Source,
		scope: &'a mut Scope,
		funcs: &'a mut Funcs,
		types: &'a mut Types,
		ty_lists: &'a mut TyLists,
		ty_comps: &'a mut TyComps,
		ty_comp_lookup: &'a mut TyCompLookup,
		func_lists: &'a mut FuncLists,
		instances: &'a mut Instances,
		bound_impls: &'a mut BoundImpls,
		modules: &'a mut Modules,
		diagnostics: &'a mut Diagnostics,
		scope_context: &'a mut ScopeContext,
		func_meta: &'a mut FuncMeta,
		to_compile: &'a mut ToCompile,
		func_instances: &'a mut FuncInstances,
		macros: &'a mut Macros,
		to_link: &'a mut ToLink,
		globals: &'a mut Globals,
		initializers: &'a mut Initializers,
		builtin_types: &'a BuiltinTypes,
		sources: &'a Sources,
		ast_data: &'a AstData,
	) -> Self {
		Self {
			source,
			scope,
			funcs,
			types,
			ty_lists,
			ty_comps,
			ty_comp_lookup,
			func_lists,
			instances,
			bound_impls,
			modules,
			diagnostics,
			scope_context,
			func_meta,
			to_compile,
			func_instances,
			macros,
			to_link,
			globals,
			initializers,
			builtin_types,
			sources,
			ast_data,
		}
	}
}

#[macro_export]
macro_rules! scope_builder {
	($self:expr, $source:expr) => {
		ScopeBuilder::new(
			$source,
			&mut $self.scope,
			&mut $self.funcs,
			&mut $self.types,
			&mut $self.ty_lists,
			&mut $self.ty_comps,
			&mut $self.ty_comp_lookup,
			&mut $self.func_lists,
			&mut $self.instances,
			&mut $self.bound_impls,
			&mut $self.modules,
			&mut $self.diagnostics,
			&mut $self.scope_context,
			&mut $self.func_meta,
			&mut $self.to_compile,
			&mut $self.func_instances,
			&mut $self.macros,
			&mut $self.to_link,
			&mut $self.globals,
			&mut $self.initializers,
			&$self.builtin_types,
			&$self.sources,
			&$self.ast_data,
		)
	};
}

pub struct TirBuilder<'a> {
	pub func: Func,
	pub global: Global,
	pub ty_lists: &'a mut TyLists,
	pub instances: &'a mut Instances,
	pub ty_comps: &'a mut TyComps,
	pub ty_comp_lookup: &'a mut TyCompLookup,
	pub bound_impls: &'a mut BoundImpls,
	pub scope_context: &'a mut ScopeContext,
	pub tir_data: &'a mut TirData,
	pub scope: &'a mut Scope,
	pub types: &'a mut Types,
	pub tir_stack: &'a mut TirStack,
	pub modules: &'a mut Modules,
	pub diagnostics: &'a mut Diagnostics,
	pub func_meta: &'a mut FuncMeta,
	pub tir_pattern_graph: &'a mut TirPatternGraph,
	pub globals: &'a mut Globals,
	pub funcs: &'a mut Funcs,
	pub func_lists: &'a FuncLists,
	pub builtin_types: &'a BuiltinTypes,
	pub sources: &'a Sources,
	pub ast_data: &'a AstData,
}

impl<'a> TirBuilder<'a> {
	pub fn new(
		ty_lists: &'a mut TyLists,
		instances: &'a mut Instances,
		ty_comps: &'a mut TyComps,
		ty_comp_lookup: &'a mut TyCompLookup,
		bound_impls: &'a mut BoundImpls,
		scope_context: &'a mut ScopeContext,
		tir_data: &'a mut TirData,
		scope: &'a mut Scope,
		types: &'a mut Types,
		tir_stack: &'a mut TirStack,
		modules: &'a mut Modules,
		diagnostics: &'a mut Diagnostics,
		func_meta: &'a mut FuncMeta,
		tir_pattern_graph: &'a mut TirPatternGraph,
		globals: &'a mut Globals,
		funcs: &'a mut Funcs,
		func_lists: &'a FuncLists,
		builtin_types: &'a BuiltinTypes,
		sources: &'a Sources,
		ast_data: &'a AstData,
	) -> Self {
		Self {
			func: Func::reserved_value(),
			global: Global::reserved_value(),
			ty_lists,
			instances,
			ty_comps,
			ty_comp_lookup,
			bound_impls,
			scope_context,
			tir_data,
			scope,
			types,
			tir_stack,
			modules,
			diagnostics,
			func_meta,
			tir_pattern_graph,
			globals,
			funcs,
			func_lists,
			builtin_types,
			sources,
			ast_data,
		}
	}
}

#[macro_export]
macro_rules! tir_builder {
	($self:expr) => {
		TirBuilder::new(
			&mut $self.ty_lists,
			&mut $self.instances,
			&mut $self.ty_comps,
			&mut $self.ty_comp_lookup,
			&mut $self.bound_impls,
			&mut $self.scope_context,
			&mut $self.tir_data,
			&mut $self.scope,
			&mut $self.types,
			&mut $self.tir_stack,
			&mut $self.modules,
			&mut $self.diagnostics,
			&mut $self.func_meta,
			&mut $self.tir_pattern_graph,
			&mut $self.globals,
			&mut $self.funcs,
			&$self.func_lists,
			&$self.builtin_types,
			&$self.sources,
			&$self.ast_data,
		)
	};
}

pub struct TyBuilder<'a> {
	pub ty: Ty,
	pub scope: &'a mut Scope,
	pub types: &'a mut Types,
	pub ty_lists: &'a mut TyLists,
	pub ty_comps: &'a mut TyComps,
	pub ty_comp_lookup: &'a mut TyCompLookup,
	pub instances: &'a mut Instances,
	pub bound_impls: &'a mut BoundImpls,
	pub scope_context: &'a mut ScopeContext,
	pub ty_graph: &'a mut TyGraph,
	pub modules: &'a mut Modules,
	pub diagnostics: &'a mut Diagnostics,
	pub builtin_types: &'a BuiltinTypes,
	pub sources: &'a Sources,
	pub ast_data: &'a AstData,
}

impl<'a> TyBuilder<'a> {
	pub fn new(
		ty: Ty,
		scope: &'a mut Scope,
		types: &'a mut Types,
		ty_lists: &'a mut TyLists,
		ty_comps: &'a mut TyComps,
		ty_comp_lookup: &'a mut TyCompLookup,
		instances: &'a mut Instances,
		bound_impls: &'a mut BoundImpls,
		scope_context: &'a mut ScopeContext,
		ty_graph: &'a mut TyGraph,
		modules: &'a mut Modules,
		diagnostics: &'a mut Diagnostics,
		builtin_types: &'a BuiltinTypes,
		sources: &'a Sources,
		ast_data: &'a AstData,
	) -> Self {
		Self {
			ty,
			scope,
			types,
			ty_lists,
			ty_comps,
			ty_comp_lookup,
			instances,
			bound_impls,
			scope_context,
			ty_graph,
			modules,
			diagnostics,
			builtin_types,
			sources,
			ast_data,
		}
	}
}

#[macro_export]
macro_rules! ty_builder {
	($self:expr, $ty:expr) => {
		TyBuilder::new(
			$ty,
			&mut $self.scope,
			&mut $self.types,
			&mut $self.ty_lists,
			&mut $self.ty_comps,
			&mut $self.ty_comp_lookup,
			&mut $self.instances,
			&mut $self.bound_impls,
			&mut $self.scope_context,
			&mut $self.ty_graph,
			&mut $self.modules,
			&mut $self.diagnostics,
			&$self.builtin_types,
			&$self.sources,
			&$self.ast_data,
		)
	};
}

pub struct GlobalBuilder<'a> {
	pub diagnostics: &'a mut Diagnostics,
}

impl<'a> GlobalBuilder<'a> {
	pub fn new(
		diagnostics: &'a mut Diagnostics,
	) -> Self {
		Self {
			diagnostics,
		}
	}
}

#[macro_export]
macro_rules! global_builder {
	($self:expr) => {
		GlobalBuilder::new(
			&mut $self.diagnostics,
		)
	};
}
