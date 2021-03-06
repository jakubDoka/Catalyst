//! This file is generated, do not edit!
use module_types::*;
use typec_types::*;
use lexer::*;
use errors::*;
use ast::*;
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
	pub types: &'a mut Types,
	pub ty_comps: &'a TyComps,
	pub vec_pool: &'a VecPool,
	pub ast_data: &'a AstData,
	pub builtin_types: &'a BuiltinTypes,
	pub sources: &'a Sources,
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
		types: &'a mut Types,
		ty_comps: &'a TyComps,
		vec_pool: &'a VecPool,
		ast_data: &'a AstData,
		builtin_types: &'a BuiltinTypes,
		sources: &'a Sources,
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
			types,
			ty_comps,
			vec_pool,
			ast_data,
			builtin_types,
			sources,
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
			&mut $self.types,
			&$self.ty_comps,
			&$self.vec_pool,
			&$self.ast_data,
			&$self.builtin_types,
			&$self.sources,
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
	pub ty_instances: &'a mut TyInstances,
	pub bound_impls: &'a mut BoundImpls,
	pub ty_comps: &'a mut TyComps,
	pub ty_graph: &'a mut TyGraph,
	pub vec_pool: &'a VecPool,
	pub ast_data: &'a AstData,
	pub sources: &'a Sources,
	pub builtin_types: &'a BuiltinTypes,
}

impl<'a> TyParser<'a> {
	pub fn new(
		types: &'a mut Types,
		diagnostics: &'a mut Diagnostics,
		scope: &'a mut Scope,
		ty_lists: &'a mut TyLists,
		ty_instances: &'a mut TyInstances,
		bound_impls: &'a mut BoundImpls,
		ty_comps: &'a mut TyComps,
		ty_graph: &'a mut TyGraph,
		vec_pool: &'a VecPool,
		ast_data: &'a AstData,
		sources: &'a Sources,
		builtin_types: &'a BuiltinTypes,
	) -> Self {
		Self {
			types,
			diagnostics,
			scope,
			ty_lists,
			ty_instances,
			bound_impls,
			ty_comps,
			ty_graph,
			vec_pool,
			ast_data,
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
			&mut $self.ty_instances,
			&mut $self.bound_impls,
			&mut $self.ty_comps,
			&mut $self.ty_graph,
			&$self.vec_pool,
			&$self.ast_data,
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
	pub func_lists: &'a mut FuncLists,
	pub ty_instances: &'a mut TyInstances,
	pub bound_impls: &'a mut BoundImpls,
	pub modules: &'a mut Modules,
	pub diagnostics: &'a mut Diagnostics,
	pub scope_context: &'a mut ScopeContext,
	pub to_compile: &'a mut ToCompile,
	pub func_instances: &'a mut FuncInstances,
	pub macros: &'a mut Macros,
	pub to_link: &'a mut ToLink,
	pub globals: &'a mut Globals,
	pub initializers: &'a mut Initializers,
	pub ty_graph: &'a mut TyGraph,
	pub vec_pool: &'a VecPool,
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
		func_lists: &'a mut FuncLists,
		ty_instances: &'a mut TyInstances,
		bound_impls: &'a mut BoundImpls,
		modules: &'a mut Modules,
		diagnostics: &'a mut Diagnostics,
		scope_context: &'a mut ScopeContext,
		to_compile: &'a mut ToCompile,
		func_instances: &'a mut FuncInstances,
		macros: &'a mut Macros,
		to_link: &'a mut ToLink,
		globals: &'a mut Globals,
		initializers: &'a mut Initializers,
		ty_graph: &'a mut TyGraph,
		vec_pool: &'a VecPool,
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
			func_lists,
			ty_instances,
			bound_impls,
			modules,
			diagnostics,
			scope_context,
			to_compile,
			func_instances,
			macros,
			to_link,
			globals,
			initializers,
			ty_graph,
			vec_pool,
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
			&mut $self.func_lists,
			&mut $self.ty_instances,
			&mut $self.bound_impls,
			&mut $self.modules,
			&mut $self.diagnostics,
			&mut $self.scope_context,
			&mut $self.to_compile,
			&mut $self.func_instances,
			&mut $self.macros,
			&mut $self.to_link,
			&mut $self.globals,
			&mut $self.initializers,
			&mut $self.ty_graph,
			&$self.vec_pool,
			&$self.builtin_types,
			&$self.sources,
			&$self.ast_data,
		)
	};
}

pub struct TirBuilder<'a> {
	pub func: Func,
	pub global: Global,
	pub stack_frame: Option<Tir>,
	pub ty_lists: &'a mut TyLists,
	pub ty_instances: &'a mut TyInstances,
	pub ty_comps: &'a mut TyComps,
	pub bound_impls: &'a mut BoundImpls,
	pub scope_context: &'a mut ScopeContext,
	pub tir_data: &'a mut TirData,
	pub scope: &'a mut Scope,
	pub types: &'a mut Types,
	pub tir_stack: &'a mut TirStack,
	pub modules: &'a mut Modules,
	pub diagnostics: &'a mut Diagnostics,
	pub tir_pattern_graph: &'a mut TirPatternGraph,
	pub globals: &'a mut Globals,
	pub global_map: &'a mut GlobalMap,
	pub global_data: &'a mut GlobalData,
	pub funcs: &'a mut Funcs,
	pub ty_graph: &'a mut TyGraph,
	pub func_instances: &'a mut FuncInstances,
	pub vec_pool: &'a VecPool,
	pub func_lists: &'a FuncLists,
	pub builtin_types: &'a BuiltinTypes,
	pub sources: &'a Sources,
	pub ast_data: &'a AstData,
}

impl<'a> TirBuilder<'a> {
	pub fn new(
		ty_lists: &'a mut TyLists,
		ty_instances: &'a mut TyInstances,
		ty_comps: &'a mut TyComps,
		bound_impls: &'a mut BoundImpls,
		scope_context: &'a mut ScopeContext,
		tir_data: &'a mut TirData,
		scope: &'a mut Scope,
		types: &'a mut Types,
		tir_stack: &'a mut TirStack,
		modules: &'a mut Modules,
		diagnostics: &'a mut Diagnostics,
		tir_pattern_graph: &'a mut TirPatternGraph,
		globals: &'a mut Globals,
		global_map: &'a mut GlobalMap,
		global_data: &'a mut GlobalData,
		funcs: &'a mut Funcs,
		ty_graph: &'a mut TyGraph,
		func_instances: &'a mut FuncInstances,
		vec_pool: &'a VecPool,
		func_lists: &'a FuncLists,
		builtin_types: &'a BuiltinTypes,
		sources: &'a Sources,
		ast_data: &'a AstData,
	) -> Self {
		Self {
			func: Func::reserved_value(),
			global: Global::reserved_value(),
			stack_frame: None,
			ty_lists,
			ty_instances,
			ty_comps,
			bound_impls,
			scope_context,
			tir_data,
			scope,
			types,
			tir_stack,
			modules,
			diagnostics,
			tir_pattern_graph,
			globals,
			global_map,
			global_data,
			funcs,
			ty_graph,
			func_instances,
			vec_pool,
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
			&mut $self.ty_instances,
			&mut $self.ty_comps,
			&mut $self.bound_impls,
			&mut $self.scope_context,
			&mut $self.tir_data,
			&mut $self.scope,
			&mut $self.types,
			&mut $self.tir_stack,
			&mut $self.modules,
			&mut $self.diagnostics,
			&mut $self.tir_pattern_graph,
			&mut $self.globals,
			&mut $self.global_map,
			&mut $self.global_data,
			&mut $self.funcs,
			&mut $self.ty_graph,
			&mut $self.func_instances,
			&$self.vec_pool,
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
	pub ty_instances: &'a mut TyInstances,
	pub bound_impls: &'a mut BoundImpls,
	pub scope_context: &'a mut ScopeContext,
	pub ty_graph: &'a mut TyGraph,
	pub modules: &'a mut Modules,
	pub diagnostics: &'a mut Diagnostics,
	pub vec_pool: &'a VecPool,
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
		ty_instances: &'a mut TyInstances,
		bound_impls: &'a mut BoundImpls,
		scope_context: &'a mut ScopeContext,
		ty_graph: &'a mut TyGraph,
		modules: &'a mut Modules,
		diagnostics: &'a mut Diagnostics,
		vec_pool: &'a VecPool,
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
			ty_instances,
			bound_impls,
			scope_context,
			ty_graph,
			modules,
			diagnostics,
			vec_pool,
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
			&mut $self.ty_instances,
			&mut $self.bound_impls,
			&mut $self.scope_context,
			&mut $self.ty_graph,
			&mut $self.modules,
			&mut $self.diagnostics,
			&$self.vec_pool,
			&$self.builtin_types,
			&$self.sources,
			&$self.ast_data,
		)
	};
}

pub struct BuiltinBuilder<'a> {
	pub builtin_source: &'a mut BuiltinSource,
	pub sources: &'a mut Sources,
	pub types: &'a mut Types,
	pub ty_lists: &'a mut TyLists,
	pub ty_instances: &'a mut TyInstances,
	pub funcs: &'a mut Funcs,
	pub func_lists: &'a mut FuncLists,
	pub ty_comps: &'a mut TyComps,
	pub bound_impls: &'a mut BoundImpls,
	pub ty_graph: &'a mut TyGraph,
	pub func_instances: &'a mut FuncInstances,
	pub globals: &'a mut Globals,
	pub global_data: &'a mut GlobalData,
	pub vec_pool: &'a VecPool,
	pub builtin_types: &'a BuiltinTypes,
}

impl<'a> BuiltinBuilder<'a> {
	pub fn new(
		builtin_source: &'a mut BuiltinSource,
		sources: &'a mut Sources,
		types: &'a mut Types,
		ty_lists: &'a mut TyLists,
		ty_instances: &'a mut TyInstances,
		funcs: &'a mut Funcs,
		func_lists: &'a mut FuncLists,
		ty_comps: &'a mut TyComps,
		bound_impls: &'a mut BoundImpls,
		ty_graph: &'a mut TyGraph,
		func_instances: &'a mut FuncInstances,
		globals: &'a mut Globals,
		global_data: &'a mut GlobalData,
		vec_pool: &'a VecPool,
		builtin_types: &'a BuiltinTypes,
	) -> Self {
		Self {
			builtin_source,
			sources,
			types,
			ty_lists,
			ty_instances,
			funcs,
			func_lists,
			ty_comps,
			bound_impls,
			ty_graph,
			func_instances,
			globals,
			global_data,
			vec_pool,
			builtin_types,
		}
	}
}

#[macro_export]
macro_rules! builtin_builder {
	($self:expr) => {
		BuiltinBuilder::new(
			&mut $self.builtin_source,
			&mut $self.sources,
			&mut $self.types,
			&mut $self.ty_lists,
			&mut $self.ty_instances,
			&mut $self.funcs,
			&mut $self.func_lists,
			&mut $self.ty_comps,
			&mut $self.bound_impls,
			&mut $self.ty_graph,
			&mut $self.func_instances,
			&mut $self.globals,
			&mut $self.global_data,
			&$self.vec_pool,
			&$self.builtin_types,
		)
	};
}

