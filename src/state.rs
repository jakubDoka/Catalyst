//! This file is generated, do not edit!
use crate::*;
use ast::*;
use incr::*;

pub struct SourceLoader<'a> {
	pub units: &'a mut Units,
	pub modules: &'a mut Modules,
	pub sources: &'a mut Sources,
	pub loader_context: &'a mut LoaderContext,
	pub diagnostics: &'a mut Diagnostics,
	pub module_map: &'a mut ModuleMap,
	pub subcommand: &'a Subcommand,
}

impl<'a> SourceLoader<'a> {
	pub fn new(
		units: &'a mut Units,
		modules: &'a mut Modules,
		sources: &'a mut Sources,
		loader_context: &'a mut LoaderContext,
		diagnostics: &'a mut Diagnostics,
		module_map: &'a mut ModuleMap,
		subcommand: &'a Subcommand,
	) -> Self {
		Self {
			units,
			modules,
			sources,
			loader_context,
			diagnostics,
			module_map,
			subcommand,
		}
	}
}

#[macro_export]
macro_rules! source_loader {
	($self:expr) => {
		SourceLoader::new(
			&mut $self.units,
			&mut $self.modules,
			&mut $self.sources,
			&mut $self.loader_context,
			&mut $self.diagnostics,
			&mut $self.module_map,
			&$self.subcommand,
		)
	};
}

pub struct MainScopeBuilder<'a> {
	pub diagnostics: &'a mut Diagnostics,
	pub scope: &'a mut Scope,
	pub modules: &'a Modules,
	pub builtin_source: &'a BuiltinSource,
	pub sources: &'a Sources,
	pub ast_data: &'a AstData,
	pub module_map: &'a ModuleMap,
}

impl<'a> MainScopeBuilder<'a> {
	pub fn new(
		diagnostics: &'a mut Diagnostics,
		scope: &'a mut Scope,
		modules: &'a Modules,
		builtin_source: &'a BuiltinSource,
		sources: &'a Sources,
		ast_data: &'a AstData,
		module_map: &'a ModuleMap,
	) -> Self {
		Self {
			diagnostics,
			scope,
			modules,
			builtin_source,
			sources,
			ast_data,
			module_map,
		}
	}
}

#[macro_export]
macro_rules! main_scope_builder {
	($self:expr) => {
		MainScopeBuilder::new(
			&mut $self.diagnostics,
			&mut $self.scope,
			&$self.modules,
			&$self.builtin_source,
			&$self.sources,
			&$self.ast_data,
			&$self.module_map,
		)
	};
}

pub struct MainTirBuilder<'a> {
	pub diagnostics: &'a mut Diagnostics,
	pub modules: &'a mut Modules,
	pub funcs: &'a mut Funcs,
	pub tir_data: &'a mut TirData,
	pub func_bodies: &'a mut FuncBodies,
	pub types: &'a mut Types,
	pub ty_lists: &'a mut TyLists,
	pub ty_comps: &'a mut TyComps,
	pub ty_comp_lookup: &'a mut TyCompLookup,
	pub func_lists: &'a mut FuncLists,
	pub instances: &'a mut Instances,
	pub bound_impls: &'a mut BoundImpls,
	pub scope_context: &'a mut ScopeContext,
	pub ty_graph: &'a mut TyGraph,
	pub ast_data: &'a mut AstData,
	pub ast_temp: &'a mut AstTemp,
	pub scope: &'a mut Scope,
	pub func_meta: &'a mut FuncMeta,
	pub to_compile: &'a mut ToCompile,
	pub tir_stack: &'a mut TirStack,
	pub tir_pattern_graph: &'a mut TirPatternGraph,
	pub ty_order: &'a mut TyOrder,
	pub reprs: &'a mut Reprs,
	pub builtin_source: &'a mut BuiltinSource,
	pub repr_fields: &'a mut ReprFields,
	pub func_instances: &'a mut FuncInstances,
	pub to_link: &'a mut ToLink,
	pub macros: &'a mut Macros,
	pub host_isa: &'a Box<dyn TargetIsa>,
	pub sources: &'a Sources,
	pub builtin_types: &'a BuiltinTypes,
	pub item_lexicon: &'a ItemLexicon,
	pub units: &'a Units,
	pub module_map: &'a ModuleMap,
}

impl<'a> MainTirBuilder<'a> {
	pub fn new(
		diagnostics: &'a mut Diagnostics,
		modules: &'a mut Modules,
		funcs: &'a mut Funcs,
		tir_data: &'a mut TirData,
		func_bodies: &'a mut FuncBodies,
		types: &'a mut Types,
		ty_lists: &'a mut TyLists,
		ty_comps: &'a mut TyComps,
		ty_comp_lookup: &'a mut TyCompLookup,
		func_lists: &'a mut FuncLists,
		instances: &'a mut Instances,
		bound_impls: &'a mut BoundImpls,
		scope_context: &'a mut ScopeContext,
		ty_graph: &'a mut TyGraph,
		ast_data: &'a mut AstData,
		ast_temp: &'a mut AstTemp,
		scope: &'a mut Scope,
		func_meta: &'a mut FuncMeta,
		to_compile: &'a mut ToCompile,
		tir_stack: &'a mut TirStack,
		tir_pattern_graph: &'a mut TirPatternGraph,
		ty_order: &'a mut TyOrder,
		reprs: &'a mut Reprs,
		builtin_source: &'a mut BuiltinSource,
		repr_fields: &'a mut ReprFields,
		func_instances: &'a mut FuncInstances,
		to_link: &'a mut ToLink,
		macros: &'a mut Macros,
		host_isa: &'a Box<dyn TargetIsa>,
		sources: &'a Sources,
		builtin_types: &'a BuiltinTypes,
		item_lexicon: &'a ItemLexicon,
		units: &'a Units,
		module_map: &'a ModuleMap,
	) -> Self {
		Self {
			diagnostics,
			modules,
			funcs,
			tir_data,
			func_bodies,
			types,
			ty_lists,
			ty_comps,
			ty_comp_lookup,
			func_lists,
			instances,
			bound_impls,
			scope_context,
			ty_graph,
			ast_data,
			ast_temp,
			scope,
			func_meta,
			to_compile,
			tir_stack,
			tir_pattern_graph,
			ty_order,
			reprs,
			builtin_source,
			repr_fields,
			func_instances,
			to_link,
			macros,
			host_isa,
			sources,
			builtin_types,
			item_lexicon,
			units,
			module_map,
		}
	}
}

#[macro_export]
macro_rules! main_tir_builder {
	($self:expr) => {
		MainTirBuilder::new(
			&mut $self.diagnostics,
			&mut $self.modules,
			&mut $self.funcs,
			&mut $self.tir_data,
			&mut $self.func_bodies,
			&mut $self.types,
			&mut $self.ty_lists,
			&mut $self.ty_comps,
			&mut $self.ty_comp_lookup,
			&mut $self.func_lists,
			&mut $self.instances,
			&mut $self.bound_impls,
			&mut $self.scope_context,
			&mut $self.ty_graph,
			&mut $self.ast_data,
			&mut $self.ast_temp,
			&mut $self.scope,
			&mut $self.func_meta,
			&mut $self.to_compile,
			&mut $self.tir_stack,
			&mut $self.tir_pattern_graph,
			&mut $self.ty_order,
			&mut $self.reprs,
			&mut $self.builtin_source,
			&mut $self.repr_fields,
			&mut $self.func_instances,
			&mut $self.to_link,
			&mut $self.macros,
			&$self.host_isa,
			&$self.sources,
			&$self.builtin_types,
			&$self.item_lexicon,
			&$self.units,
			&$self.module_map,
		)
	};
}

pub struct Generator<'a> {
	pub diagnostics: &'a mut Diagnostics,
	pub funcs: &'a mut Funcs,
	pub func_meta: &'a mut FuncMeta,
	pub ty_lists: &'a mut TyLists,
	pub reprs: &'a mut Reprs,
	pub types: &'a mut Types,
	pub repr_fields: &'a mut ReprFields,
	pub ty_comps: &'a mut TyComps,
	pub func_lists: &'a mut FuncLists,
	pub bound_impls: &'a mut BoundImpls,
	pub compile_results: &'a mut CompileResults,
	pub instances: &'a mut Instances,
	pub context: &'a mut Context,
	pub mir_builder_context: &'a mut MirBuilderContext,
	pub func_ctx: &'a mut FuncCtx,
	pub to_compile: &'a mut ToCompile,
	pub cir_builder_context: &'a mut CirBuilderContext,
	pub signatures: &'a mut Signatures,
	pub generation_context: &'a mut GenerationContext,
	pub func_instances: &'a mut FuncInstances,
	pub incr_modules: &'a mut IncrModules,
	pub incr_funcs: &'a mut IncrFuncs,
	pub isa: &'a dyn TargetIsa,
	pub builtin_types: &'a BuiltinTypes,
	pub sources: &'a Sources,
	pub func_bodies: &'a FuncBodies,
}

impl<'a> Generator<'a> {
	pub fn new(
		diagnostics: &'a mut Diagnostics,
		funcs: &'a mut Funcs,
		func_meta: &'a mut FuncMeta,
		ty_lists: &'a mut TyLists,
		reprs: &'a mut Reprs,
		types: &'a mut Types,
		repr_fields: &'a mut ReprFields,
		ty_comps: &'a mut TyComps,
		func_lists: &'a mut FuncLists,
		bound_impls: &'a mut BoundImpls,
		compile_results: &'a mut CompileResults,
		instances: &'a mut Instances,
		context: &'a mut Context,
		mir_builder_context: &'a mut MirBuilderContext,
		func_ctx: &'a mut FuncCtx,
		to_compile: &'a mut ToCompile,
		cir_builder_context: &'a mut CirBuilderContext,
		signatures: &'a mut Signatures,
		generation_context: &'a mut GenerationContext,
		func_instances: &'a mut FuncInstances,
		incr_modules: &'a mut IncrModules,
		incr_funcs: &'a mut IncrFuncs,
		isa: &'a dyn TargetIsa,
		builtin_types: &'a BuiltinTypes,
		sources: &'a Sources,
		func_bodies: &'a FuncBodies,
	) -> Self {
		Self {
			diagnostics,
			funcs,
			func_meta,
			ty_lists,
			reprs,
			types,
			repr_fields,
			ty_comps,
			func_lists,
			bound_impls,
			compile_results,
			instances,
			context,
			mir_builder_context,
			func_ctx,
			to_compile,
			cir_builder_context,
			signatures,
			generation_context,
			func_instances,
			incr_modules,
			incr_funcs,
			isa,
			builtin_types,
			sources,
			func_bodies,
		}
	}
}

#[macro_export]
macro_rules! generator {
	($self:expr, $incr_modules:expr, $incr_funcs:expr, $isa:expr) => {
		Generator::new(
			&mut $self.diagnostics,
			&mut $self.funcs,
			&mut $self.func_meta,
			&mut $self.ty_lists,
			&mut $self.reprs,
			&mut $self.types,
			&mut $self.repr_fields,
			&mut $self.ty_comps,
			&mut $self.func_lists,
			&mut $self.bound_impls,
			&mut $self.compile_results,
			&mut $self.instances,
			&mut $self.context,
			&mut $self.mir_builder_context,
			&mut $self.func_ctx,
			&mut $self.to_compile,
			&mut $self.cir_builder_context,
			&mut $self.signatures,
			&mut $self.generation_context,
			&mut $self.func_instances,
			&mut $incr_modules,
			&mut $incr_funcs,
			&$isa,
			&$self.builtin_types,
			&$self.sources,
			&$self.func_bodies,
		)
	};
}

pub struct DeadCodeElim<'a> {
	pub funcs: &'a Funcs,
	pub compile_results: &'a CompileResults,
}

impl<'a> DeadCodeElim<'a> {
	pub fn new(
		funcs: &'a Funcs,
		compile_results: &'a CompileResults,
	) -> Self {
		Self {
			funcs,
			compile_results,
		}
	}
}

#[macro_export]
macro_rules! dead_code_elim {
	($self:expr) => {
		DeadCodeElim::new(
			&$self.funcs,
			&$self.compile_results,
		)
	};
}

pub struct Logger<'a> {
	pub diagnostics: &'a mut Diagnostics,
	pub ty_lists: &'a TyLists,
	pub types: &'a Types,
	pub sources: &'a Sources,
	pub ty_comps: &'a TyComps,
	pub item_lexicon: &'a ItemLexicon,
	pub units: &'a Units,
}

impl<'a> Logger<'a> {
	pub fn new(
		diagnostics: &'a mut Diagnostics,
		ty_lists: &'a TyLists,
		types: &'a Types,
		sources: &'a Sources,
		ty_comps: &'a TyComps,
		item_lexicon: &'a ItemLexicon,
		units: &'a Units,
	) -> Self {
		Self {
			diagnostics,
			ty_lists,
			types,
			sources,
			ty_comps,
			item_lexicon,
			units,
		}
	}
}

#[macro_export]
macro_rules! logger {
	($self:expr) => {
		Logger::new(
			&mut $self.diagnostics,
			&$self.ty_lists,
			&$self.types,
			&$self.sources,
			&$self.ty_comps,
			&$self.item_lexicon,
			&$self.units,
		)
	};
}

