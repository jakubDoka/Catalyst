//! This file is generated, do not edit!
use crate::*;

pub struct SourceLoader<'a> {
	pub subcommand: &'a Subcommand,
	pub units: &'a mut Units,
	pub modules: &'a mut Modules,
	pub sources: &'a mut Sources,
	pub loader_context: &'a mut LoaderContext,
	pub diagnostics: &'a mut Diagnostics,
	pub module_map: &'a mut ModuleMap,
}

impl<'a> SourceLoader<'a> {
	pub fn new(
		subcommand: &'a Subcommand,
		units: &'a mut Units,
		modules: &'a mut Modules,
		sources: &'a mut Sources,
		loader_context: &'a mut LoaderContext,
		diagnostics: &'a mut Diagnostics,
		module_map: &'a mut ModuleMap,
	) -> Self {
		Self {
			subcommand,
			units,
			modules,
			sources,
			loader_context,
			diagnostics,
			module_map,
		}
	}
}

#[macro_export]
macro_rules! source_loader {
	($self:expr) => {
		SourceLoader::new(
			&$self.subcommand,
			&mut $self.units,
			&mut $self.modules,
			&mut $self.sources,
			&mut $self.loader_context,
			&mut $self.diagnostics,
			&mut $self.module_map,
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

