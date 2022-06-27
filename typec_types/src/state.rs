//! This file is generated, do not edit!
use crate::*;
use lexer::*;
use errors::*;
use storage::*;

pub struct TyFactory<'a> {
	pub types: &'a mut Types,
	pub ty_lists: &'a mut TyLists,
	pub ty_instances: &'a mut TyInstances,
	pub bound_impls: &'a mut BoundImpls,
	pub ty_graph: &'a mut TyGraph,
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
		ty_graph: &'a mut TyGraph,
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
			ty_graph,
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
			&mut $self.ty_graph,
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
	pub ty_comps: &'a TyComps,
	pub sources: &'a Sources,
}

impl<'a> BoundChecker<'a> {
	pub fn new(
		bound_impls: &'a mut BoundImpls,
		diagnostics: &'a mut Diagnostics,
		types: &'a mut Types,
		vec_pool: &'a VecPool,
		ty_lists: &'a TyLists,
		builtin_types: &'a BuiltinTypes,
		ty_comps: &'a TyComps,
		sources: &'a Sources,
	) -> Self {
		Self {
			bound_impls,
			diagnostics,
			types,
			vec_pool,
			ty_lists,
			builtin_types,
			ty_comps,
			sources,
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
			&$self.ty_comps,
			&$self.sources,
		)
	};
}

pub struct TyDisplay<'a> {
	pub ty: Ty,
	pub ty_lists: &'a TyLists,
	pub ty_comps: &'a TyComps,
	pub types: &'a Types,
	pub sources: &'a Sources,
}

impl<'a> TyDisplay<'a> {
	pub fn new(
		ty: Ty,
		ty_lists: &'a TyLists,
		ty_comps: &'a TyComps,
		types: &'a Types,
		sources: &'a Sources,
	) -> Self {
		Self {
			ty,
			ty_lists,
			ty_comps,
			types,
			sources,
		}
	}
}

#[macro_export]
macro_rules! ty_display {
	($self:expr, $ty:expr) => {
		TyDisplay::new(
			$ty,
			&$self.ty_lists,
			&$self.ty_comps,
			&$self.types,
			&$self.sources,
		)
	};
}

pub struct SigDisplay<'a> {
	pub sig: Sig,
	pub ty_lists: &'a TyLists,
	pub ty_comps: &'a TyComps,
	pub types: &'a Types,
	pub sources: &'a Sources,
}

impl<'a> SigDisplay<'a> {
	pub fn new(
		sig: Sig,
		ty_lists: &'a TyLists,
		ty_comps: &'a TyComps,
		types: &'a Types,
		sources: &'a Sources,
	) -> Self {
		Self {
			sig,
			ty_lists,
			ty_comps,
			types,
			sources,
		}
	}
}

#[macro_export]
macro_rules! sig_display {
	($self:expr, $sig:expr) => {
		SigDisplay::new(
			$sig,
			&$self.ty_lists,
			&$self.ty_comps,
			&$self.types,
			&$self.sources,
		)
	};
}

pub struct MissingBoundTreeDisplay<'a> {
	pub missing_bound_tree: &'a MissingBoundTree,
	pub ty_lists: &'a TyLists,
	pub ty_comps: &'a TyComps,
	pub types: &'a Types,
	pub sources: &'a Sources,
}

impl<'a> MissingBoundTreeDisplay<'a> {
	pub fn new(
		missing_bound_tree: &'a MissingBoundTree,
		ty_lists: &'a TyLists,
		ty_comps: &'a TyComps,
		types: &'a Types,
		sources: &'a Sources,
	) -> Self {
		Self {
			missing_bound_tree,
			ty_lists,
			ty_comps,
			types,
			sources,
		}
	}
}

#[macro_export]
macro_rules! missing_bound_tree_display {
	($self:expr, $missing_bound_tree:expr) => {
		MissingBoundTreeDisplay::new(
			&$missing_bound_tree,
			&$self.ty_lists,
			&$self.ty_comps,
			&$self.types,
			&$self.sources,
		)
	};
}

