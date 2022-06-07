//! This file is generated, do not edit!
use typec_types::*;
use ownership_types::*;
use lexer::*;

pub struct OwnershipSolver<'a> {
	pub o_ctx: &'a mut OwnershipContext,
	pub tir_data: &'a mut TirData,
	pub ty_lists: &'a TyLists,
	pub funcs: &'a Funcs,
	pub sources: &'a Sources,
	pub bound_impls: &'a BoundImpls,
	pub builtin_types: &'a BuiltinTypes,
	pub ty_comps: &'a TyComps,
	pub types: &'a Types,
}

impl<'a> OwnershipSolver<'a> {
	pub fn new(
		o_ctx: &'a mut OwnershipContext,
		tir_data: &'a mut TirData,
		ty_lists: &'a TyLists,
		funcs: &'a Funcs,
		sources: &'a Sources,
		bound_impls: &'a BoundImpls,
		builtin_types: &'a BuiltinTypes,
		ty_comps: &'a TyComps,
		types: &'a Types,
	) -> Self {
		Self {
			o_ctx,
			tir_data,
			ty_lists,
			funcs,
			sources,
			bound_impls,
			builtin_types,
			ty_comps,
			types,
		}
	}
}

#[macro_export]
macro_rules! ownership_solver {
	($self:expr) => {
		OwnershipSolver::new(
			&mut $self.o_ctx,
			&mut $self.tir_data,
			&$self.ty_lists,
			&$self.funcs,
			&$self.sources,
			&$self.bound_impls,
			&$self.builtin_types,
			&$self.ty_comps,
			&$self.types,
		)
	};
}

