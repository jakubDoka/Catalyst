//! This file is generated, do not edit!
use crate::*;
use typec_types::*;
use instance_types::*;

pub struct CirBuilder<'a> {
	pub builder: FunctionBuilder<'a>,
	pub cir_builder_context: &'a mut CirBuilderContext,
	pub signatures: &'a mut Signatures,
	pub isa: &'a dyn TargetIsa,
	pub funcs: &'a Funcs,
	pub reprs: &'a Reprs,
	pub types: &'a Types,
	pub builtin_types: &'a BuiltinTypes,
	pub ty_lists: &'a TyLists,
	pub func_ctx: &'a FuncCtx,
	pub sources: &'a Sources,
	pub func_meta: &'a FuncMeta,
}

impl<'a> CirBuilder<'a> {
	pub fn new(
		builder: FunctionBuilder<'a>,
		cir_builder_context: &'a mut CirBuilderContext,
		signatures: &'a mut Signatures,
		isa: &'a dyn TargetIsa,
		funcs: &'a Funcs,
		reprs: &'a Reprs,
		types: &'a Types,
		builtin_types: &'a BuiltinTypes,
		ty_lists: &'a TyLists,
		func_ctx: &'a FuncCtx,
		sources: &'a Sources,
		func_meta: &'a FuncMeta,
	) -> Self {
		Self {
			builder,
			cir_builder_context,
			signatures,
			isa,
			funcs,
			reprs,
			types,
			builtin_types,
			ty_lists,
			func_ctx,
			sources,
			func_meta,
		}
	}
}

#[macro_export]
macro_rules! cir_builder {
	($self:expr, $builder:expr, $isa:expr) => {
		CirBuilder::new(
			$builder,
			&mut $self.cir_builder_context,
			&mut $self.signatures,
			&$isa,
			&$self.funcs,
			&$self.reprs,
			&$self.types,
			&$self.builtin_types,
			&$self.ty_lists,
			&$self.func_ctx,
			&$self.sources,
			&$self.func_meta,
		)
	};
}

