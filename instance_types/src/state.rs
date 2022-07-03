//! This file is generated, do not edit!
use crate::*;
use typec_types::*;
use lexer::*;

pub struct MirDisplay<'a> {
	pub func_ctx: &'a FuncCtx,
	pub ty_comps: &'a TyComps,
	pub types: &'a Types,
	pub ty_lists: &'a TyLists,
	pub sources: &'a Sources,
}

impl<'a> MirDisplay<'a> {
	pub fn new(
		func_ctx: &'a FuncCtx,
		ty_comps: &'a TyComps,
		types: &'a Types,
		ty_lists: &'a TyLists,
		sources: &'a Sources,
	) -> Self {
		Self {
			func_ctx,
			ty_comps,
			types,
			ty_lists,
			sources,
		}
	}
}

#[macro_export]
macro_rules! mir_display {
	($self:expr, $func_ctx:expr) => {
		MirDisplay::new(
			&$func_ctx,
			&$self.ty_comps,
			&$self.types,
			&$self.ty_lists,
			&$self.sources,
		)
	};
}

