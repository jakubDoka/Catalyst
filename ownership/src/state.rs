//! This file is generated, do not edit!
use errors::*;
use lexer::*;
use ownership_types::*;
use typec_types::*;

pub struct OwnershipSolver<'a> {
    pub o_ctx: &'a mut OwnershipContext,
    pub diagnostics: &'a mut Diagnostics,
    pub tir_data: &'a TirData,
    pub funcs: &'a Funcs,
    pub types: &'a Types,
    pub ty_comps: &'a TyComps,
    pub ty_lists: &'a TyLists,
    pub sources: &'a Sources,
}

impl<'a> OwnershipSolver<'a> {
    pub fn new(
        o_ctx: &'a mut OwnershipContext,
        diagnostics: &'a mut Diagnostics,
        tir_data: &'a TirData,
        funcs: &'a Funcs,
        types: &'a Types,
        ty_comps: &'a TyComps,
        ty_lists: &'a TyLists,
        sources: &'a Sources,
    ) -> Self {
        Self {
            o_ctx,
            diagnostics,
            tir_data,
            funcs,
            types,
            ty_comps,
            ty_lists,
            sources,
        }
    }
}

#[macro_export]
macro_rules! ownership_solver {
    ($self:expr) => {
        OwnershipSolver::new(
            &mut $self.o_ctx,
            &mut $self.diagnostics,
            &$self.tir_data,
            &$self.funcs,
            &$self.types,
            &$self.ty_comps,
            &$self.ty_lists,
            &$self.sources,
        )
    };
}
