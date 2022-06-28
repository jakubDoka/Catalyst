//! This file is generated, do not edit!
use crate::*;
use instance_types::*;
use storage::*;
use typec_types::*;

pub struct CirBuilder<'a> {
    pub builder: FunctionBuilder<'a>,
    pub cir_builder_context: &'a mut CirBuilderContext,
    pub signatures: &'a mut Signatures,
    pub vec_pool: &'a VecPool,
    pub isa: &'a dyn TargetIsa,
    pub ty_comps: &'a TyComps,
    pub funcs: &'a Funcs,
    pub reprs: &'a Reprs,
    pub types: &'a Types,
    pub builtin_types: &'a BuiltinTypes,
    pub ty_lists: &'a TyLists,
    pub func_ctx: &'a FuncCtx,
    pub sources: &'a Sources,
}

impl<'a> CirBuilder<'a> {
    pub fn new(
        builder: FunctionBuilder<'a>,
        cir_builder_context: &'a mut CirBuilderContext,
        signatures: &'a mut Signatures,
        vec_pool: &'a VecPool,
        isa: &'a dyn TargetIsa,
        ty_comps: &'a TyComps,
        funcs: &'a Funcs,
        reprs: &'a Reprs,
        types: &'a Types,
        builtin_types: &'a BuiltinTypes,
        ty_lists: &'a TyLists,
        func_ctx: &'a FuncCtx,
        sources: &'a Sources,
    ) -> Self {
        Self {
            builder,
            cir_builder_context,
            signatures,
            vec_pool,
            isa,
            ty_comps,
            funcs,
            reprs,
            types,
            builtin_types,
            ty_lists,
            func_ctx,
            sources,
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
            &$self.vec_pool,
            &$isa,
            &$self.ty_comps,
            &$self.funcs,
            &$self.reprs,
            &$self.types,
            &$self.builtin_types,
            &$self.ty_lists,
            &$self.func_ctx,
            &$self.sources,
        )
    };
}
