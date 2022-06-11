//! This file is generated, do not edit!
use crate::*;
use lexer::*;

pub struct TyFactory<'a> {
    pub types: &'a mut Types,
    pub ty_lists: &'a mut TyLists,
    pub ty_instances: &'a mut TyInstances,
    pub bound_impls: &'a mut BoundImpls,
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
        ty_comps: &'a TyComps,
        sources: &'a Sources,
        builtin_types: &'a BuiltinTypes,
    ) -> Self {
        Self {
            types,
            ty_lists,
            ty_instances,
            bound_impls,
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
            &$self.ty_comps,
            &$self.sources,
            &$self.builtin_types,
        )
    };
}
