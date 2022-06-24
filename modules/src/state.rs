//! This file is generated, do not edit!
use crate::*;
use errors::*;
use lexer::*;
use module_types::*;

pub struct ModuleBuilder<'a> {
    pub sources: &'a mut Sources,
    pub modules: &'a mut Modules,
    pub units: &'a mut Units,
    pub loader_context: &'a mut LoaderContext,
    pub module_map: &'a mut ModuleMap,
    pub diagnostics: &'a mut Diagnostics,
}

impl<'a> ModuleBuilder<'a> {
    pub fn new(
        sources: &'a mut Sources,
        modules: &'a mut Modules,
        units: &'a mut Units,
        loader_context: &'a mut LoaderContext,
        module_map: &'a mut ModuleMap,
        diagnostics: &'a mut Diagnostics,
    ) -> Self {
        Self {
            sources,
            modules,
            units,
            loader_context,
            module_map,
            diagnostics,
        }
    }
}

#[macro_export]
macro_rules! module_builder {
    ($self:expr) => {
        ModuleBuilder::new(
            &mut $self.sources,
            &mut $self.modules,
            &mut $self.units,
            &mut $self.loader_context,
            &mut $self.module_map,
            &mut $self.diagnostics,
        )
    };
}

pub struct UnitBuilder<'a> {
    pub sources: &'a mut Sources,
    pub units: &'a mut Units,
    pub loader_context: &'a mut LoaderContext,
    pub diagnostics: &'a mut Diagnostics,
}

impl<'a> UnitBuilder<'a> {
    pub fn new(
        sources: &'a mut Sources,
        units: &'a mut Units,
        loader_context: &'a mut LoaderContext,
        diagnostics: &'a mut Diagnostics,
    ) -> Self {
        Self {
            sources,
            units,
            loader_context,
            diagnostics,
        }
    }
}

#[macro_export]
macro_rules! unit_builder {
    ($self:expr) => {
        UnitBuilder::new(
            &mut $self.sources,
            &mut $self.units,
            &mut $self.loader_context,
            &mut $self.diagnostics,
        )
    };
}
