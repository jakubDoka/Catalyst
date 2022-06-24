//! This file is generated, do not edit!
use crate::*;
use ast::*;
use incr::*;
use storage::*;

pub struct SourceLoader<'a> {
    pub units: &'a mut Units,
    pub modules: &'a mut Modules,
    pub sources: &'a mut Sources,
    pub loader_context: &'a mut LoaderContext,
    pub diagnostics: &'a mut Diagnostics,
    pub module_map: &'a mut ModuleMap,
    pub vec_pool: &'a VecPool,
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
        vec_pool: &'a VecPool,
        subcommand: &'a Subcommand,
    ) -> Self {
        Self {
            units,
            modules,
            sources,
            loader_context,
            diagnostics,
            module_map,
            vec_pool,
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
            &$self.vec_pool,
            &$self.subcommand,
        )
    };
}

pub struct MainScopeBuilder<'a> {
    pub diagnostics: &'a mut Diagnostics,
    pub scope: &'a mut Scope,
    pub vec_pool: &'a VecPool,
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
        vec_pool: &'a VecPool,
        modules: &'a Modules,
        builtin_source: &'a BuiltinSource,
        sources: &'a Sources,
        ast_data: &'a AstData,
        module_map: &'a ModuleMap,
    ) -> Self {
        Self {
            diagnostics,
            scope,
            vec_pool,
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
            &$self.vec_pool,
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
    pub types: &'a mut Types,
    pub ty_lists: &'a mut TyLists,
    pub ty_comps: &'a mut TyComps,
    pub func_lists: &'a mut FuncLists,
    pub ty_instances: &'a mut TyInstances,
    pub bound_impls: &'a mut BoundImpls,
    pub scope_context: &'a mut ScopeContext,
    pub ty_graph: &'a mut TyGraph,
    pub ast_data: &'a mut AstData,
    pub ast_temp: &'a mut AstTemp,
    pub scope: &'a mut Scope,
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
    pub globals: &'a mut Globals,
    pub global_map: &'a mut GlobalMap,
    pub global_data: &'a mut GlobalData,
    pub initializers: &'a mut Initializers,
    pub o_ctx: &'a mut OwnershipContext,
    pub vec_pool: &'a VecPool,
    pub host_isa: &'a Box<dyn TargetIsa>,
    pub sources: &'a Sources,
    pub builtin_types: &'a BuiltinTypes,
    pub units: &'a Units,
    pub module_map: &'a ModuleMap,
}

impl<'a> MainTirBuilder<'a> {
    pub fn new(
        diagnostics: &'a mut Diagnostics,
        modules: &'a mut Modules,
        funcs: &'a mut Funcs,
        tir_data: &'a mut TirData,
        types: &'a mut Types,
        ty_lists: &'a mut TyLists,
        ty_comps: &'a mut TyComps,
        func_lists: &'a mut FuncLists,
        ty_instances: &'a mut TyInstances,
        bound_impls: &'a mut BoundImpls,
        scope_context: &'a mut ScopeContext,
        ty_graph: &'a mut TyGraph,
        ast_data: &'a mut AstData,
        ast_temp: &'a mut AstTemp,
        scope: &'a mut Scope,
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
        globals: &'a mut Globals,
        global_map: &'a mut GlobalMap,
        global_data: &'a mut GlobalData,
        initializers: &'a mut Initializers,
        o_ctx: &'a mut OwnershipContext,
        vec_pool: &'a VecPool,
        host_isa: &'a Box<dyn TargetIsa>,
        sources: &'a Sources,
        builtin_types: &'a BuiltinTypes,
        units: &'a Units,
        module_map: &'a ModuleMap,
    ) -> Self {
        Self {
            diagnostics,
            modules,
            funcs,
            tir_data,
            types,
            ty_lists,
            ty_comps,
            func_lists,
            ty_instances,
            bound_impls,
            scope_context,
            ty_graph,
            ast_data,
            ast_temp,
            scope,
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
            globals,
            global_map,
            global_data,
            initializers,
            o_ctx,
            vec_pool,
            host_isa,
            sources,
            builtin_types,
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
            &mut $self.types,
            &mut $self.ty_lists,
            &mut $self.ty_comps,
            &mut $self.func_lists,
            &mut $self.ty_instances,
            &mut $self.bound_impls,
            &mut $self.scope_context,
            &mut $self.ty_graph,
            &mut $self.ast_data,
            &mut $self.ast_temp,
            &mut $self.scope,
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
            &mut $self.globals,
            &mut $self.global_map,
            &mut $self.global_data,
            &mut $self.initializers,
            &mut $self.o_ctx,
            &$self.vec_pool,
            &$self.host_isa,
            &$self.sources,
            &$self.builtin_types,
            &$self.units,
            &$self.module_map,
        )
    };
}

pub struct Generator<'a> {
    pub diagnostics: &'a mut Diagnostics,
    pub funcs: &'a mut Funcs,
    pub ty_lists: &'a mut TyLists,
    pub reprs: &'a mut Reprs,
    pub types: &'a mut Types,
    pub repr_fields: &'a mut ReprFields,
    pub ty_comps: &'a mut TyComps,
    pub func_lists: &'a mut FuncLists,
    pub bound_impls: &'a mut BoundImpls,
    pub compile_results: &'a mut CompileResults,
    pub ty_instances: &'a mut TyInstances,
    pub context: &'a mut Context,
    pub mir_builder_context: &'a mut MirBuilderContext,
    pub func_ctx: &'a mut FuncCtx,
    pub to_compile: &'a mut ToCompile,
    pub cir_builder_context: &'a mut CirBuilderContext,
    pub signatures: &'a mut Signatures,
    pub generation_context: &'a mut GenerationContext,
    pub func_instances: &'a mut FuncInstances,
    pub global_map: &'a mut GlobalMap,
    pub vec_pool: &'a VecPool,
    pub incr_modules: &'a mut IncrModules,
    pub incr_funcs: &'a mut IncrFuncs,
    pub isa: &'a dyn TargetIsa,
    pub globals: &'a Globals,
    pub builtin_types: &'a BuiltinTypes,
    pub sources: &'a Sources,
    pub modules: &'a Modules,
}

impl<'a> Generator<'a> {
    pub fn new(
        diagnostics: &'a mut Diagnostics,
        funcs: &'a mut Funcs,
        ty_lists: &'a mut TyLists,
        reprs: &'a mut Reprs,
        types: &'a mut Types,
        repr_fields: &'a mut ReprFields,
        ty_comps: &'a mut TyComps,
        func_lists: &'a mut FuncLists,
        bound_impls: &'a mut BoundImpls,
        compile_results: &'a mut CompileResults,
        ty_instances: &'a mut TyInstances,
        context: &'a mut Context,
        mir_builder_context: &'a mut MirBuilderContext,
        func_ctx: &'a mut FuncCtx,
        to_compile: &'a mut ToCompile,
        cir_builder_context: &'a mut CirBuilderContext,
        signatures: &'a mut Signatures,
        generation_context: &'a mut GenerationContext,
        func_instances: &'a mut FuncInstances,
        global_map: &'a mut GlobalMap,
        vec_pool: &'a VecPool,
        incr_modules: &'a mut IncrModules,
        incr_funcs: &'a mut IncrFuncs,
        isa: &'a dyn TargetIsa,
        globals: &'a Globals,
        builtin_types: &'a BuiltinTypes,
        sources: &'a Sources,
        modules: &'a Modules,
    ) -> Self {
        Self {
            diagnostics,
            funcs,
            ty_lists,
            reprs,
            types,
            repr_fields,
            ty_comps,
            func_lists,
            bound_impls,
            compile_results,
            ty_instances,
            context,
            mir_builder_context,
            func_ctx,
            to_compile,
            cir_builder_context,
            signatures,
            generation_context,
            func_instances,
            global_map,
            vec_pool,
            incr_modules,
            incr_funcs,
            isa,
            globals,
            builtin_types,
            sources,
            modules,
        }
    }
}

#[macro_export]
macro_rules! generator {
    ($self:expr, $incr_modules:expr, $incr_funcs:expr, $isa:expr) => {
        Generator::new(
            &mut $self.diagnostics,
            &mut $self.funcs,
            &mut $self.ty_lists,
            &mut $self.reprs,
            &mut $self.types,
            &mut $self.repr_fields,
            &mut $self.ty_comps,
            &mut $self.func_lists,
            &mut $self.bound_impls,
            &mut $self.compile_results,
            &mut $self.ty_instances,
            &mut $self.context,
            &mut $self.mir_builder_context,
            &mut $self.func_ctx,
            &mut $self.to_compile,
            &mut $self.cir_builder_context,
            &mut $self.signatures,
            &mut $self.generation_context,
            &mut $self.func_instances,
            &mut $self.global_map,
            &$self.vec_pool,
            &mut $incr_modules,
            &mut $incr_funcs,
            &$isa,
            &$self.globals,
            &$self.builtin_types,
            &$self.sources,
            &$self.modules,
        )
    };
}

pub struct DeadCodeElim<'a> {
    pub funcs: &'a Funcs,
    pub compile_results: &'a CompileResults,
}

impl<'a> DeadCodeElim<'a> {
    pub fn new(funcs: &'a Funcs, compile_results: &'a CompileResults) -> Self {
        Self {
            funcs,
            compile_results,
        }
    }
}

#[macro_export]
macro_rules! dead_code_elim {
    ($self:expr) => {
        DeadCodeElim::new(&$self.funcs, &$self.compile_results)
    };
}

pub struct Logger<'a> {
    pub diagnostics: &'a Diagnostics,
    pub ty_lists: &'a TyLists,
    pub types: &'a Types,
    pub sources: &'a Sources,
    pub ty_comps: &'a TyComps,
    pub units: &'a Units,
}

impl<'a> Logger<'a> {
    pub fn new(
        diagnostics: &'a Diagnostics,
        ty_lists: &'a TyLists,
        types: &'a Types,
        sources: &'a Sources,
        ty_comps: &'a TyComps,
        units: &'a Units,
    ) -> Self {
        Self {
            diagnostics,
            ty_lists,
            types,
            sources,
            ty_comps,
            units,
        }
    }
}

#[macro_export]
macro_rules! logger {
    ($self:expr) => {
        Logger::new(
            &$self.diagnostics,
            &$self.ty_lists,
            &$self.types,
            &$self.sources,
            &$self.ty_comps,
            &$self.units,
        )
    };
}
