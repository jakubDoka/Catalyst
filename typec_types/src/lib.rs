#![feature(let_else)]
#![feature(result_into_ok_or_err)]
#![feature(let_chains)]

pub mod error;
pub mod func;
pub mod global;
pub mod graph;
pub mod jit;
pub mod state;
pub mod tir;
pub mod ty;
pub mod ty_factory;
pub mod bound_checker;

pub use error::{MissingBoundTree, TyError};
pub use func::{
    Func, FuncEnt, FuncFlags, FuncInstances, FuncKind, FuncList, FuncMeta, Funcs, Initializers,
    Macros, Sig, ToCompile, ToLink,
};
pub use global::{Global, GlobalBytes, GlobalData, GlobalEnt, GlobalMap, Globals};
pub use graph::Graph;
pub use scope_context::ScopeContext;
pub use state::{TyFactory, BoundChecker};
pub use tir::{
    Tir, TirData, TirDisplay, TirEnt, TirFlags, TirKind, TirList, TirPatternGraph, TirPatternMeta,
    TirStack,
};
pub use ty::{
    BoundImpl, BoundImpls, BuiltinTypes, FuncLists, Ty, TyComp, TyCompEnt, TyCompList, TyComps,
    TyDisplay, TyEnt, TyFlags, TyGraph, TyInstances, TyKind, TyList, TyLists, TypeBase, Types,
};
pub use ty_factory::{pointer_of, infer_parameters, prepare_params};

#[macro_export]
macro_rules! ty_display {
    ($self:expr, $ty:expr) => {
        TyDisplay::new(&$self.types, &$self.ty_lists, &$self.sources, $ty)
    };
}

#[macro_export]
macro_rules! sig_display {
    ($self:expr, $sig:expr) => {
        SignatureDisplay::new($self.sources, $self.ty_lists, $self.types, $sig)
    };
}

#[macro_export]
macro_rules! tir_display {
    ($self:expr, $tir:expr) => {
        $crate::tir::TirDisplay::new(
            $self.types,
            $self.ty_lists,
            $self.ty_comps,
            $self.sources,
            $self.data,
            $tir,
        )
    };
}

pub mod scope_context {
    use ast::*;
    use storage::*;

    use crate::*;

    pub struct ScopeContext {
        pub bound_funcs: Vec<Func>,
        pub tags: Vec<Ast>,
        /// (implementor, bound, impl block)
        pub bounds_to_verify: Vec<(Ty, Ty, Ast)>,
        pub type_ast: SecondaryMap<Ty, Ast>,
        pub func_ast: SecondaryMap<Func, Ast>,
        pub global_ast: SecondaryMap<Global, Ast>,
        pub used_types: Vec<Ty>,
        pub used_types_set: EntitySet<Ty>,
        pub loops: Vec<(Tir, ID)>,
    }

    impl ScopeContext {
        pub fn new() -> Self {
            Self {
                bound_funcs: Vec::new(),
                tags: Vec::new(),
                bounds_to_verify: Vec::new(),
                type_ast: SecondaryMap::new(),
                func_ast: SecondaryMap::new(),
                global_ast: SecondaryMap::new(),
                used_types: Vec::new(),
                used_types_set: EntitySet::new(),
                loops: Vec::new(),
            }
        }

        pub fn use_type(&mut self, ty: Ty, types: &Types) {
            if self.used_types_set.insert(ty) && types[ty].flags.contains(TyFlags::GENERIC) {
                self.used_types.push(ty);
            }
        }
    }
}
