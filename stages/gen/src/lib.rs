#![feature(let_else)]
#![feature(drain_filter)]
#![feature(default_free_fn)]
#![feature(let_chains)]

mod context;
mod generate;
mod jit_context;
mod object_context;
mod state_gen;

pub use {
    crate::object_context::{ObjectContext, ObjectCreationError},
    context::{
        CodeSaveError, CompileRequest, CompileRequests, CompiledFunc, Gen, GenBlock, GenBuilder,
        GenFuncConstant, GenItemName, GenLayouts, GenReloc, GenResources, GenValue, Isa,
        IsaCreationError, Layout, Offset,
    },
    jit_context::{JitContext, JitRelocError, TokenMacroOwnedSpec},
    state_gen::Generator,
};

pub const ENTRY_POINT_NAME: &str = "__catalyst_main__";
