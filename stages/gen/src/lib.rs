#![feature(drain_filter)]
#![feature(default_free_fn)]
#![feature(let_chains)]
#![feature(iter_collect_into)]
#![feature(trivial_bounds)]
#![feature(int_roundings)]

mod context;
mod generate;
mod interpreter;
mod jit;
mod native;
mod state_gen;

pub use {
    context::{
        layout::{GenLayouts, Layout, Offset},
        CodeSaveError, CompileRequest, CompileRequestChild, CompileRequests, CompiledFunc,
        CompiledFuncInner, CompiledFuncRef, ComputedValue, Gen, GenBase, GenBlock, GenBuilder,
        GenFuncConstant, GenItemName, GenReloc, GenResources, GenValue, Isa, IsaCreationError,
    },
    generate::function_loading::abi::{PassMode, PassSignature},
    jit::{JitContext, JitRelocError},
    native::{ObjectContext, ObjectCreationError, ObjectRelocationError},
    state_gen::Generator,
};

pub const ENTRY_POINT_NAME: &str = "main";
