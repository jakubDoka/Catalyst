#![feature(drain_filter)]
#![feature(default_free_fn)]
#![feature(let_chains)]
#![feature(iter_collect_into)]
#![feature(trivial_bounds)]
#![feature(int_roundings)]
#![feature(allocator_api)]
#![feature(slice_ptr_get)]
#![feature(array_zip)]

mod ctx;
mod generate;
mod interpreter;
mod jit;
mod native;

pub use {
    ctx::{
        layout::{GenLayouts, Layout, Offset},
        CodeSaveError, CompileRequest, CompileRequestChild, CompileRequestView, CompileRequests,
        CompileRequestsShard, CompiledFunc, CompiledFuncInner, CompiledFuncRef, ComputedValue, Gen,
        GenBase, GenBlock, GenBuilder, GenFuncConstant, GenItemName, GenReloc, GenRelocator,
        GenResources, GenValue, Isa, IsaCreationError,
    },
    generate::function_loading::abi::{PassMode, PassSignature},
    generate::Generator,
    interpreter::{IRegister, IValue, Interpreter, InterpreterCtx, InterpreterError, StackFrame},
    jit::{JitContext, JitRelocError, JittedFunc, Jitter, JitterCtx, JitterError},
    native::{ObjectContext, ObjectCreationError, ObjectRelocationError},
};

pub const ENTRY_POINT_NAME: &str = "main";
