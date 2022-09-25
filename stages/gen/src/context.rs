use cranelift_codegen::{
    entity::EntityRef,
    ir::{
        self, types, AbiParam, ExtFuncData, ExternalName, InstBuilder, Type, UserExternalNameRef,
    },
    isa::CallConv,
    packed_option::PackedOption,
    Context, MachReloc,
};
use cranelift_frontend::FunctionBuilder;
use mir_t::*;
use storage::*;
use typec_t::*;

use crate::*;

pub type Offset = u32;

pub struct GeneratorCtx {
    // persistent
    pub compiled_funcs: OrderedMap<VRef<str>, CompiledFunc>,
    pub compile_requests: Vec<CompileRequest>,
    pub local_ty_slices: BumpMap<VRef<Ty>>,

    pub layouts: ShadowMap<Ty, Maybe<Layout>>,
    pub offsets: BumpMap<Offset>,

    // temp
    pub blocks: ShadowMap<BlockMir, Maybe<GenBlock>>,
    pub values: ShadowMap<ValueMir, PackedOption<ir::Value>>,
    pub func_imports: Map<VRef<str>, ir::FuncRef>,
    pub crane: Context,
}

impl Default for GeneratorCtx {
    fn default() -> Self {
        Self {
            compiled_funcs: Default::default(),
            compile_requests: Default::default(),
            local_ty_slices: Default::default(),
            layouts: Default::default(),
            offsets: Default::default(),
            blocks: Default::default(),
            values: Default::default(),
            func_imports: Default::default(),
            crane: Context::new(), // bruh
        }
    }
}

impl GeneratorCtx {
    pub fn clear(&mut self) {
        self.blocks.clear();
        self.values.clear();
        self.func_imports.clear();
    }
}

#[derive(Clone, Copy)]
pub struct Layout {
    pub size: u32,
    pub offsets: VSlice<Offset>,
    pub align: u8,
    pub repr: Type,
}

impl Layout {
    pub const EMPTY: Self = Self {
        size: 0,
        align: 1,
        offsets: VSlice::empty(),
        repr: types::INVALID,
    };
}

impl Invalid for Layout {
    unsafe fn invalid() -> Self {
        Self {
            size: 0,
            align: 0,
            offsets: VSlice::empty(),
            repr: types::INVALID,
        }
    }

    fn is_invalid(&self) -> bool {
        self.align == 0
    }
}

pub struct CompiledFunc {
    pub func: VRef<Func>,
    pub native_bytecode: Vec<u8>,
    pub native_alignment: u64,
    pub target_bytecode: Option<Vec<u8>>,
    pub target_alignment: Option<u64>,
    pub relocs: Vec<MachReloc>,
}

pub struct CompileRequest {
    pub id: VRef<str>,
    pub func: VRef<Func>,
    pub params: VRefSlice<Ty>,
}

#[derive(Clone, Copy)]
pub struct GenBlock {
    pub id: ir::Block,
    pub visit_count: u32,
}

impl Invalid for GenBlock {
    unsafe fn invalid() -> Self {
        Self {
            id: ir::Block::from_u32(0),
            visit_count: u32::MAX,
        }
    }

    fn is_invalid(&self) -> bool {
        self.visit_count == u32::MAX
    }
}
