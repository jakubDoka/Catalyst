use cranelift_codegen::{
    binemit::{CodeOffset, Reloc},
    ir::{self, types, Type},
    packed_option::PackedOption,
    Context, MachReloc,
};

use mir_t::*;
use storage::*;
use typec_t::*;

#[derive(Default)]
pub struct Gen {
    pub compiled_funcs: OrderedMap<VRef<str>, CompiledFunc>,
}

pub struct CompiledFunc {
    pub func: VRef<Func>,
    pub signature: Signature,
    pub bytecode: Vec<u8>,
    pub alignment: u64,
    pub relocs: Vec<MachReloc>,
}

impl CompiledFunc {
    pub fn new(func: VRef<Func>) -> Self {
        Self {
            func,
            signature: Signature::default(),
            bytecode: Vec::new(),
            alignment: 0,
            relocs: Vec::new(),
        }
    }
}

pub struct GenReloc {
    pub offset: CodeOffset,
    pub kind: Reloc,
    pub name: GenItemName,
    pub addend: i64,
}

pub enum GenItemName {
    Func(VRef<CompiledFunc>),
}

//////////////////////////////////
// Requests
//////////////////////////////////

#[derive(Default)]
pub struct CompileRequests {
    pub queue: Vec<CompileRequest>,
    pub ty_slices: BumpMap<VRef<Ty>>,
}

impl CompileRequests {
    pub fn add_request(
        &mut self,
        id: VRef<CompiledFunc>,
        func: VRef<Func>,
        params: impl IntoIterator<Item = VRef<Ty>>,
    ) {
        self.queue.push(CompileRequest {
            id,
            func,
            params: self.ty_slices.bump(params),
        })
    }

    pub fn clear(&mut self) {
        self.ty_slices.clear();
    }
}

pub struct CompileRequest {
    pub id: VRef<CompiledFunc>,
    pub func: VRef<Func>,
    pub params: VRefSlice<Ty>,
}

//////////////////////////////////
// Resources
//////////////////////////////////

pub struct GenResources {
    pub blocks: ShadowMap<BlockMir, Maybe<GenBlock>>,
    pub values: ShadowMap<ValueMir, PackedOption<ir::Value>>,
    pub func_imports: Map<VRef<str>, ir::FuncRef>,
    pub ctx: Context,
}

impl GenResources {
    pub fn new() -> Self {
        Self {
            blocks: ShadowMap::new(),
            values: ShadowMap::new(),
            func_imports: Map::default(),
            ctx: Context::new(),
        }
    }

    pub fn clear(&mut self) {
        self.blocks.clear();
        self.values.clear();
        self.func_imports.clear();
        self.ctx.clear();
    }
}

impl Default for GenResources {
    fn default() -> Self {
        Self::new()
    }
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

//////////////////////////////////
// Layout
//////////////////////////////////

#[derive(Default)]
pub struct GenLayouts {
    pub mapping: ShadowMap<Ty, Maybe<Layout>>,
    pub offsets: BumpMap<Offset>,
}

pub type Offset = u32;

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
