use cranelift_codegen::{
    binemit::{CodeOffset, Reloc},
    ir::{self, types, Type},
    isa::CallConv,
    packed_option::PackedOption,
};

use mir_t::*;
use storage::*;
use target_lexicon::Triple;
use typec_t::*;

#[derive(Default)]
pub struct Gen {
    pub compiled_funcs: OrderedMap<VRef<str>, CompiledFunc>,
}

pub struct CompiledFunc {
    pub func: VRef<Func>,
    pub signature: ir::Signature,
    pub bytecode: Vec<u8>,
    pub alignment: u64,
    pub relocs: Vec<GenReloc>,
    pub temp: bool,
}

impl CompiledFunc {
    pub fn new(func: VRef<Func>) -> Self {
        Self {
            func,
            signature: ir::Signature::new(CallConv::Fast),
            bytecode: Vec::new(),
            alignment: 0,
            relocs: Vec::new(),
            temp: false,
        }
    }
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

#[derive(Default)]
pub struct GenResources {
    pub blocks: ShadowMap<BlockMir, Maybe<GenBlock>>,
    pub values: ShadowMap<ValueMir, PackedOption<ir::Value>>,
    pub func_imports: Map<VRef<str>, ir::FuncRef>,
    pub func_constants: ShadowMap<FuncConstMir, Option<GenFuncConstant>>,
    pub triple: String,
    pub jit: bool,
}

impl GenResources {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn clear(&mut self, next_triplet: &Triple, jit: bool) {
        self.blocks.clear();
        self.values.clear();
        self.func_imports.clear();
        // self.func_constants.clear();
        self.triple.clear();

        use std::fmt::Write;
        write!(self.triple, "{}", next_triplet).unwrap();
        self.jit = jit;
    }
}

#[derive(Clone, Copy, Debug)]
pub enum GenFuncConstant {
    Int(u64),
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

//////////////////////////////////
// relocs
//////////////////////////////////

#[derive(Clone, Copy)]
pub struct GenReloc {
    /// The offset at which the relocation applies, *relative to the
    /// containing section*.
    pub offset: CodeOffset,
    /// The kind of relocation.
    pub kind: Reloc,
    /// The external symbol / name to which this relocation refers.
    pub name: GenItemName,
    /// The addend to add to the symbol value.
    pub addend: isize,
}

#[derive(Clone, Copy)]
pub enum GenItemName {
    Func(VRef<CompiledFunc>),
}
