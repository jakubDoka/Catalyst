use std::convert::TryInto;

use cranelift_codegen::{
    binemit::{CodeOffset, Reloc},
    ir::{self, types, Type},
    packed_option::PackedOption,
    Context,
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
    pub relocs: Vec<GenReloc>,
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

pub fn perform_jit_relocations(
    region: &[u8],
    relocs: &[GenReloc],
    get_address: impl Fn(GenItemName) -> *const u8,
    get_got_entry: impl Fn(GenItemName) -> *const u8,
    get_plt_entry: impl Fn(GenItemName) -> *const u8,
) {
    use std::ptr::write_unaligned;

    for &GenReloc {
        kind,
        offset,
        name,
        addend,
    } in relocs
    {
        assert!((offset as usize) < region.len());

        // SAFETY: we checked that the offset is in bounds.
        let at = unsafe { region.as_ptr().add(offset as usize) };

        let base = match kind {
            Reloc::Abs4
            | Reloc::Abs8
            | Reloc::X86PCRel4
            | Reloc::X86CallPCRel4
            | Reloc::S390xPCRel32Dbl
            | Reloc::S390xPLTRel32Dbl
            | Reloc::Arm64Call => get_address(name),

            Reloc::X86CallPLTRel4 => get_plt_entry(name),
            Reloc::X86GOTPCRel4 => get_got_entry(name),

            kind => unimplemented!("{kind:?}"),
        };

        let what = match kind {
            Reloc::Abs4
            | Reloc::Abs8
            | Reloc::X86PCRel4
            | Reloc::X86CallPCRel4
            | Reloc::S390xPCRel32Dbl
            | Reloc::S390xPLTRel32Dbl
            | Reloc::X86CallPLTRel4
            | Reloc::X86GOTPCRel4 => unsafe { base.offset(addend) },

            Reloc::Arm64Call => {
                // The instruction is 32 bits long.
                let instruction_ptr = at as *mut u32;
                // The offset encoded in the `bl` instruction is the
                // number of bytes divided by 4.
                let diff = ((base as isize) - (at as isize)) >> 2;
                // Sign propagating right shift disposes of the
                // included bits, so the result is expected to be
                // either all sign bits or 0, depending on if the original
                // value was negative or positive.
                assert!((diff >> 26 == -1) || (diff >> 26 == 0));
                // The lower 26 bits of the `bl` instruction form the
                // immediate offset argument.
                let chop = 32 - 26;
                let imm26 = (diff as u32) << chop >> chop;
                let ins = unsafe { instruction_ptr.read_unaligned() } | imm26;
                unsafe {
                    instruction_ptr.write_unaligned(ins);
                }
                return;
            }

            _ => unimplemented!("{kind:?}"),
        };

        let pc_relocation_addr = match kind {
            Reloc::X86PCRel4
            | Reloc::X86CallPCRel4
            | Reloc::X86GOTPCRel4
            | Reloc::X86CallPLTRel4
            | Reloc::S390xPCRel32Dbl
            | Reloc::S390xPLTRel32Dbl => unsafe { what.sub(at as usize) },

            Reloc::Abs4 | Reloc::Abs8 => what,

            _ => unimplemented!("{kind:?}"),
        };

        match kind {
            Reloc::Abs8 => unsafe { write_unaligned(at as *mut u64, pc_relocation_addr as u64) },

            Reloc::Abs4
            | Reloc::X86PCRel4
            | Reloc::X86CallPCRel4
            | Reloc::X86GOTPCRel4
            | Reloc::X86CallPLTRel4
            | Reloc::S390xPCRel32Dbl
            | Reloc::S390xPLTRel32Dbl => unsafe {
                write_unaligned(
                    at as *mut u32,
                    (pc_relocation_addr as usize)
                        .try_into()
                        .expect("relocation overflow"),
                )
            },

            _ => unimplemented!("{kind:?}"),
        }
    }
}
