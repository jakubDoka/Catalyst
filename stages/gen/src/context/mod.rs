use std::ops::{Deref, DerefMut};

use cranelift_codegen::{
    binemit::{CodeOffset, Reloc},
    ir::{self, types, ExternalName, Type, UserExternalName},
    isa::{self, CallConv, LookupError, TargetIsa},
    packed_option::PackedOption,
    settings, CodegenError, Context,
};

use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use mir_t::*;
use storage::*;
use target_lexicon::Triple;
use typec_t::*;

#[derive(Default)]
pub struct Gen {
    pub compiled_funcs: OrderedMap<VRef<str>, CompiledFunc>,
}

impl Gen {
    pub const FUNC_NAMESPACE: u32 = 0;

    pub fn save_compiled_code(
        &mut self,
        id: VRef<CompiledFunc>,
        ctx: &Context,
    ) -> Result<(), CodeSaveError> {
        let cc = ctx.compiled_code().ok_or(CodeSaveError::MissingCode)?;

        let relocs = cc
            .buffer
            .relocs()
            .iter()
            .map(|rel| {
                Ok(GenReloc {
                    offset: rel.offset,
                    kind: rel.kind,
                    name: match rel.name {
                        ExternalName::User(user) => match &ctx.func.params.user_named_funcs()[user]
                        {
                            &UserExternalName {
                                namespace: Self::FUNC_NAMESPACE,
                                index,
                            } => GenItemName::Func(unsafe { VRef::new(index as usize) }),
                            name => unreachable!("Unexpected name: {:?}", name),
                        },
                        ExternalName::TestCase(_)
                        | ExternalName::LibCall(_)
                        | ExternalName::KnownSymbol(_) => todo!(),
                    },
                    addend: rel
                        .addend
                        .try_into()
                        .map_err(|_| CodeSaveError::AddendOverflow)?,
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        let func = CompiledFunc {
            signature: ctx.func.signature.clone(),
            bytecode: cc.buffer.data().to_vec(),
            alignment: cc.alignment as u64,
            relocs,
            ..self.compiled_funcs[id]
        };

        self.compiled_funcs[id] = func;

        Ok(())
    }
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

#[derive(Debug)]
pub enum CodeSaveError {
    MissingCode,
    AddendOverflow,
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
}

impl GenResources {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn clear(&mut self) {
        self.blocks.clear();
        self.values.clear();
        self.func_imports.clear();
        // self.func_constants.clear();
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

#[derive(Clone, Copy, Debug)]
pub enum GenItemName {
    Func(VRef<CompiledFunc>),
}

//////////////////////////////////
/// Isa
//////////////////////////////////

pub struct Isa {
    pub triple: VRef<str>,
    pub pointer_ty: Type,
    pub inner: Box<dyn TargetIsa>,
    pub jit: bool,
}

impl Isa {
    pub fn new(
        triple: Triple,
        flags: settings::Flags,
        jit: bool,
        interner: &mut Interner,
    ) -> Result<Self, IsaCreationError> {
        let triple_str = interner.intern_str(&triple.to_string());
        isa::lookup(triple)
            .map_err(IsaCreationError::Lookup)?
            .finish(flags)
            .map_err(IsaCreationError::Codegen)
            .map(|isa| Self {
                triple: triple_str,
                pointer_ty: isa.pointer_type(),
                inner: isa,
                jit,
            })
    }
}

impl Deref for Isa {
    type Target = dyn TargetIsa;

    fn deref(&self) -> &Self::Target {
        &*self.inner
    }
}

impl DerefMut for Isa {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut *self.inner
    }
}

#[derive(Debug)]
pub enum IsaCreationError {
    Lookup(LookupError),
    Codegen(CodegenError),
}

impl std::error::Error for IsaCreationError {}

impl std::fmt::Display for IsaCreationError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            IsaCreationError::Lookup(e) => write!(f, "Lookup error: {}", e),
            IsaCreationError::Codegen(e) => write!(f, "Codegen error: {}", e),
        }
    }
}

//////////////////////////////////
/// Function builder
//////////////////////////////////

pub struct GenBuilder<'a, 'b> {
    pub isa: &'a Isa,
    pub body: &'a FuncMir,
    inner: FunctionBuilder<'b>,
}

impl<'a, 'b> GenBuilder<'a, 'b> {
    pub fn new(
        isa: &'a Isa,
        body: &'a FuncMir,
        func: &'b mut ir::Function,
        ctx: &'b mut FunctionBuilderContext,
    ) -> Self {
        Self {
            isa,
            body,
            inner: FunctionBuilder::new(func, ctx),
        }
    }

    pub fn ptr_ty(&self) -> Type {
        self.isa.pointer_ty
    }
}

impl<'b> Deref for GenBuilder<'_, 'b> {
    type Target = FunctionBuilder<'b>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for GenBuilder<'_, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}
