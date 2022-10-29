use std::{
    alloc, iter,
    num::NonZeroU8,
    ops::{Deref, DerefMut},
    sync::Arc,
};

use cranelift_codegen::{
    binemit::{CodeOffset, Reloc},
    ir::{self, types, ExternalName, Type, UserExternalName},
    isa::{self, CallConv, LookupError, TargetIsa},
    settings, CodegenError, Context,
};

use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use mir_t::*;
use serde::{Deserialize, Serialize};
use storage::*;
use target_lexicon::Triple;
use typec_t::*;

#[derive(Default, Deserialize, Serialize)]
pub struct Gen {
    pub compiled_funcs: OrderedMap<VRef<str>, CompiledFunc>,
}

impl Gen {
    pub const FUNC_NAMESPACE: u32 = 0;

    pub fn clear(&mut self) {
        self.compiled_funcs.clear();
    }

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
                        ExternalName::LibCall(lc) => GenItemName::LibCall(lc),
                        ExternalName::TestCase(_) | ExternalName::KnownSymbol(_) => todo!(),
                    },
                    addend: rel
                        .addend
                        .try_into()
                        .map_err(|_| CodeSaveError::AddendOverflow)?,
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        let func = CompiledFunc {
            inner: Some(Arc::new(CompiledFuncInner {
                signature: ctx.func.signature.clone(),
                bytecode: cc.buffer.data().to_vec(),
                alignment: cc.alignment as u64,
                relocs,
            })),
            ..self.compiled_funcs[id]
        };

        self.compiled_funcs[id] = func;

        Ok(())
    }
}

#[derive(Clone, Deserialize, Serialize)]
pub struct CompiledFunc {
    pub func: VRef<Func>,
    pub inner: Option<Arc<CompiledFuncInner>>,
}

#[derive(Deserialize, Serialize)]
pub struct CompiledFuncInner {
    pub signature: ir::Signature,
    pub bytecode: Vec<u8>,
    pub alignment: u64,
    pub relocs: Vec<GenReloc>,
}

impl CompiledFunc {
    pub fn new(func: VRef<Func>) -> Self {
        Self { func, inner: None }
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
    pub ty_slices: BumpMap<Ty>,
    pub children: BumpMap<CompileRequestChild>,
}

impl CompileRequests {
    pub fn add_request(
        &mut self,
        id: VRef<CompiledFunc>,
        func: VRef<Func>,
        params: impl IntoIterator<Item = Ty>,
        children: impl IntoIterator<Item = CompileRequestChild>,
    ) {
        self.queue.push(CompileRequest {
            id,
            func,
            params: self.ty_slices.bump(params),
            children: self.children.bump(children),
        })
    }

    pub fn clear(&mut self) {
        self.ty_slices.clear();
    }
}

#[derive(Clone, Copy)]
pub struct CompileRequest {
    pub id: VRef<CompiledFunc>,
    pub func: VRef<Func>,
    pub params: VSlice<Ty>,
    pub children: VSlice<CompileRequestChild>,
}

#[derive(Clone, Copy)]
pub struct CompileRequestChild {
    pub id: VRef<CompiledFunc>,
    pub func: VRef<Func>,
    pub params: VSlice<Ty>,
}

//////////////////////////////////
// Resources
//////////////////////////////////

#[derive(Default)]
pub struct GenResources {
    pub blocks: ShadowMap<BlockMir, Option<GenBlock>>,
    pub values: ShadowMap<ValueMir, GenValue>,
    pub func_imports: Map<VRef<str>, (ir::FuncRef, bool)>,
    pub block_stack: Vec<(VRef<BlockMir>, ir::Block)>,
    pub calls: Vec<CompileRequestChild>,
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

#[derive(Clone, Copy, Default, Debug)]
pub struct GenValue {
    pub computed: Option<ComputedValue>,
    pub offset: i32,
    pub must_load: bool,
}

#[derive(Clone, Copy, Debug)]
pub enum ComputedValue {
    Value(ir::Value),
    StackSlot(ir::StackSlot),
    Variable(Variable),
}

impl From<ir::Value> for ComputedValue {
    fn from(val: ir::Value) -> Self {
        Self::Value(val)
    }
}

impl From<ir::StackSlot> for ComputedValue {
    fn from(slot: ir::StackSlot) -> Self {
        Self::StackSlot(slot)
    }
}

impl From<Variable> for ComputedValue {
    fn from(var: Variable) -> Self {
        Self::Variable(var)
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

//////////////////////////////////
// Layout
//////////////////////////////////

#[derive(Default)]
pub struct GenLayouts {
    pub mapping: Map<Ty, Layout>,
    pub offsets: BumpMap<Offset>,
    pub ptr_ty: Type,
}

impl GenLayouts {
    pub fn ty_layout(
        &mut self,
        ty: Ty,
        params: &[Ty],
        typec: &mut Typec,
        interner: &mut Interner,
    ) -> Layout {
        if let Some(&layout) = self.mapping.get(&ty) {
            return layout;
        }

        let res = match ty {
            Ty::Struct(s) => {
                let Struct { fields, .. } = typec.structs[s];
                let mut offsets = bumpvec![cap typec.fields[fields].len()];

                let layouts = typec.fields[fields]
                    .to_bumpvec()
                    .into_iter()
                    .map(|field| self.ty_layout(field.ty, params, typec, interner));

                let mut align = 1;
                let mut size = 0;
                for layout in layouts {
                    align = align.max(layout.align.get());

                    offsets.push(size);

                    let padding = (layout.align.get() - (size as u8 & (layout.align.get() - 1)))
                        & (layout.align.get() - 1);
                    size += padding as u32;
                    size += layout.size;
                }

                let (repr, on_stack) = self.repr_for_size(size);

                Layout {
                    repr,
                    offsets: self.offsets.bump(offsets),
                    align: align.try_into().unwrap(),
                    size,
                    on_stack,
                }
            }
            Ty::Pointer(..) | Ty::Builtin(Builtin::Uint) => Layout {
                repr: self.ptr_ty,
                offsets: VSlice::empty(),
                align: (self.ptr_ty.bytes() as u8).try_into().unwrap(),
                size: self.ptr_ty.bytes() as u32,
                on_stack: false,
            },
            Ty::Builtin(Builtin::Bool) => Layout {
                repr: types::B1,
                offsets: VSlice::empty(),
                align: 1.try_into().unwrap(),
                size: 1,
                on_stack: false,
            },
            Ty::Builtin(b) => {
                let size = b.size();
                let (repr, on_stack) = self.repr_for_size(size);
                Layout {
                    size,
                    offsets: VSlice::empty(),
                    align: (size.max(1) as u8).try_into().unwrap(),
                    repr,
                    on_stack,
                }
            }
            Ty::Param(index) => {
                return self.ty_layout(params[index as usize], &[], typec, interner)
            }
            Ty::Instance(inst) => {
                let Instance { base, args } = typec[inst];
                // remap the instance parameters so we can compute the layout correctly
                let params = typec.args[args]
                    .to_bumpvec()
                    .into_iter()
                    .map(|ty| typec.instantiate(ty, params, interner))
                    .collect::<BumpVec<_>>();
                self.ty_layout(base.as_ty(), &params, typec, interner)
            }
            Ty::Enum(ty) => {
                let size = typec.get_enum_flag_ty(ty).map(|ty| ty.size());
                let (base_size, base_align) = typec[typec[ty].variants]
                    .to_bumpvec()
                    .into_iter()
                    .map(|variant| self.ty_layout(variant.ty, params, typec, interner))
                    .map(|layout| (layout.size, layout.align.get()))
                    .max()
                    .unwrap_or((0, 1));

                let align = base_align.max(size.unwrap_or(0) as u8);
                let size = size.map(|size| size.max(align as u32));
                let offsets = iter::once(0).chain(size).collect::<BumpVec<_>>();
                let size = base_size + size.unwrap_or(align as u32);

                let (repr, on_stack) = self.repr_for_size(size);
                Layout {
                    size,
                    offsets: self.offsets.bump(offsets),
                    align: align.try_into().unwrap(),
                    repr,
                    on_stack,
                }
            }
        };

        if ty.as_generic().map_or(true, |g| !g.is_generic(typec)) {
            self.mapping.insert(ty, res);
        }

        res
    }

    fn repr_for_size(&self, size: u32) -> (Type, bool) {
        if size > self.ptr_ty.bytes() as u32 {
            return (self.ptr_ty, true);
        }

        (
            match size {
                8.. => types::I64,
                4.. => types::I32,
                2.. => types::I16,
                0.. => types::I8,
            },
            false,
        )
    }
}

pub type Offset = u32;

#[derive(Clone, Copy, Debug)]
pub struct Layout {
    pub size: u32,
    pub offsets: VSlice<Offset>,
    pub align: NonZeroU8,
    pub repr: Type,
    pub on_stack: bool,
}

impl Layout {
    pub const EMPTY: Self = Self {
        size: 0,
        align: unsafe { NonZeroU8::new_unchecked(1) },
        offsets: VSlice::empty(),
        repr: types::INVALID,
        on_stack: false,
    };

    pub fn rust_layout(&self) -> alloc::Layout {
        alloc::Layout::from_size_align(self.size as usize, self.align.get() as usize).unwrap()
    }
}

//////////////////////////////////
// relocs
//////////////////////////////////

#[derive(Clone, Copy, Serialize, Deserialize)]
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

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub enum GenItemName {
    Func(VRef<CompiledFunc>),
    LibCall(ir::LibCall),
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
        let triple_str = interner.intern(&triple.to_string());
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
    pub body: &'a FuncMirInner,
    pub struct_ret: Option<ir::Value>,
    pub dependant_types: &'a Vec<MirTy>,
    inner: FunctionBuilder<'b>,
}

impl<'a, 'b> GenBuilder<'a, 'b> {
    pub fn new(
        isa: &'a Isa,
        body: &'a FuncMirInner,
        func: &'b mut ir::Function,
        dependant_types: &'a Vec<MirTy>,
        ctx: &'b mut FunctionBuilderContext,
    ) -> Self {
        Self {
            isa,
            body,
            struct_ret: None,
            dependant_types,
            inner: FunctionBuilder::new(func, ctx),
        }
    }

    pub fn value_ty(&self, value: VRef<ValueMir>) -> Ty {
        self.dependant_types[self.body.values[value].ty.index()].ty
    }

    pub fn ptr_ty(&self) -> Type {
        self.isa.pointer_ty
    }

    pub fn system_cc(&self) -> CallConv {
        self.isa.default_call_conv()
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
