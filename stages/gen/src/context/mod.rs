use std::{
    alloc,
    default::default,
    iter, mem,
    num::NonZeroU8,
    ops::{Deref, DerefMut, Range},
    sync::Arc,
};

use cranelift_codegen::{
    binemit::{CodeOffset, Reloc},
    ir::{self, types, ArgumentPurpose, ExternalName, LibCall, Type, UserExternalName},
    isa::{self, CallConv, LookupError, TargetIsa},
    settings, CodegenError, Context,
};

use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use mir_t::*;

use rkyv::{
    ser::{ScratchSpace, Serializer},
    with::{ArchiveWith, DeserializeWith, SerializeWith},
    Archive, Archived, Deserialize, Fallible, Resolver, Serialize,
};
use storage::{dashmap::mapref::one::Ref, *};
use target_lexicon::Triple;
use typec_t::*;

#[derive(Serialize, Archive, Deserialize)]

pub struct GenBase {
    lookup: Arc<CMap<Ident, CompiledFunc>>,
}

derive_relocated!(struct GenBase { lookup });

impl GenBase {
    pub fn new(_thread_count: u8) -> Self {
        Self { lookup: default() }
    }

    pub fn split(&self) -> impl Iterator<Item = Gen> + '_ {
        iter::repeat_with(|| Gen {
            lookup: self.lookup.clone(),
        })
    }

    pub fn register<'a>(&'a mut self, objects: &mut RelocatedObjects<'a>) {
        self.reallocate();
        objects.add_root(&mut self.lookup);
    }

    pub fn prepare(&mut self) {
        self.lookup
            .iter_mut()
            .for_each(|mut item| item.unused = true);
    }

    fn reallocate(&mut self) {
        self.lookup.retain(|_, v| !v.unused);
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, Archive, Serialize, Deserialize, Hash, PartialEq, Eq, Debug)]
pub struct CompiledFuncRef(Ident);

pub struct Gen {
    lookup: Arc<CMap<Ident, CompiledFunc>>,
}

impl Gen {
    pub fn get(&self, id: Ident) -> Option<CompiledFuncRef> {
        self.lookup.get(&id).map(|_value| CompiledFuncRef(id))
    }

    pub fn get_direct(&self, id: CompiledFuncRef) -> Ref<Ident, CompiledFunc, FvnBuildHasher> {
        self.lookup.get(&id.0).unwrap()
    }

    pub fn get_or_insert(&mut self, id: Ident, func: FragRef<Func>) -> CompiledFuncRef {
        self.lookup
            .entry(id)
            .and_modify(|func| func.unused = false)
            .or_insert_with(|| CompiledFunc::new(func, id));

        CompiledFuncRef(id)
    }

    pub fn save_compiled_code(
        &mut self,
        id: CompiledFuncRef,
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
                        ExternalName::User(user) => GenItemName::decode_name(
                            ctx.func.params.user_named_funcs()[user].clone(),
                        ),
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

        let mut func = self.lookup.get_mut(&id.0).unwrap();
        func.signature = ctx.func.signature.clone();
        func.bytecode = cc.buffer.data().to_vec();
        func.alignment = cc.alignment as u64;
        func.relocs = relocs;

        Ok(())
    }
}

#[derive(Serialize, Archive, Deserialize)]

pub struct CompiledFunc {
    func: FragRef<Func>,
    unused: bool,
    #[with(SignatureArchiver)]
    pub signature: ir::Signature,
    pub bytecode: Vec<u8>,
    pub alignment: u64,
    pub relocs: Vec<GenReloc>,
}

impl CompiledFunc {
    pub fn new(func: FragRef<Func>, _name: Ident) -> Self {
        Self {
            func,
            unused: false,
            signature: ir::Signature::new(CallConv::Fast),
            bytecode: default(),
            alignment: default(),
            relocs: default(),
        }
    }

    pub fn func(&self) -> FragRef<Func> {
        self.func
    }

    pub fn set_func(&mut self, value: FragRef<Func>) {
        self.func = value;
    }
}

derive_relocated!(struct CompiledFunc { func });

#[derive(Serialize, Archive, Deserialize)]

pub struct CompiledFuncInner {}

pub struct SignatureArchiver;

impl ArchiveWith<ir::Signature> for SignatureArchiver {
    type Archived = Archived<ArchivedSignature>;

    type Resolver = Resolver<ArchivedSignature>;

    unsafe fn resolve_with(
        field: &ir::Signature,
        pos: usize,
        resolver: Self::Resolver,
        out: *mut Self::Archived,
    ) {
        ArchivedSignature::from_signature(field).resolve(pos, resolver, out)
    }
}

impl<S: ScratchSpace + Serializer + ?Sized> SerializeWith<ir::Signature, S> for SignatureArchiver {
    fn serialize_with(
        field: &ir::Signature,
        serializer: &mut S,
    ) -> Result<Self::Resolver, <S as Fallible>::Error> {
        ArchivedSignature::from_signature(field).serialize(serializer)
    }
}

impl<D: Fallible + ?Sized> DeserializeWith<Archived<ArchivedSignature>, ir::Signature, D>
    for SignatureArchiver
where
    <ArchivedSignature as Archive>::Archived: Deserialize<ArchivedSignature, D>,
{
    fn deserialize_with(
        field: &Archived<ArchivedSignature>,
        deserializer: &mut D,
    ) -> Result<ir::Signature, <D as Fallible>::Error> {
        field
            .deserialize(deserializer)
            .map(|a| a.into_ir_signature())
    }
}

#[derive(Archive, Deserialize, Serialize)]

pub struct ArchivedSignature {
    #[with(CallConvArchiver)]
    cc: CallConv,
    args: Vec<(u16, bool)>,
    ret: Option<u16>,
}

impl ArchivedSignature {
    pub fn from_signature(sig: &ir::Signature) -> Self {
        Self {
            cc: sig.call_conv,
            args: sig
                .params
                .iter()
                .map(|p| {
                    (
                        unsafe { mem::transmute(p.value_type) },
                        p.purpose == ArgumentPurpose::StructReturn,
                    )
                })
                .collect(),
            ret: sig
                .returns
                .first()
                .map(|r| unsafe { mem::transmute(r.value_type) }),
        }
    }

    pub fn into_ir_signature(self) -> ir::Signature {
        ir::Signature {
            params: self
                .args
                .into_iter()
                .map(|(ty, is_struct_ret)| match is_struct_ret {
                    true => ir::AbiParam::special(
                        unsafe { mem::transmute(ty) },
                        ArgumentPurpose::StructReturn,
                    ),
                    false => ir::AbiParam::new(unsafe { mem::transmute(ty) }),
                })
                .collect(),
            returns: self
                .ret
                .map(|r| ir::AbiParam::new(unsafe { mem::transmute(r) }))
                .into_iter()
                .collect(),
            call_conv: self.cc,
        }
    }
}

pub struct CallConvArchiver;

impl ArchiveWith<CallConv> for CallConvArchiver {
    type Archived = Archived<u8>;

    type Resolver = Resolver<u8>;

    unsafe fn resolve_with(
        field: &CallConv,
        pos: usize,
        resolver: Self::Resolver,
        out: *mut Self::Archived,
    ) {
        mem::transmute_copy::<_, u8>(field).resolve(pos, resolver, out)
    }
}

impl<S: Serializer + ?Sized> SerializeWith<CallConv, S> for CallConvArchiver {
    fn serialize_with(
        field: &CallConv,
        serializer: &mut S,
    ) -> Result<Self::Resolver, <S as rkyv::Fallible>::Error> {
        // SAFETY: ther is none
        unsafe { mem::transmute_copy::<_, u8>(field).serialize(serializer) }
    }
}

impl<D: Fallible + ?Sized> DeserializeWith<Archived<u8>, CallConv, D> for CallConvArchiver {
    fn deserialize_with(
        field: &Archived<u8>,
        _deserializer: &mut D,
    ) -> Result<CallConv, <D as rkyv::Fallible>::Error> {
        Ok(unsafe { mem::transmute_copy::<_, CallConv>(field) })
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

#[derive(Default, Clone)]
pub struct CompileRequests {
    pub queue: Vec<CompileRequest>,
    pub ty_slices: PushMap<Ty>,
    pub children: PushMap<CompileRequestChild>,
    pub drop_children: PushMap<VSlice<CompileRequestChild>>,
}

impl CompileRequests {
    pub fn clear(&mut self) {
        self.queue.clear();
        self.ty_slices.clear();
        self.children.clear();
        self.drop_children.clear();
    }
}

#[derive(Clone, Copy)]
pub struct CompileRequest {
    pub id: CompiledFuncRef,
    pub func: FragRef<Func>,
    pub params: VSlice<Ty>,
    pub children: VSlice<CompileRequestChild>,
    pub drops: VSlice<VSlice<CompileRequestChild>>,
}

#[derive(Clone, Copy)]
pub struct CompileRequestChild {
    pub id: CompiledFuncRef,
    pub func: FragRef<Func>,
    pub params: VSlice<Ty>,
}

//////////////////////////////////
// Resources
//////////////////////////////////

#[derive(Default)]
pub struct GenResources {
    pub blocks: ShadowMap<BlockMir, Option<GenBlock>>,
    pub values: ShadowMap<ValueMir, GenValue>,
    pub func_imports: Map<CompiledFuncRef, (ir::FuncRef, bool)>,
    pub block_stack: Vec<(VRef<BlockMir>, bool, ir::Block)>,
    pub calls: Vec<CompileRequestChild>,
    pub call_offset: usize,
    pub drops: Vec<Range<usize>>,
    pub drops_offset: usize,
    pub signature_pool: Vec<ir::Signature>,
}

impl GenResources {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn clear(&mut self) {
        self.blocks.clear();
        self.values.clear();
        self.func_imports.clear();
    }

    pub fn reuse_signature(&mut self) -> ir::Signature {
        self.signature_pool
            .pop()
            .unwrap_or_else(|| ir::Signature::new(CallConv::Fast))
    }

    pub fn recycle_signatures<'a>(&mut self, sigs: impl Iterator<Item = &'a mut ir::Signature>) {
        self.signature_pool.extend(sigs.map(|sig| {
            sig.clear(CallConv::Fast);
            mem::replace(sig, ir::Signature::new(CallConv::Fast))
        }));
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
    pub forward_visit_count: u16,
    pub backward_visit_count: u16,
}

//////////////////////////////////
// Layout
//////////////////////////////////

#[derive(Default)]
pub struct GenLayouts {
    pub mapping: Map<Ty, Layout>,
    pub offsets: PushMap<Offset>,
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
                let Struct { fields, .. } = typec[s];
                let mut offsets = bumpvec![cap fields.len()];

                let layouts = typec[fields]
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
                    offsets: self.offsets.extend(offsets),
                    align: align.try_into().unwrap(),
                    size,
                    on_stack,
                }
            }
            Ty::Pointer(..) | Ty::Builtin(Builtin::Uint) => Layout {
                repr: self.ptr_ty,
                offsets: VSlice::empty(),
                align: (self.ptr_ty.bytes() as u8).try_into().unwrap(),
                size: self.ptr_ty.bytes(),
                on_stack: false,
            },
            Ty::Builtin(Builtin::Bool) => Layout {
                repr: types::I8,
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
                let params = typec[args]
                    .to_bumpvec()
                    .into_iter()
                    .map(|ty| typec.instantiate(ty, params, interner))
                    .collect::<BumpVec<_>>();
                self.ty_layout(base.as_ty(), &params, typec, interner)
            }
            Ty::Enum(ty) => {
                let size = typec.enum_flag_ty(ty).map(|ty| ty.size());
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
                    offsets: self.offsets.extend(offsets),
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
        if size > self.ptr_ty.bytes() {
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

    pub fn clear(&mut self, ptr_ty: Type) {
        self.mapping.clear();
        self.offsets.clear();
        self.ptr_ty = ptr_ty;
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

#[derive(Clone, Copy, Serialize, Archive, Deserialize)]

pub struct GenReloc {
    /// The offset at which the relocation applies, *relative to the
    /// containing section*.
    pub offset: CodeOffset,
    /// The kind of relocation.
    #[with(RelocArchiver)]
    pub kind: Reloc,
    /// The external symbol / name to which this relocation refers.
    pub name: GenItemName,
    /// The addend to add to the symbol value.
    pub addend: isize,
}

pub struct RelocArchiver;

impl ArchiveWith<Reloc> for RelocArchiver {
    type Archived = Archived<u8>;

    type Resolver = Resolver<u8>;

    unsafe fn resolve_with(
        field: &Reloc,
        pos: usize,
        resolver: Self::Resolver,
        out: *mut Self::Archived,
    ) {
        mem::transmute_copy::<_, u8>(field).resolve(pos, resolver, out)
    }
}

impl<S: Serializer + ?Sized> SerializeWith<Reloc, S> for RelocArchiver {
    fn serialize_with(
        field: &Reloc,
        serializer: &mut S,
    ) -> Result<Self::Resolver, <S as rkyv::Fallible>::Error> {
        // SAFETY: ther is none
        unsafe { mem::transmute_copy::<_, u8>(field).serialize(serializer) }
    }
}

impl<D: Fallible + ?Sized> DeserializeWith<Archived<u8>, Reloc, D> for RelocArchiver {
    fn deserialize_with(
        field: &Archived<u8>,
        _deserializer: &mut D,
    ) -> Result<Reloc, <D as rkyv::Fallible>::Error> {
        Ok(unsafe { mem::transmute_copy::<_, Reloc>(field) })
    }
}

#[derive(Clone, Copy, Debug, Serialize, Archive, Deserialize)]

pub enum GenItemName {
    Func(CompiledFuncRef),
    LibCall(#[with(LibCallArchiver)] ir::LibCall),
}

pub struct LibCallArchiver;

impl ArchiveWith<LibCall> for LibCallArchiver {
    type Archived = Archived<u8>;

    type Resolver = Resolver<u8>;

    unsafe fn resolve_with(
        field: &LibCall,
        pos: usize,
        resolver: Self::Resolver,
        out: *mut Self::Archived,
    ) {
        mem::transmute_copy::<_, u8>(field).resolve(pos, resolver, out)
    }
}

impl<S: Serializer + ?Sized> SerializeWith<LibCall, S> for LibCallArchiver {
    fn serialize_with(
        field: &LibCall,
        serializer: &mut S,
    ) -> Result<Self::Resolver, <S as rkyv::Fallible>::Error> {
        // SAFETY: ther is none
        unsafe { mem::transmute_copy::<_, u8>(field).serialize(serializer) }
    }
}

impl<D: Fallible + ?Sized> DeserializeWith<Archived<u8>, LibCall, D> for LibCallArchiver {
    fn deserialize_with(
        field: &Archived<u8>,
        _deserializer: &mut D,
    ) -> Result<LibCall, <D as rkyv::Fallible>::Error> {
        Ok(unsafe { mem::transmute_copy::<_, LibCall>(field) })
    }
}

impl GenItemName {
    pub const FUNC_FLAG: u8 = 1;

    pub fn decode_name(UserExternalName { namespace, index }: UserExternalName) -> GenItemName {
        let (flag, len, thread) = (
            (namespace >> 24) as u8,
            (namespace >> 8) as u16,
            namespace as u8,
        );

        match flag {
            Self::FUNC_FLAG => {
                GenItemName::Func(unsafe { mem::transmute(FragSliceAddr::new(index, thread, len)) })
            }
            _ => unreachable!(),
        }
    }

    pub fn encode_func(func: CompiledFuncRef) -> UserExternalName {
        let FragSliceAddr { index, thread, len } = unsafe { mem::transmute(func) };
        UserExternalName {
            namespace: thread as u32 | (len as u32) << 8 | (Self::FUNC_FLAG as u32) << 24,
            index,
        }
    }
}

//////////////////////////////////
/// Isa
//////////////////////////////////

pub struct Isa {
    pub triple: String,
    pub pointer_ty: Type,
    pub inner: Box<dyn TargetIsa>,
    pub jit: bool,
}

impl Isa {
    pub fn new(
        triple: Triple,
        flags: settings::Flags,
        jit: bool,
    ) -> Result<Self, IsaCreationError> {
        let triple_str = triple.to_string();
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

    pub fn host(jit: bool) -> Result<Self, IsaCreationError> {
        let triple = Triple::host();
        let flags_builder = settings::builder();
        let flags = settings::Flags::new(flags_builder);
        Self::new(triple, flags, jit)
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
            IsaCreationError::Lookup(e) => write!(f, "Lookup error: {e}"),
            IsaCreationError::Codegen(e) => write!(f, "Codegen error: {e}"),
        }
    }
}

//////////////////////////////////
/// Function builder
//////////////////////////////////

pub struct GenBuilder<'a, 'b> {
    pub isa: &'a Isa,
    pub body: &'a ModuleMirInner,
    pub struct_ret: Option<ir::Value>,
    pub dependant_types: &'a FuncTypes,
    inner: FunctionBuilder<'b>,
}

impl<'a, 'b> GenBuilder<'a, 'b> {
    pub fn new(
        isa: &'a Isa,
        body: &'a ModuleMirInner,
        func: &'b mut ir::Function,
        dependant_types: &'a FuncTypes,
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
        self.dependant_types[self.body.values[value].ty].ty
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
