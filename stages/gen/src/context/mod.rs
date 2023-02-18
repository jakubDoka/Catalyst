//use std::{
//    alloc::{Allocator, Global},
//    ptr::{self, NonNull},
//};

use cranelift_codegen::ir::{AbiParam, StackSlotData, StackSlotKind};

use {
    crate::*,
    cranelift_codegen::{
        binemit::{CodeOffset, Reloc},
        ir::{self, ArgumentPurpose, ExternalName, LibCall, Type, UserExternalName},
        isa::{self, CallConv, LookupError, TargetIsa},
        settings, CodegenError, Context,
    },
    cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable},
    mir_t::*,
    rkyv::{
        ser::{ScratchSpace, Serializer},
        with::{ArchiveWith, DeserializeWith, SerializeWith},
        Archive, Archived, Deserialize, Fallible, Resolver, Serialize,
    },
    std::{
        alloc,
        default::default,
        iter, mem,
        num::NonZeroU8,
        ops::{Deref, DerefMut, Range},
        sync::Arc,
    },
    storage::{dashmap::mapref::one::Ref, *},
    target_lexicon::{CDataModel, Triple},
    types::*,
};

pub mod layout;

#[derive(Serialize, Deserialize, Archive, Default)]
pub struct GenLookup {
    funcs: CMap<FragRef<&'static str>, CompiledFunc>,
}

impl Relocated for GenLookup {
    fn mark(&self, _: &mut FragRelocMarker) {}

    fn remap(&mut self, ctx: &FragMarks) -> Option<()> {
        self.funcs.remap(ctx);
        Some(())
    }
}

#[derive(Serialize, Archive, Deserialize)]
pub enum ComputedConst {
    Scalar(IRegister),
    //    Memory(ComputedConstMemory),
    Unit,
}

// pub struct ComputedConstMemory {
//     data: *mut u8,
//     layout: alloc::Layout,
// }
//
// impl ComputedConstMemory {
//     pub fn new(layout: alloc::Layout) -> Self {
//         let data = Global.allocate(layout).unwrap().as_ptr().as_mut_ptr();
//         Self { data, layout }
//     }
//
//     pub fn as_ptr(&self) -> *mut u8 {
//         self.data
//     }
// }
//
// impl Drop for ComputedConstMemory {
//     fn drop(&mut self) {
//         unsafe {
//             Global.deallocate(NonNull::new_unchecked(self.data), self.layout);
//         }
//     }
// }

// unsafe impl Send for ComputedConstMemory {}
// unsafe impl Sync for ComputedConstMemory {}

derive_relocated!(
    struct ComputedConst {}
);

#[derive(Serialize, Archive, Deserialize)]
pub struct GenBase {
    lookup: Arc<GenLookup>,
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
            .funcs
            .iter_mut()
            .for_each(|mut item| item.unused = true);
    }

    fn reallocate(&mut self) {
        self.lookup.funcs.retain(|_, v| !v.unused);
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, Archive, Serialize, Deserialize, Hash, PartialEq, Eq, Debug)]
pub struct CompiledFuncRef(pub(super) FragRef<&'static str>);

impl CompiledFuncRef {
    pub fn ident(self) -> FragRef<&'static str> {
        self.0
    }
}

pub struct Gen {
    lookup: Arc<GenLookup>,
}

impl Gen {
    pub fn get_func(&self, id: FragRef<&'static str>) -> Option<CompiledFuncRef> {
        self.lookup.funcs.get(&id).map(|_value| CompiledFuncRef(id))
    }

    pub fn get_func_direct(
        &self,
        id: CompiledFuncRef,
    ) -> Ref<FragRef<&'static str>, CompiledFunc, FvnBuildHasher> {
        self.lookup.funcs.get(&id.0).unwrap()
    }

    pub fn get_or_insert_func(
        &mut self,
        id: FragRef<&'static str>,
        func: FragRef<Func>,
    ) -> CompiledFuncRef {
        self.lookup
            .funcs
            .entry(id)
            .and_modify(|func| func.unused = false)
            .or_insert_with(|| CompiledFunc::new(func));

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

        let mut func = self.lookup.funcs.get_mut(&id.0).unwrap();
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
    pub fn new(func: FragRef<Func>) -> Self {
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
    args: Vec<CtlAbiParam>,
    ret: Option<CtlAbiParam>,
}

impl ArchivedSignature {
    pub fn from_signature(sig: &ir::Signature) -> Self {
        Self {
            cc: sig.call_conv,
            args: sig.params.iter().copied().map(Into::into).collect(),
            ret: sig.returns.first().copied().map(Into::into),
        }
    }

    pub fn into_ir_signature(self) -> ir::Signature {
        ir::Signature {
            params: self.args.into_iter().map(Into::into).collect(),
            returns: self.ret.map(Into::into).into_iter().collect(),
            call_conv: self.cc,
        }
    }
}

#[derive(Archive, Deserialize, Serialize)]
pub struct CtlAbiParam {
    #[with(TypeArchiver)]
    ty: Type,
    #[with(ArgumentPurposeArchiver)]
    purpose: ArgumentPurpose,
}

impl From<AbiParam> for CtlAbiParam {
    fn from(value: AbiParam) -> Self {
        Self {
            ty: value.value_type,
            purpose: value.purpose,
        }
    }
}

impl From<CtlAbiParam> for AbiParam {
    fn from(value: CtlAbiParam) -> Self {
        AbiParam::special(value.ty, value.purpose)
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
    pub func_imports: Map<CompiledFuncRef, (ir::FuncRef, bool, PassSignature)>,
    pub block_stack: Vec<(VRef<BlockMir>, bool, ir::Block)>,
    pub calls: Vec<CompileRequestChild>,
    pub drops: Vec<Range<usize>>,
    pub signature_pool: Vec<(ir::Signature, PassSignature)>,
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

    pub fn reuse_signature(&mut self) -> (ir::Signature, PassSignature) {
        self.signature_pool
            .pop()
            .unwrap_or_else(|| (ir::Signature::new(CallConv::Fast), default()))
    }

    pub fn recycle_signatures<'a>(&mut self, sigs: impl Iterator<Item = &'a mut ir::Signature>) {
        sigs.map(|sig| {
            sig.clear(CallConv::Fast);
            mem::replace(sig, ir::Signature::new(CallConv::Fast))
        })
        .zip(self.func_imports.drain().map(|(.., (.., ps))| ps))
        .collect_into(&mut self.signature_pool);
    }
}

#[derive(Clone, Copy, Default, Debug)]
pub struct GenValue {
    pub computed: Option<ComputedValue>,
    pub offset: i32,
    pub must_load: bool,
}

wrapper_enum! {
    #[derive(Clone, Copy, Debug)]
    enum ComputedValue: {
        Value: ir::Value,
        StackSlot: ir::StackSlot,
        Variable: Variable,
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

//////////////////////////////////
// relocs
//////////////////////////////////

#[derive(Clone, Copy, Serialize, Archive, Deserialize)]

pub struct GenReloc {
    pub offset: CodeOffset,
    #[with(RelocArchiver)]
    pub kind: Reloc,
    pub name: GenItemName,
    pub addend: isize,
}

transmute_arkive! {
    CallConvArchiver(CallConv => u8)
    RelocArchiver(Reloc => u8)
    LibCallArchiver(LibCall => u8)
    TypeArchiver(Type => u16)
    ArgumentPurposeArchiver(ArgumentPurpose => u64)
}

#[derive(Clone, Copy, Debug, Serialize, Archive, Deserialize)]

pub enum GenItemName {
    Func(CompiledFuncRef),
    LibCall(#[with(LibCallArchiver)] ir::LibCall),
}

impl GenItemName {
    pub const FUNC_FLAG: u8 = 0;

    pub fn decode_name(UserExternalName { namespace, index }: UserExternalName) -> GenItemName {
        let (flag, thread) = ((namespace >> 8) as u8, namespace as u8);

        match flag {
            Self::FUNC_FLAG => {
                GenItemName::Func(CompiledFuncRef(FragRef::new(FragAddr::new(index, thread))))
            }
            _ => unreachable!(),
        }
    }

    pub fn encode_func(func: CompiledFuncRef) -> UserExternalName {
        let FragAddr { index, thread, .. } = func.0.addr();
        UserExternalName {
            namespace: thread as u32 | (Self::FUNC_FLAG as u32) << 8,
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
    pub body: FuncMirView<'a>,
    pub struct_ret: Option<ir::Value>,
    pub ret_pass_mode: Option<PassMode>,
    pub dependant_types: &'a PushMapView<MirTy>,
    pub inner: FunctionBuilder<'b>,
}

impl<'a, 'b> GenBuilder<'a, 'b> {
    pub fn new(
        isa: &'a Isa,
        mir_func: FuncMir,
        module: &'a ModuleMir,
        func: &'b mut ir::Function,
        dependant_types: &'a PushMapView<MirTy>,
        ctx: &'b mut FunctionBuilderContext,
    ) -> Self {
        Self {
            isa,
            body: mir_func.view(module),
            struct_ret: None,
            ret_pass_mode: None,
            dependant_types,
            inner: FunctionBuilder::new(func, ctx),
        }
    }

    pub fn value_ty(&self, value: VRef<ValueMir>) -> Ty {
        self.dependant_types[self.body.values[value].ty()].ty
    }

    pub fn ptr_ty(&self) -> Type {
        self.isa.pointer_ty
    }

    pub fn system_cc(&self) -> CallConv {
        self.isa.default_call_conv()
    }

    pub fn create_stack_slot(&mut self, layout: Layout) -> ir::StackSlot {
        self.create_sized_stack_slot(StackSlotData {
            kind: StackSlotKind::ExplicitSlot,
            size: Layout::aligned_to(layout.size, 8),
        })
    }

    pub fn declare_var(&mut self, init: ir::Value, under: VRef<ValueMir>) -> Variable {
        let var = Variable::from_u32(under.as_u32());
        let repr = self.func.dfg.value_type(init);
        self.inner.declare_var(var, repr);
        self.def_var(var, init);
        var
    }

    pub fn finalize(self) {
        self.inner.finalize();
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
