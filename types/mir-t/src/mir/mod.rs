/*
    We want destintion to be more precise and known in advance.
    Q: Does the dest need to be separate type?
    Q: Can we make a do with forward destinations?
    Q: How dow we represent destinations? (memory efficiently)

    Lets say we go with forward decl.
    If if control flow returns value, we track both dest and block param.
    Current model does not need to be changed, we simply cedide which path
    to use when generating.

    No.

    What about special dest type?


*/

use std::{
    default::default,
    ops::{Deref, DerefMut},
    sync::Arc,
};

use lexing_t::*;
use rkyv::{Archive, Deserialize, Serialize};
use storage::*;
use typec_t::*;

#[derive(Serialize, Deserialize, Archive)]

pub struct MirBase {
    pub bodies: Arc<CMap<FragRef<Func>, FuncMir>>,
    pub consts: Arc<CMap<FragRef<Const>, ()>>,
    pub modules: SyncFragBase<ModuleMir>,
}

impl MirBase {
    pub fn new(thread_count: u8) -> Self {
        Self {
            bodies: default(),
            consts: default(),
            modules: SyncFragBase::new(thread_count),
        }
    }

    pub fn register<'a>(&'a mut self, objects: &mut RelocatedObjects<'a>) {
        objects.add_root(DashMapFilterUnmarkedKeys::new(&mut self.bodies));
        objects.add(&mut self.modules);
    }

    pub fn split(&self) -> impl Iterator<Item = Mir> + '_ {
        self.modules.split().map(|modules| Mir {
            bodies: self.bodies.clone(),
            modules,
        })
    }
}

pub struct Mir {
    pub bodies: Arc<CMap<FragRef<Func>, FuncMir>>,
    pub modules: SyncFragMap<ModuleMir>,
}

impl Default for Mir {
    fn default() -> Self {
        MirBase::new(1)
            .split()
            .next()
            .expect("since we pass 1 this always resultis into some")
    }
}

#[derive(Serialize, Deserialize, Archive, Clone, Copy)]

pub struct FuncMir {
    pub args: VRefSlice<ValueMir>,
    pub ret: VRef<ValueMir>,
    pub generics: VRefSlice<MirTy>,
    pub types: VSlice<MirTy>,
    pub entry: VRef<BlockMir>,
    pub module: FragRef<ModuleMir>,
    pub calls: VSlice<CallMir>,
    pub drops: VSlice<DropMir>,
    pub values: VSlice<ValueMir>,
}

impl FuncMir {
    pub fn ty(&self, ty: VRef<MirTy>, module: &ModuleMir) -> Ty {
        let Some(index) = ty.index().checked_sub(FuncTypes::BASE_LEN) else {
            return [Ty::UNIT, Ty::TERMINAL][ty.index()];
        };

        module.types[self.types][index].ty
    }

    pub fn value_ty(&self, value: VRef<ValueMir>, module: &ModuleMir) -> Ty {
        self.ty(module.values[value].ty, module)
    }
}

derive_relocated!(struct FuncMir { module });

#[derive(Deserialize, Archive, Serialize, Default, Clone)]
pub struct ModuleMir {
    pub blocks: PushMap<BlockMir>,
    pub insts: PushMap<InstMir>,
    pub values: FuncValues,
    pub value_args: PushMap<VRef<ValueMir>>,
    pub ty_params: PushMap<VRef<MirTy>>,
    pub calls: PushMap<CallMir>,
    pub drops: PushMap<DropMir>,
    types: PushMap<MirTy>,
    value_flags: BitSet,
}

derive_relocated!(struct ModuleMir { calls types });

macro_rules! gen_flags {
    (
        $($id:ident)*
    ) => {
        gen_flags!((0, [$(stringify!($id)),*].len()) $($id)*);
    };

    (
        ($counter:expr, $len:expr) $get:ident $set:ident $($tt:tt)*
    ) => {
        pub fn $get(&self, value: VRef<ValueMir>) -> bool {
            self.value_flags
                .contains(value.index() * $len + $counter)
        }

        pub fn $set(&mut self, value: VRef<ValueMir>) {
            self.value_flags
                .insert(value.index() * $len + $counter);
        }

        gen_flags!(($counter + 1, $len) $($tt)*);
    };

    (($counter:expr, $len:expr)) => {};
}

impl ModuleMir {
    pub fn clear(&mut self) {
        self.blocks.clear();
        self.insts.clear();
        self.values.clear();
        self.value_args.clear();
        self.ty_params.clear();
        self.types.clear();
        self.calls.clear();
        self.drops.clear();
        self.value_flags.clear();
    }

    gen_flags! {
        is_referenced set_referenced
        is_mutable set_mutable
        is_var set_var
    }

    pub fn save_types(&mut self, types: &FuncTypes) -> VSlice<MirTy> {
        self.types.extend(types.values().copied())
    }

    pub fn load_types(&self, types: VSlice<MirTy>) -> &[MirTy] {
        &self.types[types]
    }
}

#[derive(Serialize, Deserialize, Archive, Clone, Copy, Debug)]

pub struct DropMir {
    pub value: VRef<ValueMir>,
}

#[derive(Serialize, Deserialize, Archive, Clone, Copy, Debug)]

pub struct MirTy {
    pub ty: Ty,
}

derive_relocated!(struct MirTy { ty });

impl MirTy {
    gen_v_ref_constants!(
        UNIT
        TERMINAL
    );
}

#[derive(Serialize, Deserialize, Archive, Clone, Copy, Default)]

pub struct BlockMir {
    pub passed: OptVRef<ValueMir>,
    pub insts: VSlice<InstMir>,
    pub control_flow: ControlFlowMir,
    pub ref_count: u16,
    pub cycles: u16,
}

#[derive(Serialize, Deserialize, Archive, Clone, Copy)]

pub enum ControlFlowMir {
    Split {
        cond: VRef<ValueMir>,
        then: VRef<BlockMir>,
        otherwise: VRef<BlockMir>,
    },
    Goto {
        dest: VRef<BlockMir>,
        ret: OptVRef<ValueMir>,
    },
    Return(VRef<ValueMir>),
    Terminal,
}

impl Default for ControlFlowMir {
    fn default() -> Self {
        Self::Return(ValueMir::UNIT)
    }
}

#[derive(Default)]
pub struct DebugData {
    pub instr_spans: ShadowMap<InstMir, Span>,
    pub block_closers: ShadowMap<BlockMir, Span>,
}

impl DebugData {
    pub fn clear(&mut self) {
        //self.instr_spans.clear();
        //self.block_closers.clear();
    }
}

#[derive(Serialize, Deserialize, Archive, Clone, Copy)]

pub enum InstMir {
    Var(VRef<ValueMir>, VRef<ValueMir>),
    Int(i64, VRef<ValueMir>),
    Float(f64, VRef<ValueMir>),
    Access(VRef<ValueMir>, OptVRef<ValueMir>),
    ConstAccess(FragRef<Const>, VRef<ValueMir>),
    Call(VRef<CallMir>, VRef<ValueMir>),
    Ctor(VRefSlice<ValueMir>, VRef<ValueMir>, bool),
    Deref(VRef<ValueMir>, VRef<ValueMir>),
    Ref(VRef<ValueMir>, VRef<ValueMir>),
    Field(VRef<ValueMir>, u32, VRef<ValueMir>),
    Drop(VRef<DropMir>),
}

#[derive(Serialize, Deserialize, Archive, Clone, Copy)]

pub struct CallMir {
    pub callable: CallableMir,
    pub params: VRefSlice<MirTy>,
    pub args: VRefSlice<ValueMir>,
}

derive_relocated!(struct CallMir { callable });

#[derive(Serialize, Deserialize, Archive, Clone, Copy, Debug)]

pub enum CallableMir {
    Func(FragRef<Func>),
    SpecFunc(FragRef<SpecFunc>),
    Pointer(VRef<ValueMir>),
}

derive_relocated! {
    enum CallableMir {
        Func(f) => f,
        SpecFunc(f) => f,
        Pointer(..) =>,
    }
}

#[derive(Serialize, Deserialize, Archive, Clone, Copy)]

pub struct ValueMir {
    pub ty: VRef<MirTy>,
}

impl VRefDefault for ValueMir {
    fn default_state() -> VRef<Self> {
        Self::UNIT
    }
}

impl ValueMir {
    gen_v_ref_constants!(
        UNIT
        TERMINAL
    );
}

#[derive(Serialize, Deserialize, Archive, Clone, Copy)]

pub struct DestMir;

#[derive(Serialize, Deserialize, Archive, Clone)]

pub struct FuncTypes(PushMap<MirTy>);

impl FuncTypes {
    const BASE_LEN: usize = 2;

    pub fn new() -> Self {
        let mut pm = PushMap::new();
        pm.push(MirTy { ty: Ty::UNIT });
        pm.push(MirTy { ty: Ty::TERMINAL });
        Self(pm)
    }

    pub fn clear(&mut self) {
        self.0.truncate(Self::BASE_LEN);
    }

    pub fn values(&self) -> impl Iterator<Item = &MirTy> {
        self.0.values().skip(Self::BASE_LEN)
    }
}

impl Relocated for FuncTypes {
    fn mark(&self, marker: &mut FragRelocMarker) {
        for ty in self.0.values().skip(Self::BASE_LEN) {
            ty.ty.mark(marker);
        }
    }

    fn remap(&mut self, ctx: &FragRelocMapping) -> Option<()> {
        for ty in self.0.values_mut().skip(Self::BASE_LEN) {
            ty.ty
                .remap(ctx)
                // .expect("if we get to this point then type should be reclocable")
            ;
        }

        Some(())
    }
}

impl Default for FuncTypes {
    fn default() -> Self {
        Self::new()
    }
}

impl Deref for FuncTypes {
    type Target = PushMap<MirTy>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for FuncTypes {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Serialize, Deserialize, Archive, Clone)]

pub struct FuncValues(PushMap<ValueMir>);

impl FuncValues {
    pub fn new() -> Self {
        let mut pm = PushMap::new();
        pm.push(ValueMir { ty: MirTy::UNIT });
        pm.push(ValueMir {
            ty: MirTy::TERMINAL,
        });
        Self(pm)
    }

    pub fn clear(&mut self) {
        self.0.truncate(ValueMir::TERMINAL.index() + 1);
    }
}

impl Default for FuncValues {
    fn default() -> Self {
        Self::new()
    }
}

impl Deref for FuncValues {
    type Target = PushMap<ValueMir>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for FuncValues {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
