use std::{default::default, mem, ops::Deref, sync::Arc, usize};

use lexing_t::*;
use rkyv::{Archive, Deserialize, Serialize};
use storage::*;
use typec_t::*;

#[derive(Serialize, Deserialize, Archive)]

pub struct MirBase {
    pub bodies: Arc<CMap<BodyOwner, FuncMir>>,
    pub modules: SyncFragBase<ModuleMir>,
}

impl MirBase {
    pub fn new(thread_count: u8) -> Self {
        Self {
            bodies: default(),
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

#[derive(Serialize, Deserialize, Archive, Hash, Eq, PartialEq, Clone, Copy)]
#[archive_attr(derive(Hash, Eq, PartialEq))]
pub enum BodyOwner {
    Func(FragRef<Func>),
    Const(FragRef<Const>),
}

impl IsMarked for BodyOwner {
    fn is_marked(&self, marker: &FragRelocMarker) -> bool {
        match *self {
            BodyOwner::Func(f) => marker.is_marked(f),
            BodyOwner::Const(c) => marker.is_marked(c),
        }
    }
}

derive_relocated!(enum BodyOwner { Func(f) => f, Const(c) => c, });

pub struct Mir {
    pub bodies: Arc<CMap<BodyOwner, FuncMir>>,
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
    args: VRefSlice<ValueMir>,
    ret: VRef<ValueMir>,
    generics: VRefSlice<MirTy>,
    entry: VRef<BlockMir>,
    module: FragRef<ModuleMir>,
    entities: FuncMirEntities,
}

impl FuncMir {
    pub fn new(
        args: impl IntoIterator<Item = VRef<ValueMir>>,
        ret: VRef<ValueMir>,
        generics: impl IntoIterator<Item = VRef<MirTy>>,
        entry: VRef<BlockMir>,
        module: FragRef<ModuleMir>,
        mut entities: ModuleMirCheck,
    ) -> Self {
        Self {
            args: entities.value_args.extend(args),
            ret,
            generics: entities.ty_params.extend(generics),
            entry,
            module,
            entities: entities.finish(),
        }
    }

    pub fn view<'a>(&self, module: &'a ModuleMir) -> FuncMirView<'a> {
        let entities = self.entities.view(module);

        FuncMirView {
            args: entities.value_args.get_slice(self.args),
            ret: self.ret,
            generics: entities.ty_params.get_slice(self.generics),
            entities,
        }
    }

    pub fn module(&self) -> FragRef<ModuleMir> {
        self.module
    }
}

pub struct FuncMirView<'a> {
    pub args: &'a [VRef<ValueMir>],
    pub ret: VRef<ValueMir>,
    pub generics: &'a [VRef<MirTy>],
    pub entities: FuncMirEntitiesView<'a>,
}

impl<'a> Deref for FuncMirView<'a> {
    type Target = FuncMirEntitiesView<'a>;

    fn deref(&self) -> &Self::Target {
        &self.entities
    }
}

derive_relocated!(struct FuncMir { module });

macro_rules! gen_module {
    (
        $(
            $name:ident: $ty:ty,
        )*
    ) => {
        #[derive(Deserialize, Archive, Serialize, Default, Clone)]
        pub struct ModuleMir {
            $(
                $name: PushMap<$ty>,
            )*
        }

        impl ModuleMir {
            pub fn clear(&mut self) {
                $(
                    self.$name.clear();
                )*
            }

            pub fn check(&mut self) -> ModuleMirCheck {
                ModuleMirCheck {
                    $(
                        $name: PushMapCheck::new(&mut self.$name),
                    )*
                }
            }
        }

        pub struct ModuleMirCheck<'a> {
            $(
                pub $name: PushMapCheck<'a, $ty>,
            )*
        }

        impl ModuleMirCheck<'_> {
            pub fn finish(self) -> FuncMirEntities {
                FuncMirEntities {
                    $(
                        $name: self.$name.finish(),
                    )*
                }
            }
        }

        #[derive(Serialize, Deserialize, Archive, Clone, Copy)]
        pub struct FuncMirEntities {
            $(
                $name: VSlice<$ty>,
            )*
        }

        impl FuncMirEntities {
            pub fn view<'a>(&self, module: &'a ModuleMir) -> FuncMirEntitiesView<'a> {
                FuncMirEntitiesView {
                    $(
                        $name: PushMapView::new(&module.$name, self.$name),
                    )*
                }
            }
        }

        pub struct FuncMirEntitiesView<'a> {
            $(
                pub $name: PushMapView<'a, $ty>,
            )*
        }
    };
}

gen_module! {
    blocks: BlockMir,
    insts: InstMir,
    values: ValueMir,
    value_args: VRef<ValueMir>,
    ty_params: VRef<MirTy>,
    calls: CallMir,
    drops: DropMir,
    types: MirTy,
}

derive_relocated!(struct ModuleMir { calls types });

#[derive(Serialize, Deserialize, Archive, Clone, Copy, Debug)]

pub struct DropMir {
    pub value: VRef<ValueMir>,
}

#[derive(Serialize, Deserialize, Archive, Clone, Copy, Debug)]

pub struct MirTy {
    pub ty: Ty,
}

derive_relocated!(struct MirTy { ty });

#[derive(Serialize, Deserialize, Archive, Clone, Copy)]
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

macro_rules! gen_flags {
    (
        $repr:ident $($id:ident)*
    ) => {
        const FLAGS_LEN: usize = [$(stringify!($id)),*].len();
        const DATA_WIDTH: usize = mem::size_of::<$repr>() * 8;

        gen_flags!((0, Self::FLAGS_LEN, Self::DATA_WIDTH) $($id)*);
    };

    (
        ($counter:expr, $len:expr, $width:expr) $get:ident $set:ident $($tt:tt)*
    ) => {
        pub fn $get(&self) -> bool {
            self.ty.as_u32() & (1 << ($width - $counter)) != 0
        }

        pub fn $set(&mut self) {
            let mut ty = self.ty.as_u32();
            ty |= 1 << ($width - $counter);
            self.ty = VRef::new(ty as usize);
        }

        gen_flags!(($counter + 1, $len, $width) $($tt)*);
    };

    (($($tt:tt)*)) => {};
}

#[derive(Serialize, Deserialize, Archive, Clone, Copy)]
#[repr(transparent)]
pub struct ValueMir {
    ty: VRef<MirTy>,
}

impl ValueMir {
    gen_flags! {
        u32
        referenced mark_referenced
        mutable mark_mutable
        var mark_var
    }

    pub fn ty(&self) -> VRef<MirTy> {
        VRef::new(self.ty.as_u32() as usize & ((1 << (Self::DATA_WIDTH - Self::FLAGS_LEN)) - 1))
    }
}
