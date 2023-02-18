use std::{default::default, iter, mem, ops::Deref, sync::Arc, usize};

use span::*;
use rkyv::{Archive, Deserialize, Serialize};
use storage::*;
use types::*;
use type_creator::TypeCreator;

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
}

impl IsMarked for BodyOwner {
    fn is_marked(&self, marker: &FragRelocMarker) -> bool {
        match *self {
            BodyOwner::Func(f) => marker.is_marked(f),
        }
    }
}

derive_relocated!(enum BodyOwner { Func(f) => f,  });

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
    generics: VRefSlice<TyMir>,
    entry: VRef<BlockMir>,
    module: FragRef<ModuleMir>,
    entities: FuncMirEntities,
}

impl FuncMir {
    pub fn new(
        args: impl IntoIterator<Item = VRef<ValueMir>>,
        ret: VRef<ValueMir>,
        generics: impl IntoIterator<Item = VRef<TyMir>>,
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
            args: &entities.value_args[self.args],
            ret: self.ret,
            entry: self.entry,
            generic_types: &entities.ty_params[self.generics],
            entities,
        }
    }

    pub fn module(&self) -> FragRef<ModuleMir> {
        self.module
    }

    pub fn entry(&self) -> VRef<BlockMir> {
        self.entry
    }
}

pub struct FuncMirView<'a> {
    pub args: &'a [VRef<ValueMir>],
    pub ret: VRef<ValueMir>,
    pub entry: VRef<BlockMir>,
    pub generic_types: &'a [VRef<TyMir>],
    pub entities: FuncMirEntitiesView<'a>,
}

impl<'a> FuncMirView<'a> {
    pub fn value_ty(&self, value: VRef<ValueMir>) -> Ty {
        self.types[self.values[value].ty()].ty
    }
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

            fn roll_back_low(&mut self, check: FuncMirEntities) {
                $(
                    self.$name.truncate(check.$name.range().start);
                )*
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
                pub $name: &'a PushMapView<$ty>,
            )*
        }
    };
}

gen_module! {
    blocks: BlockMir,
    insts: InstMir,
    values: ValueMir,
    value_args: VRef<ValueMir>,
    ty_params: VRef<TyMir>,
    calls: CallMir,
    drops: DropMir,
    types: TyMir,
}

derive_relocated!(struct ModuleMir { calls types });

impl ModuleMir {
    pub fn dummy_func(
        &mut self,
        args: impl IntoIterator<Item = Ty>,
        module: FragRef<ModuleMir>,
        ret: Ty,
    ) -> FuncMir {
        let mut entities = self.check();
        let ret_value = entities.values.push(ValueMir {
            ty: entities.types.push(TyMir { ty: ret }),
        });
        let entry = entities.blocks.push(BlockMir {
            passed: None,
            insts: default(),
            control_flow: ControlFlowMir::Return(ret_value),
            ref_count: 0,
            cycles: 0,
        });
        FuncMir::new(
            args.into_iter()
                .map(|ty| {
                    entities.values.push(ValueMir {
                        ty: entities.types.push(TyMir { ty }),
                    })
                })
                .collect::<BumpVec<_>>(),
            ret_value,
            iter::empty(),
            entry,
            module,
            entities,
        )
    }

    pub fn roll_back(&mut self, body: FuncMir) {
        self.roll_back_low(body.entities);
    }
}

#[derive(Serialize, Deserialize, Archive, Clone, Copy, Debug)]

pub struct DropMir {
    pub value: VRef<ValueMir>,
}

#[derive(Serialize, Deserialize, Archive, Clone, Copy, Debug)]

pub struct TyMir {
    pub ty: Ty,
}

derive_relocated!(struct TyMir { ty });

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

#[derive(Serialize, Deserialize, Archive, Clone, Copy, Debug)]
pub enum InstMir {
    Var(VRef<ValueMir>, VRef<ValueMir>),
    Int(i64, VRef<ValueMir>),
    Float(f64, VRef<ValueMir>),
    Assign(VRef<ValueMir>, VRef<ValueMir>),
    ConstAccess(FragRef<Const>, VRef<ValueMir>),
    Call(VRef<CallMir>, VRef<ValueMir>),
    Ctor(VRefSlice<ValueMir>, VRef<ValueMir>),
    Deref(VRef<ValueMir>, VRef<ValueMir>),
    Ref(VRef<ValueMir>, VRef<ValueMir>),
    Field(VRef<ValueMir>, u32, VRef<ValueMir>),
    Drop(VRef<DropMir>),
}

#[derive(Serialize, Deserialize, Archive, Clone, Copy)]

pub struct CallMir {
    pub callable: CallableMir,
    pub params: VRefSlice<TyMir>,
    pub args: VRefSlice<ValueMir>,
}

derive_relocated!(struct CallMir { callable });

wrapper_enum! {
    #[derive(Serialize, Deserialize, Archive, Clone, Copy, Debug)]
    enum CallableMir: {
        Func: FragRef<Func>,
        SpecFunc: FragRef<SpecFunc>,
        Pointer: VRef<ValueMir>,
    }
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

        gen_flags!((1, Self::FLAGS_LEN, Self::DATA_WIDTH) $($id)*);
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
#[test]
fn test_value_flags() {
    let mut value = ValueMir::new(VRef::new(0));
    assert!(!value.referenced());
    assert!(!value.mutable());
    assert!(!value.var());

    value.mark_referenced();
    assert!(value.referenced());
    assert!(!value.mutable());
    assert!(!value.var());

    value.mark_mutable();
    assert!(value.referenced());
    assert!(value.mutable());
    assert!(!value.var());

    value.mark_var();
    assert!(value.referenced());
    assert!(value.mutable());
    assert!(value.var());
}

#[derive(Serialize, Deserialize, Archive, Clone, Copy)]
#[repr(transparent)]
pub struct ValueMir {
    ty: VRef<TyMir>,
}

impl ValueMir {
    gen_flags! {
        u32
        referenced mark_referenced
        mutable mark_mutable
        var mark_var
    }

    pub fn ty(&self) -> VRef<TyMir> {
        VRef::new(self.ty.as_u32() as usize & ((1 << (Self::DATA_WIDTH - Self::FLAGS_LEN)) - 1))
    }

    pub fn new(ty: VRef<TyMir>) -> Self {
        Self { ty }
    }
}

pub fn swap_mir_types(
    view: &FuncMirView,
    dependant_types: &mut PushMap<TyMir>,
    params: &[Ty],
    mut creator: TypeCreator,
) {
    dependant_types.clear();
    dependant_types.extend(view.types.values().cloned());

    if params.is_empty() {
        return;
    }

    for &mir_ty in view.generic_types {
        let ty = dependant_types[mir_ty].ty;
        let new_ty = creator.instantiate(ty, params);
        dependant_types[mir_ty].ty = new_ty;
    }
}
