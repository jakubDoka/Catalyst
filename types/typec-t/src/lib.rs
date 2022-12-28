#![feature(default_free_fn)]
#![allow(incomplete_features)]
#![feature(specialization)]
#![feature(option_result_contains)]
#![feature(iter_intersperse)]
#![feature(result_flattening)]
#![feature(if_let_guard)]
#![feature(decl_macro)]
#![feature(const_swap)]
#![feature(const_mut_refs)]
#![feature(associated_type_defaults)]
#![feature(slice_group_by)]
#![feature(is_sorted)]
#![feature(try_blocks)]
#![feature(never_type)]

use storage::gen_v_ref_constants;

macro_rules! gen_water_drops {
    (
        $target:ident
        $field:ident
        $(
            $name:ident => $repr:literal,
        )+
    ) => {
        impl $target {
            gen_frag_ref_constants!($($name)+);

            pub const WATER_DROPS: [(&str, FragRef<Self>); [$(Self::$name),*].len()] = sorted_water_drops([
                $(
                    ($repr, Self::$name),
                )*
            ]);

            pub fn init_water_drops(typec: &mut Typec) {
                $(
                    assert_eq!(Self::$name, typec.cache.$field.push(Default::default()));
                )*
            }


        }

        impl Humid for $target {
            const NAMES: &'static [&'static str] = &[$($repr),*];

            fn lookup_water_drop(name: &str) -> Option<FragRef<Self>> {
                lookup_water_drop(&Self::WATER_DROPS, name)
            }
            fn name(&self) -> Ident {
                self.name
            }
            fn storage(typec: &mut Typec) -> &mut FragMap<Self> {
                &mut typec.cache.$field
            }
            fn is_water_drop(s: FragRef<Self>) -> bool {
                s <= *Self::ALL.last().unwrap()
            }
        }
    };
}

mod func;
mod scope;
mod tir;
mod ty;
mod typec;

pub use {
    func::{Func, FuncFlags, FuncSlices, FuncVisibility, Funcs, Signature},
    scope::{
        ModuleItem, ModuleItemPtr, Scope, ScopeError, ScopeFrame, ScopeItem, ScopePosition,
        ScopeRecord,
    },
    tir::{
        AssignTir, BreakTir, CallTir, CallableTir, CastCheck, CtxFrame, CtxFrameItem, FieldTir,
        IfBranchTir, IfTir, Inference, LetTir, LoopHeaderTir, LoopTir, MacroCompileRequest,
        MatchArmTir, MatchTir, PatKindTir, PatTir, TirBuilder, TirBuilderCtx, TirFlags, TirFunc,
        TirKind, TirNode, TypecOutput, UnitPatKindTir, VarHeaderTir,
    },
    ty::{
        Builtin, ComputedTypecItem, Enum, Field, FieldFlags, GenericTy, Generics, Humid, Impl,
        ImplKey, Instance, Mutability, Pointer, RawMutability, Spec, SpecBase, SpecFunc,
        SpecInstance, SpecSet, Struct, Ty, TyFlags, Variant,
    },
    typec::{
        lookup_water_drop, sorted_water_drops, ImplLookup, Implemented, Loc, MacroImpl, Macros,
        Mapping, MayNeedDrop, ModuleItems, ParamPresence, SpecCmpError, Typec, TypecBase,
        TypecCache, TypecCacheBase, TypecCtxSlice, TypecLookup,
    },
};

pub type TyGraph = graphs::ProjectedCycleDetector<Ty>;
