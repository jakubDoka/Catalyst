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
                    assert_eq!(Self::$name, typec.$field.push(Default::default()));
                )*
            }

            pub fn mark_water_drops(relocator: &mut FragRelocator<Self>) {
                $(
                    relocator.mark(Self::$name);
                )*
            }

            pub fn is_water_drop(s: FragRef<Self>) -> bool {
                s <= Self::WATER_DROPS.last().unwrap().1
            }
        }

        impl Humid for $target {
            fn lookup_water_drop(name: &str) -> Option<FragRef<Self>> {
                lookup_water_drop(&Self::WATER_DROPS, name)
            }
            fn name(&self) -> Ident {
                self.name
            }
            fn storage(typec: &mut Typec) -> &mut FragMap<Self, MAX_FRAGMENT_SIZE> {
                &mut typec.$field
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
    scope::{ModuleItem, ModuleItemPtr, Scope, ScopeError, ScopeFrame, ScopeItem, ScopeRecord},
    tir::{
        AssignTir, BreakTir, CallTir, CallableTir, CastCheck, CtxFrame, CtxFrameItem, FieldTir,
        IfBranchTir, IfTir, Inference, LetTir, LoopHeaderTir, LoopTir, MacroCompileRequest,
        MatchArmTir, MatchTir, PatKindTir, PatTir, TirBuilder, TirBuilderCtx, TirKind, TirNode,
        TypecOutput, UnitPatKindTir, VarHeaderTir,
    },
    ty::{
        ArgSlices, BaseSpecs, Builtin, ComputedTypecItem, Enum, Enums, Field, FieldFlags, Fields,
        GenericTy, Generics, Humid, Impl, ImplKey, ImplLookup, Implemented, Impls, Instance,
        Instances, Mutability, ParamSlices, Pointer, Pointers, Spec, SpecBase, SpecFunc, SpecFuncs,
        SpecInstance, SpecInstances, SpecSet, SpecSums, Struct, Ty, TyFlags, TypecLookup, Variant,
        Variants,
    },
    typec::{
        lookup_water_drop, sorted_water_drops, Loc, MacroImpl, ModuleItems, ParamPresence,
        SpecCmpError, Typec, TypecCtxSlice, TypecRelocator,
    },
};

pub type TyGraph = graphs::ProjectedCycleDetector<Ty>;
