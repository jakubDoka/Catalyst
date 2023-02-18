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
#![feature(trivial_bounds)]

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

            pub fn init_water_drops(types: &mut Types) {
                $(
                    assert_eq!(Self::$name, types.cache.$field.push(Default::default()));
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
            fn storage(types: &mut Types) -> &mut FragMap<Self> {
                &mut types.cache.$field
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
mod cache;

pub use {
    func::{Func, FuncFlags, FuncSlices, FuncVisibility, Funcs, Signature},
    graphs::ProjectedCycleDetector,
    scope::{
        ModuleItem, ModuleItemPtr, Scope, ScopeError, ScopeFrame, ScopeItem, ScopePosition,
        ScopeRecord,
    },
    tir::{
        AssignTir, BreakTir, CallTir, CallableTir, CastCheck, FieldTir, IfBranchTir, IfTir,
        Inference, LetTir, LoopHeaderTir, LoopTir, MacroCompileRequest, MatchArmTir, MatchTir,
        PatKindTir, PatTir, TirFlags, TirFunc, TirKind, TirNode, TypecOutput, UnitPatKindTir,
        VarHeaderTir,
    },
    ty::{
        Array, ArraySize, Builtin, ComputedTypecItem, Const, Enum, Field, FieldFlags, GenericTy,
        Generics, Humid, Impl, ImplKey, Instance, Mutability, Pointer, RawMutability, Spec,
        SpecBase, SpecFunc, SpecInstance, SpecSet, Struct, Ty, TyFlags, Variant,
    },
    cache::{
        lookup_water_drop, sorted_water_drops, ConstFolder, ConstFolderContext, FolderValue,
        GuaranteedLoc, ImplList, ImplLookup, Implemented, Loc, MacroImpl, Macros, Mapping,
        MayNeedDrop, ModuleItems, ParamPresence, SpecCmpError, Types, TypecBase, TypeCache,
        TypecCacheBase, TypecCtxSlice, TypecLookup,
    },
};