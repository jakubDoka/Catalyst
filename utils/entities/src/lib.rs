//! # Catalyst Enities
//!
//! Crate contains core datastructures used by the catalyst programming language.
//! This crate uses tons of nightly features, keep that in mind.
//!
//! ## Contents
//!
//! Main purpose of this crate is offer paralelized datastructiures with incremental
//! compilation in mind. Things are highly specialized for processing acyclic dependency
//! graphs, but also offers thread local specialized allocators for performant temporary
//! allocations.
//!
//! Some datastructures need to be persistent, and so they implement
//! [rkyv](https://crates.io/crates/rkyv) traits.
//!
//! Package also reexports some crates for convenience of catalyst compiler.
//!
//! More information in respective item docs.

#![allow(incomplete_features)]
#![feature(
    allocator_api,
    new_uninit,
    negative_impls,
    inline_const,
    let_chains,
    slice_from_ptr_range,
    rustc_attrs,
    const_trait_impl,
    const_discriminant,
    never_type,
    default_free_fn,
    iter_collect_into,
    pointer_is_aligned,
    slice_index_methods,
    slice_group_by,
    ptr_metadata,
    auto_traits,
    if_let_guard,
    int_roundings,
    const_option,
    const_slice_index,
    specialization,
    slice_iter_mut_as_mut_slice
)]

/// Whevever `ArckiveWith` needs to be implemented on for example fieldless enum,
/// this macro can implement it by transmuting it to type that implements `Archive`.
///
/// ```rust
/// use catalyst_entities::transmute_arkive;
/// use rkyv::{Archive, Serialize, Deserialize};
///
/// enum Foo {
///    A, B, B,
/// }
///
/// transmute_arkive! {
///    FooArchiver(Foo => u8),
/// }
///
/// #[derive(Archive, Serialize, Deserialize)]
/// struct Bar {
///     #[with(FooArchiver)]
///     foo: Foo,
/// }
/// ```
/// Note that this macro can be missued if type contains indirection, simpli transmuting
/// references and pointers will almost certainly lead to UB upon deserialization.
#[macro_export]
macro_rules! transmute_arkive {
    ($($name:ident($ty:ty => $repr:ty))*) => {
        $(
            transmute_arkive!($name, $ty, $repr);
        )*
    };

    ($name:ident, $ty:ty, $repr:ty) => {
        pub struct $name;

        impl rkyv::with::ArchiveWith<$ty> for $name {
            type Archived = rkyv::Archived<$repr>;

            type Resolver = rkyv::Resolver<$repr>;

            unsafe fn resolve_with(
                field: &$ty,
                pos: usize,
                resolver: Self::Resolver,
                out: *mut Self::Archived,
            ) {
                std::mem::transmute_copy::<_, $repr>(field).resolve(pos, resolver, out)
            }
        }

        impl<S: Serializer + ?Sized> rkyv::with::SerializeWith<$ty, S> for $name {
            fn serialize_with(
                field: &$ty,
                serializer: &mut S,
            ) -> Result<Self::Resolver, <S as rkyv::Fallible>::Error> {
                // SAFETY: ther is none
                unsafe { std::mem::transmute_copy::<_, $repr>(field).serialize(serializer) }
            }
        }

        impl<D: Fallible + ?Sized> rkyv::with::DeserializeWith<rkyv::Archived<$repr>, $ty, D> for $name {
            fn deserialize_with(
                field: &rkyv::Archived<$repr>,
                _deserializer: &mut D,
            ) -> Result<$ty, <D as rkyv::Fallible>::Error> {
                Ok(unsafe { std::mem::transmute_copy::<_, $ty>(field) })
            }
        }
    };
}

mod allocator;
mod bump_vec;
mod frag_map;
mod map;
mod primitives;

pub use {
    crate::bump_vec::{BumpAlloc, BumpAllocRef, BumpVec, ToBumpVec, BUMP_ALLOC},
    allocator::{
        arena::Arena,
        code::{Align, Code, CodeAllocator, CodeGuard, CodeRelocator},
        Allocator, AllocatorFrame,
    },
    arc_swap, dashmap,
    frag_map::{
        addr::{FragAddr, FragSliceAddr, NonMaxError, NonMaxU16, NonMaxU32, NonMaxU64},
        interner::{ident::Ident, Interner, InternerBase},
        relocator::{
            DynFragMap, FragMarks, FragRelocMarker, FragRelocator, Relocated, RelocatedObjects,
        },
        shadow::{ShadowFragBase, ShadowFragMap},
        sync::{FragSliceKey, SyncFragBase, SyncFragBorrow, SyncFragMap, SyncFragView},
        ArcSwapArchiver, Cluster, ClusterBorrow, DashMapArchiver, FragBase, FragMap,
        NoInteriorMutability, SmallVecArchiver,
    },
    map::{CMap, CSet, FvnBuildHasher, Map, Set},
    primitives::{
        CtlOption, FragRef, FragRefSlice, FragSlice, NoShortCircuitCollect, OptFragRef, OptVRef,
        TransposeOption, VRef, VRefDefault, VRefSlice, VSlice,
    },
    smallvec,
};
