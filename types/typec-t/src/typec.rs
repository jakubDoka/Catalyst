use std::ops::IndexMut;

use crate::*;
use lexing_t::*;
use storage::*;

#[derive(Default)]
pub struct Typec {
    pub types: Types,
    pub bounds: Bounds,
    pub funcs: Funcs,

    pub ty_slices: TySlices,
    pub bound_slices: BoundSlices,
    pub func_slices: FuncSlices,

    pub bound_funcs: BoundFuncs,
}

impl Typec {
    pub fn pointer_id(
        &self,
        mutability: VRef<Ty>,
        base: VRef<Ty>,
    ) -> impl Iterator<Item = InternedSegment<'static>> {
        ident!("^", self.types.id(mutability), " ", self.types.id(base)).into_iter()
    }

    pub fn instance_id<'a>(
        &'a self,
        base: VRef<Ty>,
        params: &'a [VRef<Ty>],
    ) -> impl Iterator<Item = InternedSegment<'static>> + 'a {
        let prefix = ident!(self.types.id(base), "[").into_iter();
        let params = ident_join(", ", params.iter().map(|&p| self.types.id(p)));
        let suffix = ident!("]");
        prefix.chain(params).chain(suffix)
    }

    pub fn bound_instance_id<'a>(
        &'a self,
        base: VRef<Bound>,
        params: &'a [VRef<Ty>],
        assoc_types: &'a [VRef<Ty>],
    ) -> impl Iterator<Item = InternedSegment<'static>> + 'a {
        let prefix = ident!(self.bounds.id(base), "[").into_iter();
        let params = ident_join(
            ", ",
            params.iter().chain(assoc_types).map(|&p| self.types.id(p)),
        );
        let suffix = ident!("]");
        prefix.chain(params).chain(suffix)
    }

    pub fn bound_sum_id<'a>(
        &'a self,
        bounds: &'a [VRef<Bound>],
    ) -> impl Iterator<Item = InternedSegment<'static>> + 'a {
        ident_join(" + ", bounds.iter().map(|&b| self.bounds.id(b)))
    }
}

impl<K: SpecialHash, V> StorageExt<V> for OrderedMap<K, V> {}
impl<V, CACHE> StorageExt<V> for BumpMap<V, CACHE> {}
impl<V, CACHE> StorageExt<V> for PoolBumpMap<V, CACHE> {}

pub trait StorageExt<K>: IndexMut<VRef<K>, Output = K> {
    #[inline]
    fn locate(&self, target: VRef<K>) -> Loc
    where
        Self::Output: Located,
    {
        self[target].loc()
    }

    #[inline]
    fn try_inner<'a, T>(&'a self, target: VRef<K>) -> Option<&'a T>
    where
        K: Variadic,
        &'a K::Kind: TryInto<&'a T>,
    {
        self[target].kind().try_into().ok()
    }

    #[inline]
    fn try_inner_mut<'a, T>(&'a mut self, target: VRef<K>) -> Option<&'a mut T>
    where
        K: Variadic,
        &'a mut K::Kind: TryInto<&'a mut T>,
    {
        self[target].kind_mut().try_into().ok()
    }

    #[inline]
    fn inner<'a, T>(&'a self, target: VRef<K>) -> &'a T
    where
        K: Variadic,
        &'a K::Kind: TryInto<&'a T>,
    {
        self.try_inner(target).unwrap()
    }

    #[inline]
    fn inner_mut<'a, T>(&'a mut self, target: VRef<K>) -> &'a mut T
    where
        K: Variadic,
        &'a mut K::Kind: TryInto<&'a mut T>,
    {
        self.try_inner_mut(target).unwrap()
    }

    #[inline]
    fn flags(&self, target: VRef<K>) -> &<Self::Output as Flagged>::Flags
    where
        Self::Output: Flagged,
    {
        self[target].flags()
    }

    #[inline]
    fn flags_mut(&mut self, target: VRef<K>) -> &mut <Self::Output as Flagged>::Flags
    where
        Self::Output: Flagged,
    {
        self[target].flags_mut()
    }
}

pub trait Variadic: 'static {
    type Kind: 'static;

    fn kind(&self) -> &Self::Kind;
    fn kind_mut(&mut self) -> &mut Self::Kind;
}

pub trait Flagged: 'static {
    type Flags: 'static;
    fn flags(&self) -> &Self::Flags;
    fn flags_mut(&mut self) -> &mut Self::Flags;
}

pub trait Located {
    fn loc(&self) -> Loc;
}
