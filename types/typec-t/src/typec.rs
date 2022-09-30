use core::fmt;
use std::{default::default, ops::IndexMut};

use crate::*;
use lexing_t::*;
use storage::*;

#[derive(Default)]
pub struct Typec {
    pub types: Types,
    pub bounds: Bounds,
    pub funcs: Funcs,
    pub fields: Fields,

    pub ty_slices: TySlices,
    pub bound_slices: BoundSlices,
    pub func_slices: FuncSlices,

    pub bound_funcs: BoundFuncs,

    pub builtin_funcs: Vec<VRef<Func>>,
}

macro_rules! assert_init {
    (
        ($self:expr, $interner:expr)
        $(
            $name:ident {
                $($body:tt)*
            }
        )*
    ) => {
        const _: () = {
            let mut index = 0;
            $(
                assert!(Ty::$name.index() == Ty::ALL[index].index());
                index += 1;
            )*
            _ = index;
        };

        $(
            let name = if Ty::$name == Ty::UNIT {
                "()"
            } else if Ty::$name == Ty::INFERRED {
                "{unknown}"
            } else {
                stringify!($name)
            };

            $self.add_builtin_ty(name, Ty {
                $($body)*
                ..Default::default()
            }, $interner);
        )*
    };
}

impl Typec {
    pub fn display_sig(
        &self,
        func: VRef<Func>,
        interner: &Interner,
        buffer: &mut String,
    ) -> fmt::Result {
        let Func { signature, .. } = self.funcs[func];
        use fmt::Write;
        write!(
            buffer,
            "fn {}[todo] {}({}) -> {} ",
            signature
                .cc
                .expand()
                .map(|cc| &interner[cc])
                .map_or(default(), |cc| format!("\"{}\" ", cc)),
            &interner[self.funcs.id(func)],
            self.ty_slices[signature.args]
                .iter()
                .map(|&ty| &interner[self.types.id(ty)])
                .enumerate()
                .map(|(i, str)| format!("var{}: {}", i, str))
                .intersperse(String::from(", "))
                .collect::<String>(),
            &interner[self.types.id(signature.ret)],
        )
    }

    pub fn init_builtin_types(&mut self, interner: &mut Interner) {
        assert_init! {
            (self, interner)
            INFERRED {}
            MUTABLE {}
            IMMUTABLE {}
            UNIT {
                kind: TyKind::Struct(default()),
            }
            UINT {
                kind: TyKind::Integer(default()),
            }
            TERMINAL {}
        }
    }

    pub fn init_builtin_funcs(&mut self, interner: &mut Interner) {
        let anon_temp = interner.intern_str("anon_temp");
        let anon_temp = self.funcs.insert_unique(anon_temp, Default::default());
        assert!(anon_temp == Func::ANON_TEMP);

        let int_bin_ops = "+ - / *".split_whitespace();

        for op in int_bin_ops {
            for &ty in Ty::INTEGERS {
                let op = interner.intern_str(op);
                let segments = self.binary_op_id(op, ty, ty);
                let id = interner.intern(segments);

                let signature = Signature {
                    cc: default(),
                    args: self.ty_slices.bump([ty, ty]),
                    ret: ty,
                };

                let func = self.funcs.insert_unique(
                    id,
                    Func {
                        signature,
                        flags: FuncFlags::BUILTIN,
                        loc: Loc::new(op, None, None, None),
                        ..default()
                    },
                );

                self.builtin_funcs.push(func);
            }
        }
    }

    pub fn add_builtin_ty(&mut self, name: &str, mut ty: Ty, interner: &mut Interner) {
        let lower_name = name.to_lowercase();
        let ident = interner.intern_str(lower_name.as_str());
        ty.loc = Loc::new(ident, None, None, None);
        self.types.insert(ident, ty);
    }

    pub fn binary_op_id(
        &self,
        op: VRef<str>,
        lhs: VRef<Ty>,
        rhs: VRef<Ty>,
    ) -> impl Iterator<Item = InternedSegment<'static>> {
        ident!(self.types.id(lhs), " ", op, " ", self.types.id(rhs)).into_iter()
    }

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

    pub fn tuple_id<'a>(
        &'a self,
        tys: &'a [VRef<Ty>],
    ) -> impl Iterator<Item = InternedSegment<'static>> + 'a {
        let start = ident!("(");
        let body = ident_join(", ", tys.iter().map(|&t| self.types.id(t)));
        let end = ident!(")");

        start.into_iter().chain(body).chain(end)
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
