use crate::*;
use diags::*;
use lexing_t::*;
use storage::*;

type Types = OrderedMap<Ident, Ty>;
type Defs = PoolMap<Def>;
type Bounds = OrderedMap<Ident, Bound>;
type Impls = PartialOrderedMap<Ident, Impl>;

#[derive(Default)]
pub struct Typec {
    pub types: Types,
    pub defs: Defs,
    pub bounds: Bounds,
    pub impls: Impls,

    pub fields: CachedPoolBumpMap<Field>,
    pub variants: CachedPoolBumpMap<EnumVariant>,
    pub funcs: OrderedMap<Ident, Func>,

    pub ty_lists: PoolBumpMap<VRef<Ty>>,
    pub func_lists: PoolBumpMap<VRef<Func>>,
    pub bound_lists: PoolBumpMap<VRef<Bound>>,

    pub bound_funcs: CachedPoolBumpMap<BoundFunc>,
    pub computed_impls: Map<IdentPair, ()>,
}

impl Typec {
    #[inline]
    pub fn generics_of(&self, ty: VRef<Ty>) -> &[VRef<Bound>] {
        match self.types[ty].kind {
            TyKind::Struct(s) => &self.bound_lists[s.generics],
            TyKind::Enum(e) => &self.bound_lists[e.generics],
            _ => unimplemented!(),
        }
    }

    #[inline]
    pub fn sig_of_func(&self, func: VRef<Func>) -> Sig {
        self.defs[self.funcs[func].def].sig
    }

    #[inline]
    pub fn wrap_def(&mut self, id: Ident, def: VRef<Def>) -> VRef<Func> {
        self.wrap_def_with_params(id, def, None)
    }

    #[inline]
    pub fn wrap_def_with_params(
        &mut self,
        id: Ident,
        def: VRef<Def>,
        params: impl IntoIterator<Item = VRef<Ty>>,
    ) -> VRef<Func> {
        let func_ent = Func {
            def,
            params: self.ty_lists.bump(params),
        };

        self.funcs.insert_unique(id, func_ent)
    }

    pub fn is_incomplete_instance(&self, instance: VRef<Ty>) -> bool {
        match self.types[instance].kind {
            TyKind::Instance(inst) => self.ty_lists[inst.params].contains(&Ty::INFERRED),
            _ => false,
        }
    }

    #[inline]
    pub fn set_instance_param_on(&mut self, instance: VRef<Ty>, pos: usize, param: VRef<Ty>) {
        match self.types[instance].kind {
            TyKind::Instance(inst) => self.ty_lists[inst.params][pos] = param,
            _ => unreachable!(),
        }
    }

    #[inline]
    pub fn instance_base(&self, ty: VRef<Ty>) -> VRef<Ty> {
        match self.types[ty].kind {
            TyKind::Instance(inst) => inst.base,
            _ => ty,
        }
    }

    #[inline]
    pub fn bound_base(&self, bound: VRef<Bound>) -> VRef<Bound> {
        match self.bounds[bound].kind {
            BoundKind::Instance(inst) => inst.base,
            _ => bound,
        }
    }

    #[inline]
    pub fn bound_funcs(&self, bound: VRef<Bound>) -> &[BoundFunc] {
        match self.bounds[bound].kind {
            BoundKind::Base(base) => &self.bound_funcs[base.funcs],
            BoundKind::Instance(inst) => self.bound_funcs(inst.base),
        }
    }

    #[inline]
    pub fn bound_assoc_types(&self, bound: VRef<Bound>) -> &[VRef<Bound>] {
        match self.bounds[bound].kind {
            BoundKind::Base(base) => &self.bound_lists[base.assoc_types],
            BoundKind::Instance(inst) => self.bound_assoc_types(inst.base),
        }
    }

    #[inline]
    pub fn pointer_base(&self, ty: VRef<Ty>) -> VRef<Ty> {
        match self.types[ty].kind {
            TyKind::Ptr(ptr) => ptr.base,
            _ => ty,
        }
    }

    #[inline]
    pub fn absolute_base_of(&self, ty: VRef<Ty>) -> VRef<Ty> {
        self.pointer_base(self.instance_base(ty))
    }

    #[inline]
    pub fn has_param_base(&self, ty: VRef<Ty>) -> bool {
        matches!(
            self.types[self.absolute_base_of(ty)].kind,
            TyKind::Param { .. }
        )
    }

    #[inline]
    pub fn param_index(&self, ty: VRef<Ty>) -> usize {
        match self.types[ty].kind {
            TyKind::Param(index) => index as usize,
            _ => unimplemented!(),
        }
    }

    #[inline]
    pub fn params_of_def(&self, def: VRef<Def>) -> VSlice<VRef<Bound>> {
        self.defs[def].generics
    }

    #[inline]
    pub fn args_of(&self, def: VRef<Def>) -> VSlice<VRef<Ty>> {
        self.defs[def].sig.args
    }

    #[inline]
    pub fn loc_to_diag_loc(&self, loc: Loc, interner: &Interner) -> Maybe<DiagLoc> {
        loc.file
            .expand()
            .map(|file| DiagLoc {
                span: loc.span(interner),
                source: file,
            })
            .into()
    }

    #[inline]
    pub fn is_anon_bound(&self, bound: VRef<Bound>) -> bool {
        self.has_flag(bound, BoundFlags::ANON)
    }

    #[inline]
    pub fn unwrap_anon<'a>(&'a self, bound: &'a [VRef<Bound>; 1]) -> &'a [VRef<Bound>] {
        if self.is_anon_bound(bound[0]) {
            bound
        } else {
            bound
        }
    }
}

pub trait HasFlag<T, F> {
    fn has_flag(&self, item: VRef<T>, flag: F) -> bool;
}

macro_rules! impl_has_flag {
    ($($item:ty, $flag:ty, $storage:ident;)+) => {
        $(
            impl HasFlag<$item, $flag> for Typec {
                fn has_flag(&self, item: VRef<$item>, flag: $flag) -> bool {
                    self.$storage[item].flags.contains(flag)
                }
            }
        )+
    };
}

impl_has_flag!(
    Ty, TyFlags, types;
    Bound, BoundFlags, bounds;
    Def, DefFlags, defs;
);

pub trait LocOf<T> {
    fn loc_of(&self, item: VRef<T>, interner: &Interner) -> Maybe<DiagLoc>;
}

macro_rules! impl_loc_of {
    ($($item:ty, $storage:ident;)+) => {
        $(
            impl LocOf<$item> for Typec {
                fn loc_of(&self, item: VRef<$item>, interner: &Interner) -> Maybe<DiagLoc> {
                    self.loc_to_diag_loc(self.$storage[item].loc, &interner)
                }
            }
        )+
    };
}

impl_loc_of!(
    Ty, types;
    Bound, bounds;
    Def, defs;
    Impl, impls;
);

impl LocOf<Func> for Typec {
    fn loc_of(&self, item: VRef<Func>, interner: &Interner) -> Maybe<DiagLoc> {
        self.loc_to_diag_loc(self.defs[self.funcs[item].def].loc, &interner)
    }
}
