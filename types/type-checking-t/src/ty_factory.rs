use std::iter::once;

use crate::*;
use storage::*;

#[macro_export]
macro_rules! ptr_ident {
    ($base:expr) => {
        ident!("^", $base)
    };
}

impl TyFactory<'_> {
    pub fn as_func_param(&mut self, ty: Ty) -> Ty {
        match self.typec.types[ty].kind {
            TyKind::Param { .. } => {
                let prev_id = self.typec.types.id(ty);
                let new_id = self.interner.intern(ident!("unique ", prev_id));

                let copy = self.typec.types[ty];
                self.typec.types.insert(new_id, copy).0
            }

            TyKind::Bound { .. }
            | TyKind::Struct { .. }
            | TyKind::Enum { .. }
            | TyKind::Instance { .. }
            | TyKind::Ptr { .. }
            | TyKind::FuncPtr { .. }
            | TyKind::Int { .. }
            | TyKind::Bool
            | TyKind::Inferrable => unimplemented!(),
        }
    }

    pub fn rehash_ty(&mut self, ty: Ty) -> Ty {
        match self.typec.types[ty].kind {
            TyKind::Instance { base } => {
                let id = self.instance_id_low(
                    base,
                    self.typec.ty_lists[self.typec.types[ty].params]
                        .iter()
                        .copied(),
                );
                let id = self.interner.intern(&id);
                self.typec.types.rehash(id, ty)
            }
            _ => unimplemented!(),
        }
    }

    pub fn pointer_of(&mut self, mutable: bool, base: Ty) -> Ty {
        let base_id = self.typec.types.id(base);
        let id = self.interner.intern(ptr_ident!(base_id));

        if let Some(ty) = self.typec.types.index(id) {
            return ty;
        }

        let depth = self.typec.ptr_depth(base) + 1;
        let ent = TyEnt {
            kind: TyKind::Ptr { base, depth },
            flags: (self.typec.types[base].flags & !TyFlags::BUILTIN)
                | (TyFlags::MUTABLE & mutable),
            ..self.typec.types[base]
        };
        self.typec.types.insert_unique(id, ent)
    }

    pub fn instance(&mut self, base: Ty, params: impl IntoIterator<Item = Ty> + Clone) -> Ty {
        let id = self.instance_id(base, params.clone());

        if let Some(ty) = self.typec.types.index(id) {
            return ty;
        }

        let params = self.typec.ty_lists.bump(params).unwrap();
        let ent = TyEnt {
            kind: TyKind::Instance { base },
            flags: self.typec.types[base].flags,
            params: params.into(),
            ..self.typec.types[base]
        };
        self.typec.types.insert_unique(id, ent)
    }

    pub fn anon_bound_of(&mut self, bounds: &[Ty]) -> Ty {
        let id = self.anon_bound_id(bounds);
        self.bound_of(id, 0, bounds, &[], &[])
    }

    pub fn bound_of(
        &mut self,
        id: Ident,
        assoc_type_count: usize,
        inherits: &[Ty],
        params: &[Ty],
        funcs: &[BoundFuncEnt],
    ) -> Ty {
        if let Some(ty) = self.typec.types.index(id) {
            return ty;
        }

        let inherits = self.typec.ty_lists.bump_slice(inherits);
        let params = self.typec.ty_lists.bump_slice(params);
        let funcs = self.typec.bound_funcs.bump_slice(funcs);
        let ent = TyEnt {
            kind: TyKind::Bound {
                inherits,
                assoc_type_count: assoc_type_count as u32,
                funcs,
            },
            params,
            flags: TyFlags::GENERIC,
            ..TyEnt::default()
        };
        self.typec.types.insert_unique(id, ent)
    }

    pub fn param_of(&mut self, bound: Ty) -> Ty {
        let id = self
            .interner
            .intern(param_ident!(self.typec.types.id(bound)));

        if let Some(ty) = self.typec.types.index(id) {
            return ty;
        }

        let ent = TyEnt {
            kind: TyKind::Param { bound, index: 0 },
            flags: TyFlags::GENERIC,
            ..TyEnt::default()
        };
        self.typec.types.insert_unique(id, ent)
    }

    pub fn anon_bound_id(&mut self, bounds: &[Ty]) -> Ident {
        let segments = bounds
            .iter()
            .map(|&ty| self.typec.types.id(ty))
            .flat_map(|id| [InternedSegment::from(id), " + ".into()])
            .take((bounds.len() * 2).checked_sub(1).unwrap_or(0))
            .collect::<Vec<_>>();
        self.interner.intern(&segments)
    }

    pub fn instance_id(&mut self, base: Ty, params: impl IntoIterator<Item = Ty> + Clone) -> Ident {
        self.interner.intern(&self.instance_id_low(base, params))
    }

    pub fn instance_id_low(
        &self,
        base: Ty,
        params: impl IntoIterator<Item = Ty> + Clone,
    ) -> Vec<InternedSegment<'static>> {
        [InternedSegment::from(self.typec.types.id(base)), "[".into()]
            .into_iter()
            .chain(
                params
                    .clone()
                    .into_iter()
                    .map(|ty| self.typec.types.id(ty))
                    .flat_map(|id| [InternedSegment::from(id), ", ".into()])
                    .take((params.into_iter().count() * 2).checked_sub(1).unwrap_or(0)),
            )
            .chain(once(InternedSegment::from("]")))
            .collect::<Vec<_>>()
    }

    pub fn next_param_of(&mut self, param: Ty) -> Ty {
        let TyKind::Param { index, bound } = self.typec.types[param].kind else {
            unreachable!();
        };

        let bound_id = self.typec.types.id(bound);
        let id = self.interner.intern(ident!(index + 1, bound_id));

        if let Some(next) = self.typec.types.index(id) {
            return next;
        }

        let clone = TyEnt {
            kind: TyKind::Param {
                index: index + 1,
                bound,
            },
            ..self.typec.types[param]
        };
        self.typec.types.insert_unique(id, clone)
    }

    pub fn try_instantiate(&mut self, ty: Ty, inferred_slots: &[Ty]) -> Option<Ty> {
        match self.typec.types[ty].kind {
            TyKind::Param { index, .. }
                if inferred_slots[index as usize] == BuiltinTypes::INFERRED =>
            {
                return None
            }
            TyKind::Param { index, .. } => return Some(inferred_slots[index as usize]),
            TyKind::Instance { base } => {
                let params = self.typec.ty_lists[self.typec.types[ty].params]
                    .to_bumpvec()
                    .into_iter()
                    .map(|ty| self.try_instantiate(ty, inferred_slots))
                    .collect::<Option<BumpVec<_>>>()?;
                return Some(self.instance(base, params.iter().copied()));
            }
            TyKind::Ptr { base, .. } => {
                let mutable = self.typec.types[ty].flags.contains(TyFlags::MUTABLE);
                let base = self.try_instantiate(base, inferred_slots)?;
                return Some(self.pointer_of(mutable, base));
            }
            TyKind::FuncPtr { .. } => {
                // let args = self.typec.ty_lists[sig.args]
                //     .to_bumpvec()
                //     .into_iter()
                //     .map(|ty| self.try_instantiate(ty, inferred_slots))
                //     .collect::<Option<Vec<_>>>()?;
                // let ret = if let Some(ret) = sig.ret.expand() {
                //     Some(self.try_instantiate(ret, inferred_slots)?)
                // } else {
                //     None
                // };
                todo!()
            }

            TyKind::Bound { .. }
            | TyKind::Struct { .. }
            | TyKind::Enum { .. }
            | TyKind::Int { .. }
            | TyKind::Bool
            | TyKind::Inferrable => unreachable!(),
        }
    }
}
