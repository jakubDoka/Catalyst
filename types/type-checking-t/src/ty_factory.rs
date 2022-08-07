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
            kind: TyKind::Instance { base, params },
            flags: self.typec.types[base].flags,
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
        param_count: usize,
        inherits: &[Ty],
        assoc_types: &[Ty],
        funcs: &[BoundFuncEnt],
    ) -> Ty {
        if let Some(ty) = self.typec.types.index(id) {
            return ty;
        }

        let inherits = self.typec.ty_lists.bump_slice(inherits);
        let assoc_types = self.typec.ty_lists.bump_slice(assoc_types);
        let funcs = self.typec.bound_funcs.bump_slice(funcs);
        let ent = TyEnt {
            kind: TyKind::Bound {
                inherits,
                assoc_types,
                funcs,
            },
            param_count: param_count as u8,
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
        let segments = [InternedSegment::from(self.typec.types.id(base)), "[".into()]
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
            .collect::<Vec<_>>();

        self.interner.intern(&segments)
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
}
