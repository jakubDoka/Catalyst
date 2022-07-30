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
        let base_id = self.types.ents.id(base);
        let id = self.interner.intern(ptr_ident!(base_id));

        if let Some(ty) = self.types.ents.index(id) {
            return ty;
        }

        let depth = self.types.ptr_depth(base) + 1;
        let ent = TyEnt {
            kind: TyKind::Ptr { base, depth },
            flags: (self.types.ents[base].flags & !TyFlags::BUILTIN) | (TyFlags::MUTABLE & mutable),
            ..self.types.ents[base]
        };
        self.types.ents.insert_unique(id, ent)
    }

    pub fn instance(&mut self, base: Ty, params: &[Ty]) -> Ty {
        let id = self.instance_id(base, params);

        if let Some(ty) = self.types.ents.index(id) {
            return ty;
        }

        let params = self.types.slices.bump_slice(params).unwrap();
        let ent = TyEnt {
            kind: TyKind::Instance { base, params },
            flags: self.types.ents[base].flags,
            ..self.types.ents[base]
        };
        self.types.ents.insert_unique(id, ent)
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
        if let Some(ty) = self.types.ents.index(id) {
            return ty;
        }

        let inherits = self.types.slices.bump_slice(inherits);
        let assoc_types = self.types.slices.bump_slice(assoc_types);
        let funcs = self.types.funcs.bump_slice(funcs);
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
        self.types.ents.insert_unique(id, ent)
    }

    pub fn param_of(&mut self, bound: Ty) -> Ty {
        let id = self
            .interner
            .intern(param_ident!(self.types.ents.id(bound)));

        if let Some(ty) = self.types.ents.index(id) {
            return ty;
        }

        let ent = TyEnt {
            kind: TyKind::Param { bound, index: 0 },
            flags: TyFlags::GENERIC,
            ..TyEnt::default()
        };
        self.types.ents.insert_unique(id, ent)
    }

    pub fn anon_bound_id(&mut self, bounds: &[Ty]) -> Ident {
        let segments = bounds
            .iter()
            .map(|&ty| self.types.ents.id(ty))
            .flat_map(|id| [InternedSegment::from(id), " + ".into()])
            .take((bounds.len() * 2).checked_sub(1).unwrap_or(0))
            .collect::<Vec<_>>();
        self.interner.intern(&segments)
    }

    pub fn instance_id(&mut self, base: Ty, params: &[Ty]) -> Ident {
        let segments = [InternedSegment::from(self.types.ents.id(base)), "[".into()]
            .into_iter()
            .chain(
                params
                    .iter()
                    .map(|&ty| self.types.ents.id(ty))
                    .flat_map(|id| [InternedSegment::from(id), ", ".into()])
                    .take((params.len() * 2).checked_sub(1).unwrap_or(0)),
            )
            .chain(once(InternedSegment::from("]")))
            .collect::<Vec<_>>();

        self.interner.intern(&segments)
    }

    pub fn next_param_of(&mut self, param: Ty) -> Ty {
        let TyKind::Param { index, bound } = self.types.ents[param].kind else {
            unreachable!();
        };

        let bound_id = self.types.ents.id(bound);
        let id = self.interner.intern(ident!(index + 1, bound_id));

        if let Some(next) = self.types.ents.index(id) {
            return next;
        }

        let clone = TyEnt {
            kind: TyKind::Param {
                index: index + 1,
                bound,
            },
            ..self.types.ents[param]
        };
        self.types.ents.insert_unique(id, clone)
    }
}
