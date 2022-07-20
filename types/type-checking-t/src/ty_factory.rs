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

    pub fn instance_id(&mut self, base: Ty, params: &[Ty]) -> Ident {
        let mut segments: Vec<InternedSegment> = vec![];
        segments.push(self.types.ents.id(base).into());
        segments.push("[".into());
        let (&first, rest) = params.split_first().unwrap();
        segments.push(self.types.ents.id(first).into());
        for &ty in rest {
            segments.push(", ".into());
            segments.push(self.types.ents.id(ty).into());
        }
        segments.push("]".into());
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
