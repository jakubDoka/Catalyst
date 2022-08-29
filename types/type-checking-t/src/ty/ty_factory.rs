use crate::*;
use storage::*;

#[macro_export]
macro_rules! ptr_ident {
    ($base:expr) => {
        ident!("^", $base)
    };
}

impl TyFactory<'_> {
    pub fn as_func_param(&mut self, ty: VRef<Ty>) -> VRef<Ty> {
        match self.typec.types[ty].kind {
            TyKind::Param(..) => {
                let prev_id = self.typec.types.id(ty);
                let new_id = self.interner.intern(ident!("unique ", prev_id));

                let copy = self.typec.types[ty];
                self.typec.types.insert(new_id, copy).0
            }

            _ => unimplemented!(),
        }
    }

    pub fn rehash_ty(&mut self, ty: VRef<Ty>) -> VRef<Ty> {
        match self.typec.types[ty].kind {
            TyKind::Instance(inst) => {
                let id = self.instance_id_low(inst.base, &self.typec.ty_lists[inst.params]);
                let id = self.interner.intern(id);
                self.typec.types.rehash(id, ty)
            }
            _ => unimplemented!(),
        }
    }

    pub fn pointer_of(&mut self, mutable: bool, base: VRef<Ty>) -> VRef<Ty> {
        let base_id = self.typec.types.id(base);
        let id = self.interner.intern(ptr_ident!(base_id));

        if let Some(ty) = self.typec.types.index(id) {
            return ty;
        }

        let ent = if let TyKind::Ptr(ptr) = self.typec.types[base].kind {
            Ty {
                kind: TyPtr {
                    depth: ptr.depth + 1,
                    ..ptr
                }
                .into(),
                ..self.typec.types[base]
            }
        } else {
            Ty {
                kind: TyPtr { base, depth: 1 }.into(),
                flags: (self.typec.types[base].flags & !TyFlags::BUILTIN)
                    | (TyFlags::MUTABLE & mutable),
                ..self.typec.types[base]
            }
        };

        self.typec.types.insert_unique(id, ent)
    }

    pub fn instance(&mut self, base: VRef<Ty>, params: &[VRef<Ty>]) -> VRef<Ty> {
        let id = self.instance_id(base, params);

        if let Some(ty) = self.typec.types.index(id) {
            return ty;
        }

        let params = self.typec.ty_lists.bump_slice(params);
        let ent = Ty {
            kind: TyInstance { base, params }.into(),
            flags: self.typec.types[base].flags,
            ..self.typec.types[base]
        };
        self.typec.types.insert_unique(id, ent)
    }

    pub fn instance_id(&mut self, base: VRef<Ty>, params: &[VRef<Ty>]) -> Ident {
        self.interner.intern(self.instance_id_low(base, params))
    }

    pub fn instance_id_low(
        &self,
        base: VRef<Ty>,
        params: &[VRef<Ty>],
    ) -> BumpVec<InternedSegment<'static>> {
        let head = ident!(self.typec.types.id(base), "[").into_iter();

        let Some((&first, other)) = params.split_first() else {
            unreachable!();
        };

        let first = ident!(self.typec.types.id(first)).into_iter();
        let others = other
            .iter()
            .flat_map(|&ty| ident!(", ", self.typec.types.id(ty)));
        let tail = ident!("]").into_iter();
        head.chain(first).chain(others).chain(tail).collect()
    }

    pub fn next_param_of(&mut self, param: VRef<Ty>) -> VRef<Ty> {
        let TyKind::Param(index) = self.typec.types[param].kind else {
            unreachable!();
        };

        let id = self.interner.intern(ident!("param ", index + 1));

        if let Some(next) = self.typec.types.index(id) {
            return next;
        }

        let clone = Ty {
            kind: TyKind::Param(index + 1),
            ..self.typec.types[param]
        };
        self.typec.types.insert_unique(id, clone)
    }

    pub fn try_instantiate(
        &mut self,
        ty: VRef<Ty>,
        inferred_slots: &[VRef<Ty>],
    ) -> Option<VRef<Ty>> {
        match self.typec.types[ty].kind {
            TyKind::Param(index) if inferred_slots[index as usize] == Ty::INFERRED => return None,
            TyKind::Param(index) => return Some(inferred_slots[index as usize]),
            TyKind::Instance(inst) => {
                let params = self.typec.ty_lists[inst.params]
                    .to_bumpvec()
                    .into_iter()
                    .map(|ty| self.try_instantiate(ty, inferred_slots))
                    .collect::<Option<BumpVec<_>>>()?;
                return Some(self.instance(inst.base, &params));
            }
            TyKind::Ptr(ptr) => {
                let mutable = self.typec.has_flag(ty, TyFlags::MUTABLE);
                let base = self.try_instantiate(ptr.base, inferred_slots)?;
                return Some(self.pointer_of(mutable, base));
            }
            TyKind::FuncPtr(..) => {
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

            _ => None,
        }
    }
}
