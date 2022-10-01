use std::default::default;

use storage::*;
use typec_t::*;

use crate::*;

impl TyUtils<'_> {
    pub fn nth_param(&mut self, index: usize) -> VRef<Ty> {
        let key = self.interner.intern(ident!("param ", index as u32));

        let fallback = |_: &mut Types| Ty {
            kind: TyKind::Param(index as u32),
            flags: TyFlags::GENERIC,
            loc: default(),
        };

        self.typec.types.get_or_insert(key, fallback)
    }

    pub fn instantiate(&mut self, ty: VRef<Ty>, params: &[VRef<Ty>]) -> VRef<Ty> {
        match self.typec.types[ty].kind {
            TyKind::Instance(TyInstance { base, args }) => {
                let params = self.typec.ty_slices[args]
                    .to_bumpvec()
                    .into_iter()
                    .map(|arg| self.instantiate(arg, params))
                    .collect::<BumpVec<_>>();

                let generic = params.iter().any(|&arg| self.typec.types.is_generic(arg));

                let segments = self.typec.instance_id(base, &params);
                let id = self.interner.intern(segments);

                let fallback = |types: &mut Types| Ty {
                    kind: TyKind::Instance(TyInstance {
                        base,
                        args: self.typec.ty_slices.bump(params),
                    }),
                    flags: TyFlags::GENERIC & generic,
                    ..types[ty]
                };

                self.typec.types.get_or_insert(id, fallback)
            }
            TyKind::Pointer(TyPointer {
                base,
                mutability,
                depth,
            }) => {
                let base = self.instantiate(base, params);
                let mutability = self.instantiate(mutability, params);
                let generic =
                    self.typec.types.is_generic(base) | self.typec.types.is_generic(mutability);

                let segments = self.typec.pointer_id(base, mutability);
                let id = self.interner.intern(segments);

                let fallback = |types: &mut Types| Ty {
                    kind: TyKind::Pointer(TyPointer {
                        base,
                        mutability,
                        depth,
                    }),
                    flags: TyFlags::GENERIC & generic,
                    ..types[ty]
                };

                self.typec.types.get_or_insert(id, fallback)
            }
            TyKind::Param(index) => params[index as usize],

            TyKind::Struct(_) | TyKind::Integer(_) | TyKind::Bool => ty,

            TyKind::Inferred | TyKind::SelfBound => unreachable!(),
        }
    }
}
