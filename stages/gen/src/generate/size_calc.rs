use super::*;

impl Generator<'_> {
    pub fn ty_repr(&mut self, ty: Ty, ptr_ty: Type) -> Type {
        self.ty_layout(ty, ptr_ty).repr
    }

    pub fn ty_layout(&mut self, ty: Ty, ptr_ty: Type) -> Layout {
        self.ty_layout_low(ty, &[], ptr_ty)
    }

    fn ty_layout_low(&mut self, ty: Ty, params: &[Ty], ptr_ty: Type) -> Layout {
        if let Some(&layout) = self.gen_layouts.mapping.get(&ty) {
            return layout;
        }

        let res = match ty {
            Ty::Struct(s) => {
                let Struct { fields, .. } = self.typec.structs[s];
                let mut offsets = bumpvec![cap self.typec.fields[fields].len()];

                let layouts = self.typec.fields[fields]
                    .to_bumpvec()
                    .into_iter()
                    .map(|field| self.ty_layout_low(field.ty, params, ptr_ty));

                let mut align = 1;
                let mut size = 0;
                for layout in layouts {
                    align = align.max(layout.align.get());

                    offsets.push(size);

                    let padding = (layout.align.get() - (size as u8 & (layout.align.get() - 1)))
                        & (layout.align.get() - 1);
                    size += padding as u32;
                    size += layout.size;
                }

                let (repr, on_stack) = Self::repr_for_size(size, ptr_ty);

                Layout {
                    repr,
                    offsets: self.gen_layouts.offsets.bump(offsets),
                    align: align.try_into().unwrap(),
                    size,
                    on_stack,
                }
            }
            Ty::Pointer(..) | Ty::Builtin(Builtin::Uint) => Layout {
                repr: ptr_ty,
                offsets: VSlice::empty(),
                align: (ptr_ty.bytes() as u8).try_into().unwrap(),
                size: ptr_ty.bytes() as u32,
                on_stack: false,
            },
            Ty::Builtin(Builtin::Bool) => Layout {
                repr: types::B1,
                offsets: VSlice::empty(),
                align: 1.try_into().unwrap(),
                size: 1,
                on_stack: false,
            },
            Ty::Builtin(b) => {
                let size = b.size();
                let (repr, on_stack) = Self::repr_for_size(size, ptr_ty);
                Layout {
                    size,
                    offsets: VSlice::empty(),
                    align: (size.max(1) as u8).try_into().unwrap(),
                    repr,
                    on_stack,
                }
            }
            Ty::Param(index) => return self.ty_layout_low(params[index as usize], &[], ptr_ty),
            Ty::Instance(inst) => {
                let Instance { base, args } = self.typec[inst];
                // remap the instance parameters so we can compute the layout correctly
                let params = self.typec.args[args]
                    .to_bumpvec()
                    .into_iter()
                    .map(|ty| self.typec.instantiate(ty, params, self.interner))
                    .collect::<BumpVec<_>>();
                return self.ty_layout_low(base.as_ty(), &params, ptr_ty);
            }
            Ty::Enum(ty) => {
                let size = self.typec.get_enum_flag_ty(ty).map_or(0, |ty| ty.size());
                let (base_size, base_align) = self.typec[self.typec[ty].variants]
                    .to_bumpvec()
                    .into_iter()
                    .map(|variant| self.ty_layout_low(variant.ty, params, ptr_ty))
                    .map(|layout| (layout.size, layout.align.get()))
                    .max()
                    .unwrap_or((0, 1));

                let align = base_align.max(size as u8);
                let size = base_size + size.max(align as u32);
                let offsets = [0, (size != 0) as u32 * align as u32];

                let (repr, on_stack) = Self::repr_for_size(size, ptr_ty);
                Layout {
                    size,
                    offsets: self.gen_layouts.offsets.bump(offsets),
                    align: align.try_into().unwrap(),
                    repr,
                    on_stack,
                }
            }
        };

        self.gen_layouts.mapping.insert(ty, res);

        res
    }

    fn repr_for_size(size: u32, ptr_ty: Type) -> (Type, bool) {
        if size > ptr_ty.bytes() as u32 {
            return (ptr_ty, true);
        }

        (
            match size {
                8.. => types::I64,
                4.. => types::I32,
                2.. => types::I16,
                0.. => types::I8,
            },
            false,
        )
    }
}
