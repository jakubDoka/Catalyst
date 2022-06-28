use cranelift_codegen::ir::{self, Type};

use crate::*;
use instance_types::*;
use storage::*;
use typec_types::{ty_factory::prepare_params, *};

impl<'a> ReprInstancing<'a> {
    pub fn load_generic_types(
        &mut self,
        params: TyList,
        subs: TyList,
        types: TyList,
        replace_cache: &mut ReplaceCache,
    ) {
        let mut types = self.vec_pool.alloc(self.ty_lists.get(types));

        // for &ty in types.iter() {
        //     print!("{} ", ty_display!(self, ty));
        // }
        // println!();
        let params = self.vec_pool.alloc(self.ty_lists.get(params));
        let subs = self.vec_pool.alloc(self.ty_lists.get(subs));

        // for (&a, &b) in params.iter().zip(subs.iter()) {
        //     print!("({} {}) ", ty_display!(self, a), ty_display!(self, b));
        // }
        // println!();

        prepare_params(&subs, self.types);

        let mut new_types = self.vec_pool.alloc_iter(
            types
                .iter()
                .map(|&ty| self.instantiate_repr(params.as_slice(), subs.as_slice(), ty)),
        );

        // this is done like this because there is no guarantee that
        // for all a, b in P is a not in b and b not in a, where P are `params`
        for (ty, new_ty) in types.drain(..).zip(new_types.drain(..)) {
            // print!("{} -> {} ", ty_display!(self, ty), ty_display!(self, new_ty));
            replace_cache.save(new_ty, ty, self.types, self.reprs);
        }
        // println!();
    }

    pub fn instantiate_repr(&mut self, params: &[Ty], subs: &[Ty], ty: Ty) -> Ty {
        let mut new_instances = self.vec_pool.get();
        let (result, _) = ty_factory!(self).instantiate_recur(ty, params, subs, &mut new_instances);

        // the types are sorted by dependance (leafs first)
        for &instance in new_instances.iter() {
            layout_builder!(self).build_size(instance);
        }

        build_reprs(self.ptr_ty, self.reprs, new_instances.drain(..));

        result
    }
}

impl<'a> LayoutBuilder<'a> {
    pub fn build(&mut self, order: impl IntoIterator<Item = Ty>) {
        self.reprs.resize(self.types.len());
        for id in order {
            self.build_size(id);
        }
    }

    pub fn build_size(&mut self, id: Ty) {
        self.build_size_low(id, TyList::default());
    }

    pub fn build_size_low(&mut self, id: Ty, params: TyList) {
        let ty = &self.types[id];
        if ty.flags.contains(TyFlags::GENERIC)
            || (ty.flags.contains(TyFlags::BUILTIN) && !matches!(ty.kind, TyKind::Struct(..)))
        {
            return;
        }

        match ty.kind {
            TyKind::Instance(base, params) => {
                let TyEnt { kind, .. } = self.types[base];
                match kind {
                    TyKind::Struct(fields) => {
                        self.resolve_struct_repr(id, base, params, fields);
                    }
                    _ => unimplemented!("{kind:?}"),
                }
            }
            TyKind::Struct(fields) => {
                self.resolve_struct_repr(id, id, params, fields);
            }
            TyKind::Ptr(..) | TyKind::FuncPtr(..) => {
                self.reprs[id].layout = Layout::PTR;
                self.reprs[id].flags = ReprFlags::COPYABLE;
            }
            TyKind::Enum(ty, variants) => {
                self.resolve_enum_repr(id, ty, params, variants);
            }
            kind => unimplemented!("{kind:?}"),
        };
    }

    pub fn true_type(&self, ty: Ty, subs: &[Ty], params: TyList) -> Ty {
        let TyEnt { kind, flags, .. } = self.types[ty];

        if !flags.contains(TyFlags::GENERIC) {
            return ty;
        }

        match kind {
            TyKind::Param(index, ..) => self.ty_lists.get(params)[index as usize],
            TyKind::Ptr(base, _) => {
                let base = self.true_type(base, subs, params);
                let mutable = flags.contains(TyFlags::MUTABLE);
                let ptr_id = ID::pointer(self.types[base].id, mutable);
                // println!("{}", ty_display!(self, base));
                self.ty_instances.get(ptr_id).unwrap().clone()
            }
            TyKind::Instance(base, params) => {
                let mut id = ID::new("<instance>") + self.types[base].id;
                for &param in self.ty_lists.get(params) {
                    let param = self.true_type(param, subs, params);
                    id = id + self.types[param].id;
                }
                self.ty_instances.get(id).unwrap().clone()
            }
            kind => unimplemented!("{kind:?}"),
        }
    }

    pub fn resolve_enum_repr(
        &mut self,
        ty: Ty,
        discriminant_ty: Ty,
        _params: TyList,
        fields: TyCompList,
    ) {
        let (layout, copyable) = self
            .ty_comps
            .get(fields)
            .iter()
            .map(|field| {
                (
                    self.reprs[field.ty].layout,
                    self.reprs[field.ty].flags.contains(ReprFlags::COPYABLE),
                )
            })
            .fold(
                (Layout::ZERO, true),
                |(acc_layout, acc_copyable), (layout, copyable)| {
                    (layout.max(acc_layout), copyable & acc_copyable)
                },
            );

        let (offset, layout) = {
            let discriminant_layout = self.reprs[discriminant_ty].layout;
            let size = layout.size();
            let align = layout.align().max(discriminant_layout.align());
            let offset = discriminant_layout.size().max(align);
            let layout = Layout::new(size + offset, align);
            (offset, layout)
        };

        self.reprs[ty].layout = layout;
        let fields = self.repr_fields.push(&[
            ReprField {
                offset: Offset::ZERO,
                ty: discriminant_ty,
            },
            ReprField { offset, ty },
        ]);

        self.reprs[ty] = ReprEnt {
            repr: ir::types::INVALID,
            layout,
            fields,
            flags: ReprFlags::COPYABLE & copyable,
        };
    }

    pub fn resolve_struct_repr(&mut self, ty: Ty, base: Ty, params: TyList, fields: TyCompList) {
        let fields = self.ty_comps.get(fields);
        // println!("resolve_struct_repr: {}", ty_display!(self, ty));

        let subs = collect_ty_params(base, self.types, self.vec_pool, self.builtin_types);

        let align = fields
            .iter()
            .map(|field| {
                self.reprs[self.true_type(field.ty, subs.as_slice(), params)]
                    .layout
                    .size()
            })
            .fold(Offset::ZERO, |acc, align| acc.max(align).min(Offset::PTR));

        let mut size = Offset::ZERO;
        let mut copyable = true;
        for &field in fields.iter() {
            let field_ty = self.true_type(field.ty, subs.as_slice(), params);
            let ent = &self.reprs[field_ty];

            assert_ne!(
                ent.layout,
                Layout::ZERO,
                "zero layout for {} {} {}",
                ty_display!(self, field_ty),
                field_ty,
                field.name.log(self.sources),
            );

            copyable &= ent.flags.contains(ReprFlags::COPYABLE);

            let field = ReprField {
                offset: size,
                ty: field_ty,
            };

            self.repr_fields.push_one(field);

            size = size + ent.layout.size();
            let padding = align - size % align;
            if padding != align {
                size = size + padding;
            }
        }
        let fields = self.repr_fields.close_frame();

        let flags = ReprFlags::COPYABLE & copyable;
        self.reprs[ty] = ReprEnt {
            repr: ir::types::INVALID,
            flags,
            fields,
            layout: Layout::new(size, align),
        };
    }
}

pub fn smallest_repr_for(size: Offset, ptr_ty: Type) -> (Type, bool) {
    let size = size.arch(ptr_ty.bytes() == 4);
    if size > ptr_ty.bytes() as i32 {
        return (ptr_ty, true);
    }
    let repr = match size {
        0 => ir::types::INVALID,
        1 => ir::types::I8,
        2 => ir::types::I16,
        3..=4 => ir::types::I32,
        5..=8 => ir::types::I64,
        _ => unreachable!(),
    };
    (repr, false)
}

pub fn build_builtin_reprs(ptr_ty: Type, reprs: &mut Reprs, builtin_types: &BuiltinTypes) {
    macro_rules! gen {
        ($(($ty:ident, $repr:expr, $layout:expr),)*) => {
            $(
                reprs[builtin_types.$ty] = ReprEnt {
                    layout: $layout,
                    repr: $repr,
                    flags: ReprFlags::COPYABLE,
                    ..reprs[builtin_types.$ty]
                };
            )*
        };
    }

    let hom = |i| Layout::new(Offset::new(i, i), Offset::new(i, i));

    gen!(
        (bool, ir::types::B1, hom(1)),
        (i8, ir::types::I8, hom(1)),
        (i16, ir::types::I16, hom(2)),
        (i32, ir::types::I32, hom(4)),
        (i64, ir::types::I64, hom(8)),
        (int, ptr_ty, Layout::PTR),
        (u8, ir::types::I8, hom(1)),
        (u16, ir::types::I16, hom(2)),
        (u32, ir::types::I32, hom(4)),
        (u64, ir::types::I64, hom(8)),
        (uint, ptr_ty, Layout::PTR),
        (char, ir::types::I32, hom(4)),
    );

    reprs[builtin_types.str] = ReprEnt {
        repr: ptr_ty,
        flags: ReprFlags::COPYABLE | ReprFlags::ON_STACK,
        ..reprs[builtin_types.str]
    };
}

pub fn build_reprs(ptr_ty: Type, reprs: &mut Reprs, types: impl Iterator<Item = Ty>) {
    for ty in types {
        let (repr, on_stack) = smallest_repr_for(reprs[ty].layout.size(), ptr_ty);
        reprs[ty].repr = repr;
        reprs[ty].flags.set(ReprFlags::ON_STACK, on_stack);
    }
}
