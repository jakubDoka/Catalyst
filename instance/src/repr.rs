use cranelift_codegen::ir::{self, Type};

use instance_types::*;
use lexer_types::*;
use storage::*;
use typec_types::*;

use crate::layout_builder;

pub struct ReprInstancing<'a> {
    pub types: &'a mut Types,
    pub ty_lists: &'a mut TyLists,
    pub instances: &'a mut Instances,
    pub ty_comps: &'a TyComps,
    pub sources: &'a Sources,
    pub repr_fields: &'a mut ReprFields,
    pub reprs: &'a mut Reprs,
    pub ptr_ty: Type,
}

impl<'a> ReprInstancing<'a> {
    pub fn load_generic_types(
        &mut self,
        params: TyList,
        types: TyList,
        replace_cache: &mut ReplaceCache,
    ) {
        let types = self.ty_lists.get(types).to_vec(); // TODO: optimize if needed
        let new_types = types
            .iter()
            .map(|&ty| self.instantiate_repr(params, ty))
            .collect::<Vec<_>>();

        // this is done like this because there is no guarantee that
        // for all a, b in P is a not in b and b not in a, where P are `params`
        for (ty, new_ty) in types.into_iter().zip(new_types) {
            replace_cache.save(new_ty, ty, self.types, self.reprs);
        }
    }

    pub fn instantiate_repr(&mut self, params: TyList, ty: Ty) -> Ty {
        let mut new_instances = vec![]; // TODO: optimize if needed
        let result = self.expand_instances(params, ty, &mut new_instances);

        // the types are sorted by dependance (leafs first)
        for &instance in &new_instances {
            layout_builder!(self).build_size(instance);
        }

        build_reprs(self.ptr_ty, self.reprs, new_instances.drain(..));

        result
    }

    fn expand_instances(&mut self, params: TyList, ty: Ty, new_instances: &mut Vec<Ty>) -> Ty {
        let TyEnt { kind, flags, .. } = self.types[ty];
        if !flags.contains(TyFlags::GENERIC) {
            return ty;
        }

        match kind {
            TyKind::Param(index, ..) => self.ty_lists.get(params)[index as usize],
            TyKind::Ptr(base, depth) => {
                let ins_base = self.expand_instances(params, base, new_instances);
                let ptr_id = ID::pointer(self.types[ins_base].id);

                if let Some(&already) = self.instances.get(ptr_id) {
                    return already;
                }

                let pointer = TyEnt {
                    kind: TyKind::Ptr(ins_base, depth),
                    flags: flags & !TyFlags::GENERIC,
                    ..self.types[ty]
                };

                let new_ty = self.types.push(pointer);
                self.instances.insert_unique(ptr_id, new_ty);
                //new_instances.push(new_ty); // already know the size
                new_ty
            }
            TyKind::Instance(base, i_params) => {
                let mut id = ID::new("<instance>") + self.types[base].id;
                self.ty_lists.mark_frame();
                for param in self.ty_lists.get(i_params).to_vec() {
                    // TODO: optimize if needed
                    let param = self.expand_instances(params, param, new_instances);
                    id = id + self.types[param].id;
                    self.ty_lists.push_one(param);
                }

                if let Some(&already) = self.instances.get(id) {
                    self.ty_lists.discard();
                    return already;
                }

                let new_i_params = self.ty_lists.pop_frame();

                match self.types[base].kind {
                    TyKind::Struct(fields) => {
                        for &field in self.ty_comps.get(fields) {
                            self.expand_instances(new_i_params, field.ty, new_instances);
                        }
                    }
                    kind => todo!("{kind:?}"),
                }

                let instance = TyEnt {
                    id,
                    kind: TyKind::Instance(base, new_i_params),
                    flags: flags & !TyFlags::GENERIC,
                    ..self.types[ty]
                };
                let ty = self.types.push(instance);
                self.instances.insert_unique(id, ty);
                new_instances.push(ty);

                ty
            }
            kind => todo!("{kind:?}"),
        }
    }
}

pub struct LayoutBuilder<'a> {
    pub types: &'a Types,
    pub sources: &'a Sources,
    pub ty_comps: &'a TyComps,
    pub instances: &'a Instances,
    pub ty_lists: &'a TyLists,
    pub repr_fields: &'a mut ReprFields,
    pub reprs: &'a mut Reprs,
}

impl<'a> LayoutBuilder<'a> {
    pub fn new(
        types: &'a Types,
        sources: &'a Sources,
        ty_comps: &'a TyComps,
        instances: &'a Instances,
        ty_lists: &'a TyLists,
        repr_fields: &'a mut ReprFields,
        reprs: &'a mut Reprs,
    ) -> Self {
        Self {
            types,
            sources,
            ty_comps,
            instances,
            ty_lists,
            repr_fields,
            reprs,
        }
    }

    pub fn build_layouts(&mut self, graph: &Graph<Ty>) {
        let order = {
            let mut vec: Vec<Ty> = Vec::with_capacity(self.types.len());
            graph.total_ordering(&mut vec).unwrap();
            vec
        };

        self.reprs.resize(self.types.len());
        for id in order {
            println!("0000 {id}");
            self.build_size(id);
        }
    }

    pub fn build_size(&mut self, id: Ty) {
        self.build_size_low(id, TyList::default());
    }

    pub fn build_size_low(&mut self, id: Ty, params: TyList) {
        let ty = &self.types[id];
        if ty.flags.contains(TyFlags::GENERIC) || ty.flags.contains(TyFlags::BUILTIN) {
            return;
        }
        
        match ty.kind {
            TyKind::Instance(base, params) => {
                let TyEnt { kind, .. } = self.types[base];
                match kind {
                    TyKind::Struct(fields) => {
                        self.resolve_struct_repr(id, params, fields);
                    }
                    _ => todo!("{kind:?}"),
                }
            }
            TyKind::Struct(fields) | TyKind::EnumVar(.., fields) => {
                self.resolve_struct_repr(id, params, fields);
            }
            TyKind::Ptr(..) => {
                self.reprs[id].layout = Layout::PTR;
                self.reprs[id].flags = ReprFlags::COPYABLE;
            }
            TyKind::Enum(.., variants) => {
                self.resolve_enum_repr(id, params, variants);
            }
            kind => todo!("{kind:?}"),
        };
    }

    pub fn true_type(&self, ty: Ty, params: TyList) -> Ty {
        let TyEnt { kind, flags, .. } = self.types[ty];

        if !flags.contains(TyFlags::GENERIC) {
            return ty;
        }

        match kind {
            TyKind::Param(index, ..) => self.ty_lists.get(params)[index as usize],
            TyKind::Ptr(base, _) => {
                let base = self.true_type(base, params);
                let ptr_id = ID::pointer(self.types[base].id);
                self.instances.get(ptr_id).unwrap().clone()
            }
            TyKind::Instance(base, params) => {
                let mut id = ID::new("<instance>") + self.types[base].id;
                for &param in self.ty_lists.get(params) {
                    let param = self.true_type(param, params);
                    id = id + self.types[param].id;
                }
                self.instances.get(id).unwrap().clone()
            }
            kind => todo!("{kind:?}"),
        }
    }

    pub fn resolve_enum_repr(&mut self, ty: Ty, _params: TyList, fields: TyCompList) {
        let (layout, copyable) = self.ty_comps
            .get(fields)
            .iter()
            .map(|field| (
                self.reprs[field.ty].layout, 
                self.reprs[field.ty].flags.contains(ReprFlags::COPYABLE)
            )).fold(
            (Layout::ZERO, true),
            |(acc_layout, acc_copyable), (layout, copyable)| 
                (layout.max(acc_layout), copyable & acc_copyable),
        );

        for field in self.ty_comps.get(fields) {
            self.reprs[field.ty].layout = layout;
        }

        self.reprs[ty] = ReprEnt {
            repr: ir::types::INVALID,
            layout,
            flags: ReprFlags::COPYABLE & copyable,
        };
    }

    pub fn resolve_struct_repr(&mut self, ty: Ty, params: TyList, fields: TyCompList) {
        let fields = self.ty_comps.get(fields);
        let ty_id = self.types[ty].id;

        let align = fields
            .iter()
            .map(|field| self.reprs[self.true_type(field.ty, params)].layout.size())
            .fold(Offset::ZERO, |acc, align| acc.max(align).min(Offset::PTR));

        let mut size = Offset::ZERO;
        let mut copyable = true;
        for (i, &field) in fields.iter().enumerate() {
            let field_ty = self.true_type(field.ty, params);
            let ent = &self.reprs[field_ty];

            copyable &= ent.flags.contains(ReprFlags::COPYABLE);

            let field = ReprField { offset: size };
            let id = ID::raw_field(ty_id, i as u64);
            assert!(self.repr_fields.insert(id, field).is_none(), "{id:?}");

            size = size + ent.layout.size();
            let padding = align - size % align;
            if padding != align {
                size = size + padding;
            }
        }

        let flags = ReprFlags::COPYABLE & copyable;
        self.reprs[ty] = ReprEnt {
            repr: ir::types::INVALID,
            flags,
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
        (char, ir::types::I32, hom(4)),
    );
}

pub fn build_reprs(ptr_ty: Type, reprs: &mut Reprs, types: impl Iterator<Item = Ty>) {        
    for ty in types {
        let (repr, on_stack) = smallest_repr_for(reprs[ty].layout.size(), ptr_ty);
        reprs[ty].repr = repr;
        reprs[ty].flags.set(ReprFlags::ON_STACK, on_stack);
    }
}