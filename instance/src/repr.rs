use cranelift_codegen::{
    ir::{self, Type},
};

use instance_types::*;
use module_types::*;
use storage::*;
use lexer_types::*;
use typec_types::*;
use crate::*;

pub struct ReprBuilder<'a> {
    pub t_types: &'a Types,
    pub t_graph: &'a GenericGraph,
    pub sources: &'a Sources,
    pub types: &'a mut instance_types::Reprs,
    pub ptr_ty: Type,
}

impl<'a> ReprBuilder<'a> {
    pub fn translate(&mut self) -> errors::Result {
        let order = {
            let mut vec: Vec<Ty> = Vec::with_capacity(self.t_types.ents.len());
            self.t_graph.total_ordering(&mut vec).unwrap();
            for &ty in &vec {
                println!("{}", TyDisplay::new(self.t_types, self.sources, ty));
            }
            vec
        };

        self.types.ents.resize(self.t_types.ents.len());
        for id in order {
            resolve_type_repr!(self, id, true);
        }

        Ok(())
    }

    pub fn smallest_repr_for(size: Size, ptr_ty: Type) -> (Type, bool) {
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
}

pub fn resolve_type_repr(
    id: Ty,
    t_types: &Types,
    reprs: &mut Reprs,
    ptr_ty: Type,
    skip_generic: bool,
) {
    let ty = &t_types.ents[id];
    let repr = match ty.kind {
        _ if ty.flags.contains(TyFlags::GENERIC) && skip_generic => ir::types::INVALID,
        TyKind::Int(base) => match base {
            64 => ir::types::I64,
            32 => ir::types::I32,
            16 => ir::types::I16,
            8 => ir::types::I8,
            _ => ptr_ty,
        },
        TyKind::Bool => ir::types::B1,
        TyKind::Struct(fields) => {
            resolve_struct_repr(id, fields, t_types, reprs, ptr_ty);
            return;
        }
        TyKind::Ptr(..) => ptr_ty,
        TyKind::Instance(..) => todo!(),
        TyKind::Nothing
        | TyKind::Bound(..)
        | TyKind::Param(..)
        | TyKind::BoundCombo(..)
        | TyKind::Unresolved => ir::types::INVALID,
    };

    let bytes = repr.bytes() as i32;
    let size = Size::new(bytes, bytes);
    reprs.ents[id] = ReprEnt {
        repr,
        size,
        flags: ReprFlags::COPYABLE,
        align: size.min(Size::PTR),
    };
}

#[macro_export]
macro_rules! resolve_type_repr {
    ($self:expr, $id:expr, $skip_generic:expr) => {
        resolve_type_repr($id, $self.t_types, $self.types, $self.ptr_ty, $skip_generic)
    };
}

pub fn resolve_struct_repr(
    ty: Ty,
    fields: SFieldList,
    t_types: &Types,
    reprs: &mut Reprs,
    ptr_ty: Type,
) {
    let fields = t_types.sfields.get(fields);
    let ty_id = t_types.ents[ty].id;

    let align = fields
        .iter()
        .map(|field| reprs.ents[field.ty].align)
        .fold(Size::ZERO, |acc, align| acc.max(align).min(Size::PTR));

    let mut size = Size::ZERO;
    let mut copyable = true;
    for (i, &field) in fields.iter().enumerate() {
        let ent = &reprs.ents[field.ty];

        copyable &= ent.flags.contains(ReprFlags::COPYABLE);

        let field = ReprField { offset: size };
        let id = ID::raw_field(ty_id, i as u64);
        assert!(reprs.fields.insert(id, field).is_none(), "{id:?}");

        size = size + ent.size;
        let padding = align - size % align;
        if padding != align {
            size = size + padding;
        }
    }

    let (repr, on_stack) = ReprBuilder::smallest_repr_for(size, ptr_ty);
    let flags = (ReprFlags::COPYABLE & copyable) | (ReprFlags::ON_STACK & on_stack);
    reprs.ents[ty] = ReprEnt {
        repr,
        flags,
        size,
        align,
    };
}
