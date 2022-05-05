use std::panic;

use cranelift_codegen::{
    ir::{self, Type},
    packed_option::ReservedValue,
};
use lexer::*;
use modules::*;
use typec::*;

use crate::{Result, *};

pub struct Translator<'a> {
    pub t_types: &'a typec::Types,
    pub t_graph: &'a GenericGraph,
    pub sources: &'a Sources,
    pub types: &'a mut Types,
    pub ptr_ty: Type,
}

impl<'a> Translator<'a> {
    pub fn translate(&mut self) -> Result {
        let order = {
            let mut vec: Vec<Ty> = Vec::with_capacity(self.t_types.ents.len());
            self.t_graph.total_ordering(&mut vec).unwrap();
            for &ty in &vec {
                println!("{}", ty::Display::new(self.t_types, self.sources, ty));
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

    pub fn field_id(ty: ID, field: u64) -> ID {
        ty + ID(field)
    }
}

pub fn resolve_type(target: Ty, t_types: &mut typec::Types, types: &mut Types, ptr_ty: Type) -> Ty {
    let mut new_ty_dump = vec![];
    let result = typec::late_instantiate(target, t_types, &mut new_ty_dump);
    for ty in new_ty_dump {
        resolve_type_repr(ty, t_types, types, ptr_ty, false);
    }
    result
}

#[macro_export]
macro_rules! resolve_type {
    ($self:expr, $target:expr) => {
        resolve_type($target, $self.t_types, $self.types, $self.ptr_ty)
    };
}

pub fn resolve_type_repr(
    id: Ty,
    t_types: &typec::Types,
    types: &mut Types,
    ptr_ty: Type,
    skip_generic: bool,
) {
    let ty = &t_types.ents[id];
    let repr = match ty.kind {
        _ if ty.flags.contains(ty::Flags::GENERIC) && skip_generic => ir::types::INVALID,
        ty::Kind::Int(base) => match base {
            64 => ir::types::I64,
            32 => ir::types::I32,
            16 => ir::types::I16,
            8 => ir::types::I8,
            _ => ptr_ty,
        },
        ty::Kind::Bool => ir::types::B1,
        ty::Kind::Struct(fields) => {
            resolve_struct_repr(id, fields, t_types, types, ptr_ty);
            return;
        }
        ty::Kind::Ptr(..) => ptr_ty,
        ty::Kind::Instance(..) => todo!(),
        ty::Kind::Nothing
        | ty::Kind::Bound(..)
        | ty::Kind::Param(..)
        | ty::Kind::BoundCombo(..)
        | ty::Kind::Unresolved => ir::types::INVALID,
    };

    let bytes = repr.bytes() as i32;
    let size = Size::new(bytes, bytes);
    types.ents[id] = Ent {
        repr,
        size,
        flags: Flags::COPYABLE,
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
    t_types: &typec::Types,
    types: &mut Types,
    ptr_ty: Type,
) {
    let fields = t_types.sfields.get(fields);
    let ty_id = t_types.ents[ty].id;

    let align = fields
        .iter()
        .map(|field| types.ents[field.ty].align)
        .fold(Size::ZERO, |acc, align| acc.max(align).min(Size::PTR));

    let mut size = Size::ZERO;
    let mut copyable = true;
    for (i, &field) in fields.iter().enumerate() {
        let ent = &types.ents[field.ty];

        copyable &= ent.flags.contains(Flags::COPYABLE);

        let field = Field { offset: size };
        let id = Translator::field_id(ty_id, i as u64);
        assert!(types.fields.insert(id, field).is_none(), "{id:?}");

        size = size + ent.size;
        let padding = align - size % align;
        if padding != align {
            size = size + padding;
        }
    }

    let (repr, on_stack) = Translator::smallest_repr_for(size, ptr_ty);
    let flags = (Flags::COPYABLE & copyable) | (Flags::ON_STACK & on_stack);
    types.ents[ty] = Ent {
        repr,
        flags,
        size,
        align,
    };
}

pub struct Types {
    pub fields: Map<Field>,
    pub ents: SecondaryMap<Ty, Ent>,
}

impl Types {
    pub fn new() -> Self {
        Types {
            fields: Map::new(),
            ents: SecondaryMap::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Ent {
    pub repr: Type,
    pub size: Size,
    pub align: Size,
    pub flags: Flags,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Field {
    pub offset: Size,
}

impl ReservedValue for Field {
    fn reserved_value() -> Self {
        Self {
            offset: Size::new(i32::MAX, i32::MAX),
        }
    }

    fn is_reserved_value(&self) -> bool {
        self.offset == Size::new(i32::MAX, i32::MAX)
    }
}

bitflags::bitflags! {
    #[derive(Default)]
    pub struct Flags: u32 {
        /// This type cannot fit into register.
        const ON_STACK = 1 << 0;
        /// This type can be safely copied.
        const COPYABLE = 1 << 1;
    }
}

typec::impl_bool_bit_and!(Flags);
