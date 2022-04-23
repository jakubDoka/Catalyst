use cranelift_codegen::{
    ir::{self, Type},
    packed_option::ReservedValue,
};
use cranelift_entity::{SecondaryMap};
use lexer::*;
use modules::*;
use typec::*;

use crate::{Result, *};

pub struct TypeTranslator<'a> {
    pub t_types: &'a typec::Types,
    pub t_graph: &'a GenericGraph,
    pub types: &'a mut Types,
    pub ptr_ty: Type,
}

impl<'a> TypeTranslator<'a> {
    pub fn translate(&mut self) -> Result {
        let order = {
            let mut vec: Vec<Ty> = Vec::with_capacity(self.t_types.ents.len());
            self.t_graph.total_ordering(&mut vec).unwrap();
            vec
        };

        self.types.ents.resize(self.t_types.ents.len());
        for id in order {
            let ty = &self.t_types.ents[id];
            let repr = match ty.kind {
                ty::Kind::Int(base) => match base {
                    64 => ir::types::I64,
                    32 => ir::types::I32,
                    16 => ir::types::I16,
                    8 => ir::types::I8,
                    _ => self.ptr_ty,
                },
                ty::Kind::Bool => ir::types::B1,
                ty::Kind::Struct(fields) => {
                    self.translate_struct(id, fields)?;
                    continue;
                }
                ty::Kind::Nothing | ty::Kind::Bound(..) => ir::types::INVALID,
                ty::Kind::Unresolved => unreachable!(),
            };

            let bytes = repr.bytes() as i32;
            let size = Size::new(bytes, bytes);
            self.types.ents[id] = Ent {
                repr,
                size,
                flags: Flags::COPYABLE,
                align: size.min(Size::PTR),
            };
        }
        Ok(())
    }

    pub fn translate_struct(&mut self, ty: Ty, fields: SFieldList) -> Result {
        let fields = self.t_types.sfields.get(fields);
        let ty_id = self.t_types.ents[ty].id;

        let align = fields
            .iter()
            .map(|field| self.types.ents[field.ty].align)
            .fold(Size::ZERO, |acc, align| acc.max(align).min(Size::PTR));

        let mut size = Size::ZERO;
        let mut copyable = true;
        for (i, &field) in fields.iter().enumerate() {
            let ent = &self.types.ents[field.ty];

            copyable &= ent.flags.contains(Flags::COPYABLE);

            let field = Field {
                offset: size,
            };
            let id = Self::field_id(ty_id, i as u64);
            assert!(self.types.fields.insert(id, field).is_none(), "{id:?}");

            size = size + ent.size;
            let padding = align - size % align;
            if padding != align {
                size = size + padding;
            }
        }

        let (repr, on_stack) = Self::smallest_repr_for(size, self.ptr_ty);
        let flags = 
            (Flags::COPYABLE & copyable) | 
            (Flags::ON_STACK & on_stack);
        self.types.ents[ty] = Ent {
            repr,
            flags,
            size,
            align,
        };

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

#[derive(Debug, Clone, Copy)]
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