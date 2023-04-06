use crate::*;
use rkyv::{Archive, Deserialize, Serialize};
use span::*;
use storage::*;

use super::compact::{CompactBaseTy, CompactTy};

/// Variants are particularly orders so we can combine two with max function.
#[derive(Clone, Copy, Default, Serialize, Deserialize, Archive, PartialEq, Eq, PartialOrd, Ord)]
pub enum DropSpec {
    Copy,
    #[default]
    Hibrid,
    Drop,
}

derive_relocated!(struct Const { ty });
#[derive(Archive, Serialize, Deserialize)]
pub struct Const {
    pub name: Ident,
    pub ty: CompactTy,
    pub value: FolderValue,
    pub loc: Loc,
}

derive_relocated!(struct Instance { base args });
#[derive(Clone, Copy, Serialize, Deserialize, Archive)]
pub struct Instance {
    pub base: CompactBaseTy,
    pub args: FragSlice<CompactTy>,
}

derive_relocated!(struct Struct { generics fields });
#[derive(Clone, Copy, Default, Serialize, Deserialize, Archive)]
pub struct Struct {
    pub name: Ident,
    pub generics: WhereClause,
    pub fields: FragSlice<Field>,
    pub loc: Loc,
    pub drop_spec: DropSpec,
}

impl Struct {
    pub fn find_field<'a>(
        s: FragRef<Self>,
        name: Ident,
        types: &Types,
        arena: &ProxyArena<'a>,
    ) -> Option<(usize, FragRef<Field>, Ty<'a>)> {
        types[types[s].fields]
            .iter()
            .zip(types[s].fields.keys())
            .enumerate()
            .find_map(|(i, (&v, k))| {
                (v.name == name).then_some((i, k, Ty::load(v.ty, types, arena)))
            })
    }

    pub fn update_drop_spec(&self, types: &Types) -> DropSpec {
        types[self.fields]
            .iter()
            .map(|f| f.ty.drop_spec(types))
            .max()
            .unwrap_or(DropSpec::Copy)
    }
}

gen_water_drops! {
    Struct
    structs
    LEXER => "Lexer",
}

derive_relocated!(struct Field { ty });
#[derive(Clone, Copy, Serialize, Deserialize, Archive)]
pub struct Field {
    pub vis: Option<Vis>,
    pub ty: CompactTy,
    pub flags: FieldFlags,
    pub span: Span,
    pub name: Ident,
}

bitflags! {
    FieldFlags: u8 {
        MUTABLE
        USED
        DECIDES_DROP_SPEC
    }
}

derive_relocated!(struct Enum { generics variants });
#[derive(Clone, Copy, Default, Serialize, Deserialize, Archive)]
pub struct Enum {
    pub name: Ident,
    pub generics: WhereClause,
    pub variants: FragSlice<Variant>,
    pub loc: Loc,
    pub drop_spec: DropSpec,
}

impl Enum {
    pub fn find_variant<'a>(
        e: FragRef<Self>,
        name: Ident,
        types: &Types,
        arena: &ProxyArena<'a>,
    ) -> Option<(usize, Ty<'a>)> {
        types[types[e].variants]
            .iter()
            .enumerate()
            .find_map(|(i, v)| (v.name == name).then_some((i, Ty::load(v.ty, types, arena))))
    }

    pub fn update_drop_spec(&self, types: &Types) -> DropSpec {
        types[self.variants]
            .iter()
            .map(|f| f.ty.drop_spec(types))
            .max()
            .unwrap_or(DropSpec::Copy)
    }
}

gen_water_drops! {
    Enum
    enums
    OPTION => "Option",
    TOKEN_KIND => "TokenKind",
}

derive_relocated!(struct Variant { ty });
#[derive(Clone, Copy, Serialize, Deserialize, Archive)]
pub struct Variant {
    pub name: Ident,
    pub ty: CompactTy,
    pub span: Span,
    pub flags: VariantFlags,
}

bitflags! {
    VariantFlags: u8 {
        DECIDES_DROP_SPEC
    }
}

pub type ArraySize = u32;

derive_relocated!(struct Array { item });
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Deserialize, Archive, Serialize,
)]
pub struct Array {
    pub(super) item: CompactTy,
    pub(super) len: ArraySize,
}
