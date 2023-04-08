use crate::*;
use rkyv::{Archive, Deserialize, Serialize};
use span::*;
use storage::*;

derive_relocated!(struct Const { ty });
#[derive(Archive, Serialize, Deserialize)]
pub struct Const {
    pub name: Ident,
    pub ty: Ty,
    pub value: FolderValue,
    pub loc: Loc,
}

derive_relocated!(struct Struct { generics fields });
#[derive(Clone, Copy, Default, Serialize, Deserialize, Archive)]
pub struct Struct {
    pub name: Ident,
    pub generics: Generics,
    pub fields: FragSlice<Field>,
    pub loc: Loc,
    pub drop_spec: DropSpec,
}

impl Struct {
    pub fn find_field(s: FragRef<Self>, name: Ident, types: &Types) -> Option<(usize, Ty)> {
        types[types[s].fields]
            .iter()
            .enumerate()
            .find_map(|(i, &v)| (v.name == name).then_some((i, v.ty)))
    }

    pub fn compute_drop_spec(&self, types: &Types) -> DropSpec {
        types[self.fields]
            .iter()
            .map(|f| f.ty.drop_spec(types))
            .max()
            .unwrap_or(DropSpec::Copy)
            .max(self.drop_spec)
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
    pub ty: Ty,
    pub flags: FieldFlags,
    pub span: Span,
    pub name: Ident,
}

bitflags! {
    FieldFlags: u8 {
        MUTABLE
        USED
    }
}

derive_relocated!(struct Enum { generics variants });
#[derive(Clone, Copy, Default, Serialize, Deserialize, Archive)]
pub struct Enum {
    pub name: Ident,
    pub generics: Generics,
    pub variants: FragSlice<Variant>,
    pub loc: Loc,
    pub drop_spec: DropSpec,
}

impl Enum {
    pub fn find_variant(e: FragRef<Self>, name: Ident, types: &Types) -> Option<(usize, Ty)> {
        types[types[e].variants]
            .iter()
            .enumerate()
            .find_map(|(i, v)| (v.name == name).then_some((i, v.ty)))
    }

    pub fn compute_drop_spec(&self, types: &Types) -> DropSpec {
        types[self.variants]
            .iter()
            .map(|v| v.ty.drop_spec(types))
            .max()
            .unwrap_or(DropSpec::Copy)
            .max(self.drop_spec)
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
    pub ty: Ty,
    pub span: Span,
}

pub type ArraySize = u32;

derive_relocated!(struct Array { item });
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Deserialize, Archive, Serialize,
)]

pub struct Array {
    pub item: Ty,
    pub len: ArraySize,
}

#[derive(Clone, Copy, Default, Serialize, Deserialize, Archive, PartialEq, Eq, PartialOrd, Ord)]
pub enum DropSpec {
    ImplementsCopy,
    #[default]
    Copy,
    Uncretain,
    Drop,
}
