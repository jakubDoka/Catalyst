use crate::*;
use rkyv::{Archive, Deserialize, Serialize};
use span::*;
use storage::*;

derive_relocated!(struct Impl { generics key methods });
#[derive(Clone, Copy, Serialize, Deserialize, Archive)]
pub struct Impl {
    pub generics: Generics,
    pub key: ImplKey,
    pub methods: FragRefSlice<Func>,
    pub loc: GuaranteedLoc,
}

derive_relocated!(struct ImplKey { ty spec });
#[derive(
    Clone, Copy, PartialEq, Eq, Hash, Debug, PartialOrd, Ord, Serialize, Deserialize, Archive,
)]
#[archive_attr(derive(PartialEq, Eq, Hash))]
pub struct ImplKey {
    pub ty: Ty,
    pub spec: Spec,
}

derive_relocated!(struct SpecBase { generics inherits methods });
#[derive(Clone, Copy, Default, Deserialize, Archive, Serialize)]
pub struct SpecBase {
    pub name: Ident,
    pub generics: Generics,
    pub inherits: FragSlice<Spec>,
    pub methods: FragSlice<SpecFunc>,
    pub loc: Loc,
}

impl SpecBase {
    pub fn is_macro(_s: FragRef<Self>) -> bool {
        false
    }
}

gen_water_drops! {
    SpecBase
    base_specs
    DROP => "Drop",
    COPY => "Copy",
}

derive_relocated!(struct SpecInstance { base args });
#[derive(Clone, Copy, Deserialize, Archive, Serialize)]
pub struct SpecInstance {
    pub base: FragRef<SpecBase>,
    pub args: FragSlice<Ty>,
}

derive_relocated!(struct SpecFunc { generics signature parent });
#[derive(Clone, Copy, Serialize, Deserialize, Archive)]
pub struct SpecFunc {
    pub generics: Generics,
    pub signature: Signature,
    pub name: Ident,
    pub span: Span,
    pub parent: FragRef<SpecBase>,
}

impl SpecFunc {
    pub fn is_generic(&self) -> bool {
        !self.generics.is_empty()
    }
}
