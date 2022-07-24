use lexing_t::*;
use storage::*;

pub type AstData = CacheBumpMap<AstList, AstEnt, Ast>;
pub type Visibility = ShadowMap<Ident, Vis>;

#[derive(Debug, Clone, Copy, Default)]
pub struct AstEnt {
    pub kind: AstKind,
    pub children: Maybe<AstList>,
    pub span: Span,
}

impl AstEnt {
    pub fn new(kind: AstKind, children: Maybe<AstList>, span: Span) -> Self {
        AstEnt {
            kind,
            children,
            span,
        }
    }

    pub fn leaf(kind: AstKind, span: Span) -> Self {
        AstEnt::new(kind, Maybe::none(), span)
    }

    pub fn none() -> Self {
        AstEnt::default()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AstKind {
    Return,
    Binary,

    Struct {
        vis: Vis,
    },
    StructBody,
    StructField {
        vis: Vis,
        mutable: bool,
        exported: bool,
    },

    Fn {
        vis: Vis,
    },
    FnArg {
        mutable: bool,
    },
    FnBody,

    Generics,
    GenericParam,

    TyInstance,
    PtrTy {
        mutable: bool,
    },

    ManifestSection,
    ManifestImports,
    ManifestImport {
        use_git: bool,
    },
    ManifestField,

    Imports,
    Import,

    String,
    Int,

    Ident,
    IdentChain,
    Operator,
    OperatorWithModule,

    None,
}

impl AstKind {
    pub fn is_none(&self) -> bool {
        *self == AstKind::None
    }
}

impl Default for AstKind {
    fn default() -> Self {
        AstKind::None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Vis {
    Pub,
    None,
    Priv,
}

impl Default for Vis {
    fn default() -> Self {
        Vis::None
    }
}

gen_v_ptr!(Ast AstList);
