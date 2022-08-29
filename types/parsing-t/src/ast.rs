use lexing_t::*;
use scope::*;
use storage::*;

pub type AstData = CacheBumpMap<Ast>;

#[derive(Debug, Clone, Copy, Default)]
pub struct Ast {
    pub kind: AstKind,
    pub children: VSlice<Ast>,
    pub span: Span,
}

impl Ast {
    pub fn new(kind: AstKind, children: VSlice<Ast>, span: Span) -> Self {
        Ast {
            kind,
            children,
            span,
        }
    }

    pub fn leaf(kind: AstKind, span: Span) -> Self {
        Ast::new(kind, VSlice::empty(), span)
    }

    pub fn none() -> Self {
        Ast::default()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AstKind {
    Return,
    Binary,
    Index,
    Call,
    DotExpr,

    Bound {
        vis: Vis,
    },
    BoundType {
        vis: Vis,
    },
    BoundImpl {
        vis: Vis,
    },
    BoundBody,

    Impl {
        vis: Vis,
    },
    ImplBody,
    ImplType,
    ImplUse,

    Struct {
        vis: Vis,
    },
    StructBody,
    StructField {
        vis: Vis,
        mutable: bool,
        exported: bool,
    },
    StructExpr,
    StructExprBody,
    StructExprField,

    Func {
        vis: Vis,
    },
    FuncSignature {
        vis: Vis,
    },
    FuncArg {
        mutable: bool,
    },
    FuncBody,

    Generics,
    GenericParam,

    TyInstance,
    BoundInstance,
    InstanceExpr,
    PtrTy {
        mutable: bool,
    },
    FieldTy,

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
