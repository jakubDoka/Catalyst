use std::default::default;

use lexing::*;
use storage::*;



pub type AstData = CacheBumpMap<AstList, AstEnt, Ast>;

#[derive(Clone, Copy)]
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
        AstEnt::new(AstKind::No, Maybe::none(), default())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AstKind {
    Imports,
    Import,
    String,
    Ident,
    Int,
    No,
}

gen_v_ptr!(Ast AstList);