use lexing::*;
use storage::*;

pub struct AstEnt {
    pub kind: AstKind,
    pub children: AstList,
    pub span: Span,    
}

pub enum AstKind {
    
}

gen_v_ptr!(Ast AstList);