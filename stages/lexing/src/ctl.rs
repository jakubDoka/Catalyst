use std::mem;

use crate::*;

#[repr(C)]
pub struct CtlToken {
    pub kind: TokenKind,
    pub span: Span,
}

pub extern "C" fn ctl_lexer_next(lexer: &mut Lexer) -> CtlToken {
    unsafe { mem::transmute(lexer.next_tok::<NoTokenMeta>()) }
}
