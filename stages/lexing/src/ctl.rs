use crate::*;

pub extern "C" fn ctl_lexer_next(lexer: &mut Lexer) -> Token {
    lexer.next_tok()
}
