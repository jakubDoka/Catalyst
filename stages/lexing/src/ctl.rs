use storage::*;

use crate::*;

pub type NextFn = unsafe extern "C" fn(*mut (), *mut CtlLexer) -> CtlOption<Token>;
pub type DropFn = unsafe extern "C" fn(*mut ());
pub type ClearFn = unsafe extern "C" fn(*mut ());

pub struct TokenMacro {
    data: *mut (),
    next: NextFn,
    clear: ClearFn,
    drop: DropFn,
}

impl TokenMacro {
    /// # Safety
    /// data must be a valid pointer to a state which can be passed to
    /// next, drop and clear.
    pub unsafe fn new(data: *mut (), next: NextFn, clear: ClearFn, drop: DropFn) -> Self {
        Self {
            data,
            next,
            clear,
            drop,
        }
    }

    pub fn next(&mut self, lexer: &mut CtlLexer) -> Option<Token> {
        unsafe { (self.next)(self.data, lexer).into() }
    }

    pub fn clear(&mut self) {
        unsafe { (self.clear)(self.data) }
    }
}

impl Drop for TokenMacro {
    fn drop(&mut self) {
        unsafe { (self.drop)(self.data) }
    }
}

#[repr(C)]
pub struct CtlLexer<'a, 'b> {
    inner: &'a mut Lexer<'b>,
}

/// Returns next token from the lexer. Used for Catalyst ffi.
/// # Safety
/// The pointer must be valid.
pub unsafe extern "C" fn ctl_lexer_next(lexer: *mut CtlLexer) -> Token {
    (*lexer).inner.next_tok()
}
