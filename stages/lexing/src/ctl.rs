use storage::*;

use crate::*;

pub struct TokenMacroData;

#[derive(Default)]
pub struct TokenMacroCtx {
    specs: Map<VRef<str>, TokenMacroPool>,
}

impl TokenMacroCtx {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn declare_macro(&mut self, name: VRef<str>, spec: TokenMacroSpec) -> bool {
        self.specs.insert(name, TokenMacroPool::new(spec)).is_some()
    }

    pub fn alloc(&mut self, name: VRef<str>) -> Option<TokenMacro> {
        self.specs.get_mut(&name).map(|p| p.alloc(name))
    }

    pub fn free(&mut self, mut token_macro: TokenMacro) {
        token_macro.clear();
        if let Some(pool) = self.specs.get_mut(&token_macro.name) {
            pool.free(token_macro);
        }
    }
}

struct TokenMacroPool {
    pub free: Vec<*mut TokenMacroData>,
    pub spec: TokenMacroSpec,
}

impl TokenMacroPool {
    fn new(spec: TokenMacroSpec) -> Self {
        Self {
            free: Vec::new(),
            spec,
        }
    }

    fn alloc(&mut self, name: VRef<str>) -> TokenMacro {
        let data = if let Some(ptr) = self.free.pop() {
            ptr
        } else {
            unsafe { (self.spec.new)() }
        };

        TokenMacro {
            name,
            data,
            spec: self.spec,
        }
    }

    fn free(&mut self, r#macro: TokenMacro) {
        self.free.push(r#macro.into_data());
    }
}

impl Drop for TokenMacroPool {
    fn drop(&mut self) {
        for ptr in self.free.drain(..) {
            unsafe {
                (self.spec.drop)(ptr);
            }
        }
    }
}

#[derive(Clone, Copy)]
pub struct TokenMacroSpec {
    pub new: unsafe extern "C" fn() -> *mut TokenMacroData,
    pub start: unsafe extern "C" fn(*mut TokenMacroData, CtlLexer) -> bool,
    pub next: unsafe extern "C" fn(*mut TokenMacroData, CtlLexer) -> CtlOption<Token>,
    pub clear: unsafe extern "C" fn(*mut TokenMacroData),
    pub drop: unsafe extern "C" fn(*mut TokenMacroData),
}

pub struct TokenMacro {
    name: VRef<str>,
    data: *mut TokenMacroData,
    spec: TokenMacroSpec,
}

impl TokenMacro {
    pub fn next(&mut self, lexer: &mut Lexer) -> Option<Token> {
        unsafe { (self.spec.next)(self.data, CtlLexer { inner: lexer }).into() }
    }

    fn clear(&mut self) {
        unsafe { (self.spec.clear)(self.data) }
    }

    pub fn start(&mut self, lexer: &mut Lexer) -> bool {
        unsafe { (self.spec.start)(self.data, CtlLexer { inner: lexer }) }
    }

    fn into_data(self) -> *mut TokenMacroData {
        let data = self.data;
        std::mem::forget(self);
        data
    }
}

impl Drop for TokenMacro {
    fn drop(&mut self) {
        unsafe { (self.spec.drop)(self.data) }
    }
}

#[repr(C)]
pub struct CtlLexer<'a, 'b> {
    inner: &'a mut Lexer<'b>,
}

/// Returns next token from the lexer. Used for Catalyst ffi.
pub extern "C" fn ctl_lexer_next(lexer: CtlLexer) -> Token {
    lexer.inner.next_tok()
}
