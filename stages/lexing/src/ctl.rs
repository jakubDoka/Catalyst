use storage::*;

use crate::*;

pub struct TokenMacroData;

#[derive(Default)]
pub struct TokenMacroCtx<'a> {
    specs: Map<VRef<str>, TokenMacroPool<'a>>,
}

impl<'a> TokenMacroCtx<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn declare_macro(&mut self, name: VRef<str>, spec: TokenMacroSpec<'a>) -> bool {
        self.specs.insert(name, TokenMacroPool::new(spec)).is_some()
    }

    pub fn alloc(&mut self, name: VRef<str>) -> Option<TokenMacro<'a>> {
        self.specs.get_mut(&name).map(|p| p.alloc(name))
    }

    pub fn free(&mut self, mut token_macro: TokenMacro<'a>) {
        token_macro.clear();
        if let Some(pool) = self.specs.get_mut(&token_macro.name) {
            pool.free(token_macro);
        }
    }
}

struct TokenMacroPool<'a> {
    pub free: Vec<*mut TokenMacroData>,
    pub spec: TokenMacroSpec<'a>,
}

impl<'a> TokenMacroPool<'a> {
    fn new(spec: TokenMacroSpec<'a>) -> Self {
        Self {
            free: Vec::new(),
            spec,
        }
    }

    fn alloc(&mut self, name: VRef<str>) -> TokenMacro<'a> {
        let data = if let Some(ptr) = self.free.pop() {
            ptr
        } else {
            self.spec.new.call()
        };

        TokenMacro {
            name,
            data,
            spec: self.spec,
        }
    }

    fn free(&mut self, r#macro: TokenMacro<'a>) {
        self.free.push(r#macro.into_data());
    }
}

impl Drop for TokenMacroPool<'_> {
    fn drop(&mut self) {
        for ptr in self.free.drain(..) {
            self.spec.drop.call(ptr);
        }
    }
}

function_pointer! {
    TokenMacroNew -> *mut TokenMacroData,
    TokenMacroStart(s: *mut TokenMacroData, l: CtlLexer<'_, '_>) -> bool,
    TokenMacroNext(s: *mut TokenMacroData, l: CtlLexer<'_, '_>) -> CtlOption<Token>,
    TokenMacroClear(s: *mut TokenMacroData),
    TokenMacroDrop(s: *mut TokenMacroData),
}

#[derive(Clone, Copy)]
pub struct TokenMacroSpec<'a> {
    pub new: TokenMacroNew<'a>,
    pub start: TokenMacroStart<'a>,
    pub next: TokenMacroNext<'a>,
    pub clear: TokenMacroClear<'a>,
    pub drop: TokenMacroDrop<'a>,
}

pub struct TokenMacro<'a> {
    name: VRef<str>,
    data: *mut TokenMacroData,
    spec: TokenMacroSpec<'a>,
}

unsafe impl Send for TokenMacro<'_> {}

impl TokenMacro<'_> {
    pub fn next(&mut self, lexer: &mut Lexer) -> Option<Token> {
        self.spec
            .next
            .call(self.data, CtlLexer { inner: lexer })
            .into()
    }

    fn clear(&mut self) {
        self.spec.clear.call(self.data)
    }

    pub fn start(&mut self, lexer: &mut Lexer) -> bool {
        self.spec.start.call(self.data, CtlLexer { inner: lexer })
    }

    fn into_data(self) -> *mut TokenMacroData {
        let data = self.data;
        std::mem::forget(self);
        data
    }
}

impl Drop for TokenMacro<'_> {
    fn drop(&mut self) {
        self.spec.drop.call(self.data)
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
