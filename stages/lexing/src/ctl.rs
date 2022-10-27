use std::alloc::Layout;

use storage::*;

use crate::*;

pub struct TokenMacroData;

#[derive(Default)]
pub struct TokenMacroCtx<'specs> {
    specs: Map<VRef<str>, TokenMacroSpec<'specs>>,
}

impl<'specs> TokenMacroCtx<'specs> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn declare_macro(&mut self, name: VRef<str>, spec: TokenMacroSpec<'specs>) {
        self.specs.insert(name, spec);
    }

    pub fn get_macro(&self, name: VRef<str>) -> Option<TokenMacroSpec<'specs>> {
        self.specs.get(&name).copied()
    }

    pub fn clear<'detached>(mut self) -> TokenMacroCtx<'detached> {
        self.specs.clear();
        unsafe { std::mem::transmute(self) }
    }
}

function_pointer! {
    TokenMacroNew(s: *mut u8, l: CtlLexer<'_, '_>),
    TokenMacroNext(s: *mut u8, l: CtlLexer<'_, '_>) -> CtlOption<Token>,
    TokenMacroDrop(s: *mut u8),
}

#[derive(Clone, Copy)]
pub struct TokenMacroSpec<'funcs> {
    pub layout: Layout,
    pub new: TokenMacroNew<'funcs>,
    pub next: TokenMacroNext<'funcs>,
    pub drop: TokenMacroDrop<'funcs>,
}

pub struct TokenMacro<'spec, 'data> {
    data: &'data mut [u8],
    spec: TokenMacroSpec<'spec>,
}

unsafe impl Send for TokenMacro<'_, '_> {}

impl<'spec, 'data> TokenMacro<'spec, 'data> {
    pub fn new(data: &'data mut [u8], spec: TokenMacroSpec<'spec>, lexer: &mut Lexer) -> Self {
        spec.new.call(data.as_mut_ptr(), CtlLexer { inner: lexer });
        Self { data, spec }
    }

    pub fn next(&mut self, lexer: &mut Lexer) -> Option<Token> {
        self.spec
            .next
            .call(self.data.as_mut_ptr(), CtlLexer { inner: lexer })
            .into()
    }
}

impl Drop for TokenMacro<'_, '_> {
    fn drop(&mut self) {
        self.spec.drop.call(self.data.as_mut_ptr());
    }
}

#[repr(C)]
pub struct CtlLexer<'a, 'b> {
    inner: &'a mut Lexer<'b>,
}

pub extern "C" fn ctl_lexer_next(lexer: CtlLexer) -> Token {
    lexer.inner.next_tok()
}
