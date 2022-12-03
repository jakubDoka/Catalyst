use std::{alloc::Layout, marker::PhantomData, mem};

use storage::*;

use crate::*;

pub struct TokenMacroData;

#[derive(Default)]
pub struct TokenMacroCtx<'specs> {
    specs: Map<Ident, TokenMacroSpec<'specs>>,
}

impl<'specs> TokenMacroCtx<'specs> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn declare_macro(&mut self, name: Ident, spec: TokenMacroSpec<'specs>) {
        self.specs.insert(name, spec);
    }

    pub fn get_macro(&self, name: Ident) -> Option<TokenMacroSpec<'specs>> {
        self.specs.get(&name).copied()
    }

    pub fn clear<'detached>(mut self) -> TokenMacroCtx<'detached> {
        self.specs.clear();
        unsafe { std::mem::transmute(self) }
    }
}

function_pointer! {
    TokenMacroNew(s: *mut u8, l: CtlLexer<'_, '_, '_>),
    TokenMacroNext(s: *mut u8) -> CtlOption<Token>,
    TokenMacroDrop(s: *mut u8) -> CtlLexer<'static, 'static, 'static>,
}

#[derive(Clone, Copy)]
pub struct TokenMacroSpec<'funcs> {
    pub layout: Layout,
    pub new: TokenMacroNew<'funcs>,
    pub next: TokenMacroNext<'funcs>,
    pub drop: TokenMacroDrop<'funcs>,
}

pub struct TokenMacro<'spec, 'data, 'source> {
    data: &'data mut [u8],
    spec: TokenMacroSpec<'spec>,
    ph: PhantomData<&'source str>,
}

unsafe impl Send for TokenMacro<'_, '_, '_> {}

impl<'spec, 'data, 'source> TokenMacro<'spec, 'data, 'source> {
    pub fn new(
        data: &'data mut [u8],
        spec: TokenMacroSpec<'spec>,
        lexer: CtlLexer<'spec, 'data, 'source>,
    ) -> Self {
        spec.new.call(data.as_mut_ptr(), lexer);
        Self {
            data,
            spec,
            ph: PhantomData,
        }
    }

    pub fn next_tok(&mut self) -> Option<Token> {
        self.spec.next.call(self.data.as_mut_ptr()).into()
    }

    pub fn drop(self) -> CtlLexer<'spec, 'data, 'source> {
        unsafe { mem::transmute(self.spec.drop.call(self.data.as_mut_ptr())) }
    }
}

#[repr(C)]
pub enum CtlLexer<'spec, 'data, 'source> {
    Base(Lexer<'source>),
    Macro(TokenMacro<'spec, 'data, 'source>),
}

const _: () = {
    assert!(std::mem::size_of::<CtlLexer>() == 64);
    assert!(std::mem::align_of::<CtlLexer>() == 8);
};

// #[test]
// fn test() {
//     panic!("size: {}, align: {}", mem::size_of::<CtlLexer>(), mem::align_of::<CtlLexer>());
// }

impl CtlLexer<'_, '_, '_> {
    pub fn progress(&mut self) -> usize {
        match self {
            Self::Base(lexer) => lexer.progress(),
            Self::Macro(..) => {
                self.drop();
                self.progress()
            }
        }
    }

    pub fn next_tok(&mut self) -> Token {
        match self {
            CtlLexer::Base(base) => base.next_tok(),
            CtlLexer::Macro(token_macro) => match token_macro.next_tok() {
                Some(tok) => tok,
                None => {
                    self.drop();
                    self.next_tok()
                }
            },
        }
    }

    fn drop(&mut self) {
        storage::map_in_place(self, |s| match s {
            Self::Macro(lexer) => lexer.drop(),
            Self::Base(..) => unreachable!(),
        });
    }
}

pub extern "C" fn ctl_lexer_next(lexer: &mut CtlLexer) -> Token {
    lexer.next_tok()
}
