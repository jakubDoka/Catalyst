#![feature(let_else)]
#![feature(explicit_generic_args_with_impl_trait)]
#![feature(let_chains)]
#![feature(if_let_guard)]
#![feature(inline_const_pat)]
#![allow(incomplete_features)]
#![feature(atomic_mut_ptr)]
#![feature(default_free_fn)]

pub mod bound_verifier;
pub mod builtin_builder;
pub mod error;
pub mod ident_hasher;
pub mod scope;
pub mod state;
pub mod tir;
pub mod ty;
pub mod ty_parser;

pub use state::{
    BoundVerifier, BuiltinBuilder, IdentHasher, ScopeBuilder, TirBuilder, TyBuilder,
    TyParser,
};
pub use utils::{
    char_value, int_value, not_found_handler, parse_call_conv,
    scope_error_handler, CharError,
};

pub mod utils {
    use cranelift_codegen::isa::CallConv;
    use std::{any::TypeId, str::FromStr};

    use ast::*;
    use errors::*;
    use lexer::*;
    use module_types::*;
    use storage::*;
    use typec_types::*;

    pub fn scope_error_handler<'a>(
        diagnostics: &'a mut Diagnostics,
        not_found: impl Fn() -> TyError + 'a,
        loc: Span,
        expected: &'a str,
        matcher: &'a [(TypeId, &str)],
    ) -> impl FnMut(ScopeFindError) + 'a {
        move |e| {
            let err = match e {
                ScopeFindError::NotFound => not_found(),
                ScopeFindError::InvalidType(got) => TyError::InvalidItemType {
                    expected: expected.into(),
                    got: matcher
                        .iter()
                        .find_map(|&(id, name)| (id == got).then_some(name))
                        .unwrap_or("something unhandled")
                        .into(),
                    loc,
                },
                ScopeFindError::Collision(items) => TyError::ScopeCollision { items, loc },
                ScopeFindError::Other => return,
            };
            diagnostics.push(err);
        }
    }

    pub fn int_value(sources: &Sources, span: Span, signed: bool) -> u128 {
        let mut chars = sources.display(span).chars();
        let mut value = 0;
        while let Some(c @ '0'..='9') = chars.next() {
            value = value * 10 + (c as u128 - '0' as u128);
        }

        if signed {
            value = value.wrapping_sub(i128::MIN as u128);
        }

        value
    }

    #[derive(Debug, Clone, Copy)]
    pub enum CharError {
        ExtraCharacters,
        NoCharacter,
    }

    pub fn char_value(sources: &Sources, span: Span) -> std::result::Result<char, CharError> {
        let mut chars = sources.display(span.strip_sides()).chars();
        let char = chars.next().ok_or(CharError::NoCharacter)?;
        if chars.next().is_some() {
            return Err(CharError::ExtraCharacters);
        }
        Ok(char)
    }

    pub fn parse_call_conv(
        call_conv: Ast,
        sources: &Sources,
        ast_data: &AstData,
        diagnostics: &mut Diagnostics,
    ) -> Option<CallConv> {
        if call_conv.is_reserved_value() {
            Some(CallConv::Fast)
        } else {
            let span = ast_data.nodes[call_conv].span.strip_sides();
            let str = sources.display(span);
            if str == "default" {
                None
            } else {
                CallConv::from_str(str)
                    .map_err(|_| diagnostics.push(TyError::InvalidCallConv { loc: span }))
                    .ok()
            }
        }
    }

    pub fn not_found_handler(span: Span) -> impl Fn() -> TyError {
        move || TyError::ScopeItemNotFound { loc: span }
    }
}

#[macro_export]
macro_rules! matcher {
    ($($name:ident = $str:literal),*) => {
        &[$(
           (std::any::TypeId::of::<$name>(), $str),
        )*]
    };
    () => {

    };
}
