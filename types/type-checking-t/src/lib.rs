#![feature(let_else)]
#![feature(inline_const)]
#![feature(default_free_fn)]

#[macro_export]
macro_rules! field_ident {
    ($self:expr, $field:expr) => {
        storage::ident!($self, ".", $field)
    };
}

#[macro_export]
macro_rules! impl_pair_ident {
    ($pair:expr) => {
        storage::ident!($pair.0.index() as u32, "$$", $pair.1.index() as u32)
    };
}

#[macro_export]
macro_rules! param_ident {
    ($param:expr) => {
        storage::ident!("param ", $param)
    };
}

#[macro_export]
macro_rules! binary_ident {
    ($param:expr) => {
        storage::ident!("binary ", $param)
    };
}

#[macro_export]
macro_rules! match_scope_ptr {
    (
        ($self:expr, $ptr:expr, $span:expr) => {
            $($binding:ident: $ty:ty => $code:expr,)*
        }
    ) => {
        $(
            if let Some($binding) = $ptr.try_read::<$ty>() {
                let _ = $binding;
                $code
            } else
        )*
        {
            let options = [$(stringify!($binding),)*];
            $self.workspace.push(diag! {
                ($span, $self.current_file) => "expected {}" { options.join(" or ") },
            });
            return Err(());
        }
    };
}

#[macro_export]
macro_rules! gen_kind {
    {
        $name:ident {
            $(
                $variant:ident $(= $struct:ident $({
                    $(
                        $field:ident: $ty:ty,
                    )*
                })?)?,
            )*
        }
    } => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub enum $name {
            $(
                $variant$(($struct))?,
            )*
        }

        impl $name {
            pub fn downcast<T: TryFrom<Self, Error = ()>>(self) -> Option<T> {
                self.try_into().ok()
            }
        }

        $(
            $(
                $(
                    #[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
                    pub struct $struct {
                        $(
                            pub $field: $ty,
                        )*
                    }
                )?

                impl Into<$name> for $struct {
                    fn into(self) -> $name {
                        $name::$variant(self)
                    }
                }

                impl TryFrom<$name> for $struct {
                    type Error = ();
                    fn try_from(value: $name) -> Result<Self, Self::Error> {
                        match value {
                            $name::$variant(value) => Ok(value),
                            _ => Err(()),
                        }
                    }
                }
            )?
        )*
    }
}

mod bound;
mod builtin_builder;
mod func;
mod state_gen;
mod ty;

pub use bound::{
    bound_checker::{SignatureError, TyInferenceError},
    Bound, BoundBase, BoundFlags, BoundFunc, BoundInstance, BoundKind, Impl,
};
pub use func::{
    tir::{Tir, TirData, TirFlags, TirKind, TirMeta},
    Def, DefFlags, Func, FuncParserCtx, Sig,
};
pub use items::ItemContext;
pub use state_gen::{BoundChecker, BuiltinBuilder, TirDisplay, TyFactory};
pub use ty::{
    typec::{HasFlag, LocOf, Typec},
    EnumVariant, Field, Ty, TyEnum, TyFlags, TyInstance, TyInt, TyKind, TyPtr,
};

mod items {
    use crate::*;
    use parsing_t::*;
    use storage::*;

    #[derive(Default)]
    pub struct ItemContext {
        pub attrs: Vec<Ast>,
        pub funcs: Vec<(Maybe<Ident>, Ast, VRef<Def>)>,
        pub types: Vec<(Ast, VRef<Ty>)>,
        pub bound_impls: Vec<(Ast, VRef<Impl>)>,
    }
}
