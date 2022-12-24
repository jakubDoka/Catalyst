#![feature(default_free_fn)]
#![feature(type_alias_impl_trait)]
#![feature(let_chains)]
#![feature(never_type)]

#[macro_export]
macro_rules! token {
    (none) => {
        None
    };
    ($value:ident) => {
        Some(TokenKind::$value)
    };
}

#[macro_export]
macro_rules! branch {
    (
        $self:expr => {
            $($cond:ident$(($($value:pat = $default:expr),*))? => $res:expr,)*
            @$($fallback:tt)*
        }
    ) => {
        match $self.state.current.kind {
            $(TokenKind::$cond$(($($value),*))? => $res,)*
            _ => $crate::branch!(@fallback ($self, $($cond$(($($default),*))?)*) $($fallback)*),
        }
    };

    (
        @fallback $ignored:tt => $handler:expr
    ) => {
        $handler
    };

    (
        @fallback ($self:expr, $($cond:ident$(($($default:expr),*))?)*) $ast_name:literal
    ) => {
        const EXPECTED: &[&str] = &[
            $(
                TokenKind::$cond$(($($value),*))?.as_str(),
            )*
        ];
        $crate::parser::ExpectedStartOfAst {
            ast_name: $ast_name,
            expected: EXPECTED,
        }
    };

    (
        str $self:expr => {
            $($str:literal => $res:expr,)*
            @$($fallback:tt)*
        }
    ) => {
        match $self.span_str($self.current.span) {
            $($str => $res,)*
            _ => {
                $crate::branch!(@str_fallback ($self, $($str)*) $($fallback)*)
            },
        }
    };

    (
        @str_fallback $ignored:tt => $handler:expr,
    ) => {
        $handler
    };

    (
        @str_fallback ($self:expr, $($str:literal)*) $ast_name:literal,
    ) => {
        {
            $self.workspace.push($crate::parser::ExpectedStartOfAst {
                ast_name: $ast_name,
                expected: [$($str,)*].join(", "),
                found: $self.state.current.kind,
                loc: $self.loc(),
            })?
        }
    };

    (
        @fallback $ignored:tt => $handler:expr,
    ) => {
        $handler
    };

    (
        @fallback ($self:expr, $($cond:ident$(($($default:expr),*))?)*) $ast_name:literal,
    ) => {
        {
            $self.workspace.push($crate::parser::ExpectedStartOfAst {
                ast_name: $ast_name,
                expected: [
                    $(
                        TokenKind::$cond$(($($default),*))?.as_str(),
                    )*
                ].join(", "),
                found: $self.state.current.kind,
                loc: $self.loc(),
            })?
        }
    };
}

mod parser;

pub use {
    parser::items::ItemsAstResult,
    parser::{Parser, ParserCtx},
    parsing_t::*,
};
