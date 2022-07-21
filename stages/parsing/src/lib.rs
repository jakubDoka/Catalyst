#![feature(default_free_fn)]
#![feature(let_else)]
#![feature(let_chains)]

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
macro_rules! list {
    ($self:expr, $start:ident, $sep:ident, $end:ident, exp $func:expr) => {
        $self.list(token!($start), token!($sep), token!($end), $func)
    };

    ($self:expr, $start:ident, $sep:ident, $end:ident, $func:ident) => {
        list!($self, $start, $sep, $end, exp Self::$func)
    };
}

#[macro_export]
macro_rules! opt_list {
    ($self:expr, $start:ident, $sep:ident, $end:ident, exp $func:expr) => {
        $self.opt_list(TokenKind::$start, token!($sep), token!($end), $func)
    };

    ($self:expr, $start:ident, $sep:ident, $end:ident, $func:ident) => {
        opt_list!($self, $start, $sep, $end, exp Self::$func)
    };
}

#[macro_export]
macro_rules! branch {
    (
        $self:expr => {
            $($cond:ident$(($($value:pat = $default:expr),*))? => $res:expr,)*
        }
    ) => {
        match $self.state.current.kind {
            $(TokenKind::$cond$(($($value),*))? => {$res;},)*
            _ => {
                let terminals = [
                    $(TokenKind::$cond$(($($default),*))?),*
                ];
                $self.expect_error(&terminals);
                return Err(())
            },
        }
    };

    (
        str $self:expr => {
            $($str:literal => $res:expr,)*
        }
    ) => {
        match $self.current_token_str() {
            $($str => drop($res),)*
            _ => {
                let terminals = [
                    $($str),*
                ];
                $self.expect_str_error(&terminals);
                return Err(())
            },
        }
    };
}

mod parser;

pub use parser::{Parser, ParserState};
