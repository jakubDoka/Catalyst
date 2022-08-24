#![feature(default_free_fn)]
#![feature(let_else)]
#![feature(type_alias_impl_trait)]

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
            $(_ => $default_branch:expr,)?
        }
    ) => {
        match $self.state.current.kind {
            $(TokenKind::$cond$(($($value),*))? => {$res;},)*
            _ => {
                branch!(__default_branch__ $self, ($($cond$(($($value = $default),*))?),*) $($default_branch)?)
            },
        }
    };

    (__default_branch__ $self:expr, ($($cond:ident$(($($value:pat = $default:expr),*))?),*)) => {
        {
            let terminals = [
                $(TokenKind::$cond$(($($default),*))?),*
            ];
            $self.expect_error(terminals);
            return Err(())
        }
    };


    (__default_branch__ $self:expr, ($($ignored:tt)*) $default_branch:expr) => {
        {
            $default_branch
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
