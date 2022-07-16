#![feature(let_else)]

#[macro_export]
macro_rules! bat {
    ($(($($code:tt)*))*) => {
        [$(
            bat!(__line__ $($code)*)
        ),*]
    };

    (__line__ $command:ident $($($arg:tt)*),*) => {
        {
            let mut cmd = std::process::Command::new(stringify!($command));
            $(
                bat!(__arg__ cmd, $($arg)*);
            )*
            cmd
        }
        
    };

    (__arg__ $cmd:expr, $expr:expr) => {
        $cmd.arg(format!("{}", $expr))
    };

    (__arg__ $cmd:expr, ?$expr:expr) => {
        $cmd.args($expr.into_iter())
    };
}

fn test() {
    let _ = bat! {
        (sudo "--10")
    };
}

pub mod modules;
pub mod packages;
pub mod state_gen;
pub mod version;

pub use state_gen::PackageLoader;