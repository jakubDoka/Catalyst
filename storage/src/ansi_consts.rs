pub const ERR: &str = "\x1B[38;5;1m\x1B[1m";
pub const SUCCESS: &str = "\x1B[38;5;2m\x1B[1m";
pub const WARNING: &str = "\x1B[38;5;3m\x1B[1m";
pub const INFO: &str = "\x1B[38;5;4m\x1B[1m";

pub const END: &str = "\x1B[0m";

#[macro_export]
macro_rules! write_colored {
    ($to:expr, $color:expr, $str:expr, $($args:expr),* $(,)?) => {
        write!($to, "{}{}{}", $color, format!($str, $($args),*), $crate::ansi_consts::END)
    };
}

#[test]
fn test() {
    println!("{}error{}", ERR, END);
    println!("{}warning{}", WARNING, END);
    println!("{}info{}", INFO, END);
    println!("{}success{}", SUCCESS, END);
}
