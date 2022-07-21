pub const ERR: &str = "\x1B[38;5;1m\x1B[1m";
pub const SUCCESS: &str = "\x1B[38;5;2m\x1B[1m";
pub const WARN: &str = "\x1B[38;5;3m\x1B[1m";
pub const INFO: &str = "\x1B[38;5;4m\x1B[1m";
pub const WEAK: &str = "\x1B[38;5;246m";
pub const END: &str = "\x1B[0m";
pub const HIGHLIGHT: &str = "\x1B[1m";

pub struct Style<'a> {
    pub err: &'a str,
    pub success: &'a str,
    pub warn: &'a str,
    pub info: &'a str,
    pub weak: &'a str,
    pub highlight: &'a str,

    pub end: &'a str,
}

impl<'a> Style<'a> {
    pub const DEFAULT: Self = Self {
        err: ERR,
        success: SUCCESS,
        warn: WARN,
        info: INFO,
        weak: WEAK,
        highlight: HIGHLIGHT,
        end: END,
    };

    pub const NONE: Self = Self {
        err: "",
        success: "",
        warn: "",
        info: "",
        weak: "",
        highlight: "",
        end: "",
    };
}
