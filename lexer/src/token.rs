use std::ops::Range;

use crate::source_info::Span;

#[derive(Debug, Clone, Copy, Default)]
pub struct Token {
    kind: Kind,
    span: Span,
}

impl Token {
    pub fn new(kind: Kind, span: Span) -> Token {
        Self { kind, span }
    }

    pub fn kind(&self) -> Kind {
        self.kind
    }

    pub fn range(&self) -> Range<usize> {
        self.span.range()
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

macro_rules! gen_kind {
    ($($name:ident$(($ty:ident))? = $repr:literal,)*) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum Kind {
            $($name$(($ty))?),*
        }

        impl Kind {
            pub fn as_str(&self) -> &'static str {
                #[allow(unused)]
                match self {
                    $(Self::$name$(($ty))? => $repr,)*
                }
            }
        }
    };
}

gen_kind!(
    Fn = "'fn'",
    Ret = "'ret'",
    Use = "'use'",
    Extern = "'extern'",
    If = "'if'",
    Else = "'else'",
    Loop = "'loop'",
    Break = "'break'",
    Let = "'let'",
    Struct = "'struct'",
    Bound = "'bound'",
    Mut = "'mut'",


    Ident = "<ident>",
    Operator = "<operator>",

    Int(i16) = "<int>",
    String = "<string>",
    Bool(bool) = "<bool>",

    LeftCurly = "'{'",
    RightCurly = "'}'",

    LeftParen = "'('",
    RightParen = "')'",

    LeftBracket = "'['",
    RightBracket = "']'",

    Comma = "','",
    Colon = "':'",
    Dot = "'.'",
    RightArrow = "'->'",
    DoubleColon = "'::'",

    NewLine = "'\\n' | ';'",

    Eof = "<eof>",

    None = "<none>",
);

impl Default for Kind {
    fn default() -> Self {
        Kind::None
    }
}
