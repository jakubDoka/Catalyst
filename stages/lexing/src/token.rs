use lexing_t::*;
use logos::Logos;
use std::fmt;

pub const EQUAL_SIGN_PRECEDENCE: u8 = 14;

#[derive(Clone, Copy, Default, Debug)]
#[repr(C)]
pub struct Token {
    pub kind: TokenKind,
    /// Span points to &str inside source file. Origin of span is
    /// implied by surrounding code.
    pub span: Span,
}

/// Generates the [`TokenKind`] struct and some useful methods.
/// It is simply reducing repetition.
macro_rules! gen_kind {
    (
        keywords {
            $($keyword:ident = $keyword_repr:literal,)*
        }

        punctuation {
            $($punctuation:ident = $punctuation_repr:literal,)*
        }

        paren_pairs {
            $(
                $pair0:ident = $pair_repr0:literal,
                $pair1:ident = $pair_repr1:literal,
            )*
        }

        literal {
            $($literal:ident = $literal_regex:literal,)*
        }

        skipped {
            $($skipped:ident = $skipped_regex:literal,)*
        }

        operators {
            $(($($op_lit:literal)+) = $op_precedence:expr,)*
        }
    ) => {


        impl TokenKind {
            pub const fn as_str(self) -> &'static str {
                match self {
                    $(
                        TokenKind::$keyword => concat!("'", $keyword_repr, "'"),
                    )*

                    $(
                        TokenKind::$punctuation => concat!("'", $punctuation_repr, "'"),
                    )*

                    $(
                        TokenKind::$pair0 => concat!("'", $pair_repr0, "'"),
                        TokenKind::$pair1 => concat!("'", $pair_repr1, "'"),
                    )*

                    $(
                        TokenKind::$literal => stringify!($literal),
                    )*

                    $(
                        TokenKind::$skipped => stringify!($skipped),
                    )*

                    TokenKind::Operator(..) => "Operator",
                    TokenKind::NewLine => "'\\n' | ';'",
                    TokenKind::Error => "<error>",
                    TokenKind::Eof => "<eof>",
                    TokenKind::None => "<none>",
                }
            }

            pub fn complement(self) -> Option<Self> {
                match self {
                    $(
                        TokenKind::$pair0 => Some(TokenKind::$pair1),
                    )*
                    _ => None,
                }
            }

            pub fn is_closing(self) -> bool {
                match self {
                    $(TokenKind::$pair1)|* => true,
                    _ => false,
                }
            }
        }

        impl fmt::Display for TokenKind {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}", self.as_str())
            }
        }

        #[derive(Clone, Copy, Logos, Debug, PartialEq, Eq)]
        #[repr(C, u8)]
        pub enum TokenKind {
            $(
                #[token($keyword_repr)]
                $keyword,
            )*

            $(
                #[token($punctuation_repr)]
                $punctuation,
            )*

            $(
                #[token($pair_repr0)]
                $pair0,
                #[token($pair_repr1)]
                $pair1,
            )*

            $(
                #[regex($literal_regex)]
                $literal,
            )*

            $(
                #[regex($skipped_regex, logos::skip)]
                $skipped,
            )*

            $(
                $(
                    #[token($op_lit, |_| $op_precedence)]
                )+
            )*
            Operator(u8),

            #[regex(r"(\n|;)")]
            NewLine,

            #[error]
            Error,
            Eof,
            None,
        }
    };
}

gen_kind!(
    keywords {
        Func = "fn",
        Type = "type",
        Return = "return",
        Use = "use",
        Extern = "extern",
        If = "if",
        Elif = "elif",
        Else = "else",
        For = "for",
        Loop = "loop",
        Break = "break",
        Continue = "continue",
        Let = "let",
        Struct = "struct",
        Spec = "spec",
        Enum = "enum",
        Mut = "mut",
        Impl = "impl",
        As = "as",
        Match = "match",
        Pub = "pub",
        Priv = "priv",
        Const = "const",
    }

    punctuation {
        Comma = ",",
        Colon = ":",
        Dot = ".",
        RightArrow = "->",
        ThickRightArrow = "=>",
        DoubleColon = "::",
        Hash = "#",
        DoubleHash = "##",
        BackSlash = "\\",
        DoubleDot = "..",
        Tilde = "~",
    }

    paren_pairs {
        LeftCurly = "{",
        RightCurly = "}",
        LeftParen = "(",
        RightParen = ")",
        LeftBracket = "[",
        RightBracket = "]",
    }

    literal {
        Label = "'[a-zA-Z0-9_]+",
        Ident = "[a-zA-Z_][a-zA-Z0-9_]*",
        Macro = r"[a-zA-Z_][a-zA-Z0-9_]*!",
        Int = "[0-9]+((u)(32)|uint)?",
        String = r#""(\\"|[^"])*""#,
        Bool = "(true|false)",
        Char = r"'(.|\\(n|r|t|\\|'))'",
    }

    skipped {
        Comment = r"(/\*([^*]/|\*[^/]|[^*/])*\*/|//[^\n]*)",
        Space = r"[ \r\t]+",
    }

    operators {
        ("*" "/" "%") = 3,
        ("+" "-") = 4,
        ("<<" ">>") = 5,
        ("<" ">" "<=" ">=") = 6,
        ("==" "!=") = 7,
        ("&") = 8,
        ("^") = 9,
        ("|") = 10,
        ("&&") = 11,
        ("||") = 12,
        (
            "=" "+="
            "-=" "*="
            "/=" "%="
            "<<=" ">>="
            "&=" "^="
            "|="
        ) = EQUAL_SIGN_PRECEDENCE,
    }
);

impl Default for TokenKind {
    #[inline]
    fn default() -> Self {
        TokenKind::None
    }
}
