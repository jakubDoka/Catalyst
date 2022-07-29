use lexing_t::*;
use logos::Logos;

pub const EQUAL_SIGN_PRECEDENCE: u8 = 14;

/// Smallest lexical component.
#[derive(Clone, Copy, Default)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

macro_rules! gen_kind {
    (
        keywords {
            $($keyword:ident = $keyword_repr:literal,)*
        }

        punctuation {
            $($punctuation:ident = $punctuation_repr:literal,)*
        }

        pairs {
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
            pub fn as_str(self) -> &'static str {
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

        #[derive(Clone, Copy, Logos, Debug, PartialEq, Eq)]
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
        Fn = "fn",
        Type = "type",
        Return = "return",
        Use = "use",
        Extern = "extern",
        If = "if",
        Else = "else",
        For = "for",
        Break = "break",
        Continue = "continue",
        Let = "let",
        Struct = "struct",
        Bound = "bound",
        Enum = "enum",
        Mut = "mut",
        Impl = "impl",
        As = "as",
        Match = "match",
        Pub = "pub",
        Priv = "priv",
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
        Tick = "`",
    }

    pairs {
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
        Int = "[0-9]+((i|u)(8|16|32|64)?)?",
        String = r#""(\\"|[^"])*""#,
        Bool = "(true|false)",
        Char = r"'(.|\\(n|r|t|\\|'))'",
    }

    skipped {
        Space = r"[ \r\t]+",
        Comment = r"(/\*([^*]/|\*[^/]|[^*/])*\*/|//[^\n]*)",
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

impl IntoIterator for TokenKind {
    type Item = TokenKind;
    type IntoIter = std::iter::Once<TokenKind>;

    fn into_iter(self) -> Self::IntoIter {
        std::iter::once(self)
    }
}

impl Default for TokenKind {
    fn default() -> Self {
        TokenKind::None
    }
}
