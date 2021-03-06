use std::{
    ops::{IndexMut, Range, RangeBounds},
    path::{Path, PathBuf},
};

pub const EQUAL_SIGN_PRECEDENCE: u8 = 14;

use logos::Logos;

use storage::*;

macro_rules! gen_kind {
    (
        keywords {
            $($keyword:ident = $keyword_repr:literal,)*
        }

        punctuation {
            $($punctuation:ident = $punctuation_repr:literal,)*
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
        Return = "return",
        Use = "use",
        Extern = "extern",
        If = "if",
        Else = "else",
        Loop = "loop",
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
    }

    punctuation {
        LeftCurly = "{",
        RightCurly = "}",
        LeftParen = "(",
        RightParen = ")",
        LeftBracket = "[",
        RightBracket = "]",
        Comma = ",",
        Colon = ":",
        Dot = ".",
        RightArrow = "->",
        ThickRightArrow = "=>",
        DoubleColon = "::",
        Hash = "#",
        DoubleHash = "##",
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

impl Default for TokenKind {
    fn default() -> Self {
        TokenKind::None
    }
}

pub type Sources = PrimaryMap<Source, SourceEnt>;

impl SourcesExt for Sources {}

pub trait SourcesExt: IndexMut<Source, Output = SourceEnt> {
    fn display(&self, span: Span) -> &str {
        &self[span.source].content[span.range()]
    }

    fn id_of(&self, span: Span) -> ID {
        ID::new(self.display(span))
    }

    fn line_data_of(&self, span: Span) -> (usize, usize) {
        self[span.source].mapping.line_data_at(span.start as usize)
    }

    fn file_of(&self, span: Span) -> (u32, u32) {
        self[span.source].path_span
    }
}

pub struct BuiltinSource {
    pub spans: Map<Span>,
    pub source: Source,
}

impl BuiltinSource {
    pub fn new(sources: &mut Sources) -> Self {
        assert!(sources.is_empty());

        let source = SourceEnt {
            content: "".to_string(),
            path: PathBuf::new(),
            mapping: LineMapping::new(""),
            path_span: (0, 0),
        };

        let source = sources.push(source);

        Self {
            spans: Map::new(),
            source,
        }
    }

    pub fn make_span(&mut self, sources: &mut Sources, content: &str) -> Span {
        if let Some(&span) = self.spans.get(content) {
            return span;
        }

        let span = {
            let ent = &mut sources[self.source];
            let len = ent.content.len();
            ent.content.push_str(content);
            Span::new(self.source, len, len + content.len())
        };

        self.spans.insert(content, span);
        span
    }
}

gen_entity!(Source);

#[derive(Default)]
pub struct SourceEnt {
    pub path: PathBuf,
    pub content: String,
    pub mapping: LineMapping,
    pub path_span: (u32, u32),
}

impl SourceEnt {
    pub fn new(path: PathBuf, content: String) -> Self {
        SourceEnt {
            path,
            mapping: LineMapping::new(&content),
            content,
            path_span: (0, 0),
        }
    }

    pub fn content(&self) -> &str {
        &self.content
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn line_mapping(&self) -> &LineMapping {
        &self.mapping
    }

    pub fn line_count(&self) -> usize {
        self.mapping.line_count()
    }
}

#[derive(Default)]
pub struct LineMapping {
    new_lines: Vec<u32>,
}

impl LineMapping {
    pub fn new(file_content: &str) -> Self {
        Self {
            new_lines: std::iter::once(0)
                .chain(file_content.match_indices('\n').map(|(i, _)| i as u32))
                .chain(std::iter::once(file_content.len() as u32))
                .collect(),
        }
    }

    pub fn line_data_at(&self, pos: usize) -> (usize, usize) {
        if pos == 0 {
            return (1, 0);
        }

        match self.new_lines.binary_search(&(pos as u32)) {
            Ok(i) | Err(i) => (i, pos - self.new_lines[i - 1] as usize),
        }
    }

    pub fn line_count(&self) -> usize {
        self.new_lines.len() - 1
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Span {
    start: u32,
    end: u32,
    source: Source,
}

impl Span {
    #[inline]
    pub fn new(source: Source, start: usize, progress: usize) -> Span {
        Span {
            start: start as u32,
            end: progress as u32,
            source,
        }
    }

    #[inline]
    pub fn strip_sides(&self) -> Span {
        Span {
            start: self.start + 1,
            end: self.end - 1,
            source: self.source,
        }
    }

    #[inline]
    pub fn slice(&self, range: impl RangeBounds<usize>) -> Span {
        Span {
            start: self.start
                + match range.start_bound() {
                    std::ops::Bound::Excluded(b) => *b as u32,
                    std::ops::Bound::Included(b) => *b as u32,
                    std::ops::Bound::Unbounded => 0,
                },
            end: match range.end_bound() {
                std::ops::Bound::Excluded(b) => self.start + *b as u32,
                std::ops::Bound::Included(b) => self.start + *b as u32 + 1,
                std::ops::Bound::Unbounded => self.end,
            },
            source: self.source,
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        (self.end - self.start) as usize
    }

    #[inline]
    pub fn range(&self) -> Range<usize> {
        self.start as usize..self.end as usize
    }

    #[inline]
    pub fn join(self, other: Self) -> Self {
        if other.source.is_reserved_value() {
            return self;
        }

        assert!(self.source == other.source);

        Self {
            start: self.start,
            end: other.end,
            source: self.source,
        }
    }

    #[inline]
    pub fn source(&self) -> Source {
        self.source
    }

    #[inline(never)]
    pub fn log(&self, sources: &Sources) -> String {
        let mut string = String::new();
        self.loc_to(sources, &mut string).unwrap();
        self.underline_to(ansi_consts::ERR, '^', sources, &mut string, &|_| Ok(()))
            .unwrap();
        string
    }

    #[inline(never)]
    pub fn underline_error(
        &self,
        sources: &Sources,
        to: &mut String,
        message: &dyn Fn(&mut String) -> std::fmt::Result,
    ) -> std::fmt::Result {
        self.underline_to(ansi_consts::ERR, '^', sources, to, message)
    }

    #[inline(never)]
    pub fn underline_info(
        &self,
        sources: &Sources,
        to: &mut String,
        message: &dyn Fn(&mut String) -> std::fmt::Result,
    ) -> std::fmt::Result {
        self.underline_to(ansi_consts::INFO, '~', sources, to, message)
    }

    #[inline(never)]
    pub fn underline_to(
        &self,
        color: &str,
        underline_char: char,
        sources: &Sources,
        to: &mut String,
        message: &dyn Fn(&mut String) -> std::fmt::Result,
    ) -> std::fmt::Result {
        use std::fmt::Write;
        if self.is_reserved_value() {
            return writeln!(to, "undefined span");
        }

        let (prefix, suffix) = (color, ansi_consts::END);

        // called immediately for `?` use
        let source = &sources[self.source].content;
        let range = self.range();
        let left = source[..range.start].rfind('\n').map_or(0, |i| i + 1);
        let right = source[range.end..]
            .find('\n')
            .map_or(source.len(), |i| i + range.end);

        let span = &source[range.clone()];

        if span == "\n" {
            writeln!(to, "{prefix}|{suffix} {}", &source[left..range.start])?;
            write!(to, "{prefix}| ")?;
            for _ in left..range.start {
                write!(to, " ")?;
            }
            write!(to, "^")?;
            message(to)?;
            writeln!(to, "|{suffix}")?;
            return Ok(());
        }

        let (min, max) = span.split('\n').skip(1).fold(
            (
                range.start - left,
                if let Some(i) = span.find('\n') {
                    range.start - left + i
                } else {
                    range.end - left
                },
            ),
            |(min, max), line| {
                (
                    std::cmp::min(min, line.len() - line.trim_start().len()),
                    std::cmp::max(max, line.trim_end().len()),
                )
            },
        );

        for line in source[left..right].split('\n') {
            writeln!(to, "{prefix}|{suffix} {}", line)?;
        }

        write!(to, "{prefix}|{suffix} ")?;

        for _ in 0..min {
            write!(to, " ")?;
        }

        write!(to, "{prefix}")?;
        for _ in min..max {
            to.write_char(underline_char)?;
        }
        message(to)?;
        writeln!(to, "{suffix}")?;

        Ok(())
    }

    #[inline(never)]
    pub fn loc_to(&self, sources: &Sources, to: &mut impl std::fmt::Write) -> std::fmt::Result {
        if self.is_reserved_value() {
            return writeln!(to, "undefined span");
        }

        let source_ent = &sources[self.source()];

        let (line, col) = source_ent.mapping.line_data_at(self.range().start);

        writeln!(
            to,
            "|> {}:{}:{} ",
            source_ent.path.display(),
            line,
            col
        )
    }
}

impl ReservedValue for Span {
    #[inline]
    fn reserved_value() -> Self {
        Span::default()
    }

    #[inline]
    fn is_reserved_value(&self) -> bool {
        self.source.is_reserved_value()
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    #[inline]
    pub fn new(kind: TokenKind, span: Span) -> Token {
        Self { kind, span }
    }

    #[inline]
    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    #[inline]
    pub fn range(&self) -> Range<usize> {
        self.span.range()
    }

    #[inline]
    pub fn span(&self) -> Span {
        self.span
    }
}
