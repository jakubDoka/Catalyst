#![feature(let_else)]

//! Crate defines all diagnostic structures and macros for constructing them.

pub extern crate lexing_t as inner_lexing;
pub extern crate lsp_types as raw;

/// macro produces '[`storage::Maybe`]<[`Location`]>'.
///
/// # Examples
/// ```
///
/// use lexing_t::*;
/// use diags::*;
/// use storage::*;
///
/// let mut interner = Interner::new();
/// let source = interner.intern_str("a.ctl");
/// let span = Span::new(0..0);
/// let value = Maybe::some(Loc {
///     span: Maybe::some(span),
///     source,
/// });
///
/// assert_eq!(location!(none), Maybe::<Loc>::none());
/// assert_eq!(location!(exp value), value);
/// assert_eq!(location!(span, source), value);
/// assert_eq!(location!(source), Maybe::some(Loc {
///     span: Maybe::none(),
///     source,
/// }));
/// ```
#[macro_export]
macro_rules! location {
    (none) => {
        Maybe::none()
    };
    (exp $value:expr) => {
        $value
    };
    ($source:expr $(,)?) => {
        Maybe::some($crate::Loc {
            span: Maybe::none(),
            source: $source,
        })
    };
    ($span:expr, $source:expr $(,)?) => {
        Maybe::some($crate::Loc {
            span: $span.into(),
            source: $source,
        })
    };
}

/// macro offers shorthand for [`raw::DiagnosticSeverity`].
///
/// # Examples
/// ```
/// use diags::*;
/// use diags::raw::*;
///
/// assert_eq!(severity!(), DiagnosticSeverity::ERROR);
/// assert_eq!(severity!(error), DiagnosticSeverity::ERROR);
/// assert_eq!(severity!(warning), DiagnosticSeverity::WARNING);
/// assert_eq!(severity!(information), DiagnosticSeverity::INFORMATION);
/// assert_eq!(severity!(hint), DiagnosticSeverity::HINT);
/// ```
#[macro_export]
macro_rules! severity {
    () => {
        $crate::raw::DiagnosticSeverity::ERROR
    };
    (error) => {
        $crate::severity!()
    };
    (warning) => {
        $crate::raw::DiagnosticSeverity::WARNING
    };
    (information) => {
        $crate::raw::DiagnosticSeverity::INFORMATION
    };
    (hint) => {
        $crate::raw::DiagnosticSeverity::HINT
    };
}

/// macro offers shorthand for '[`Vec`]<[`DiagRel`]>' with formatted messages
/// and optional locations.
///
/// # Examples
/// ```
/// use diags::*;
/// use diags::raw::*;
/// use storage::*;
/// use lexing_t::*;
///
/// let mut interner = Interner::new();
/// let source = interner.intern_str("a.ctl");
/// let span = Span::new(0..0);
///
///
/// let diags = related!(
///     (none) => "no location",
///     (span, source) => "located here",
///     (source) => "parametrized {}" { 10 },
/// );
///
/// let result = vec![
///     DiagRel {
///         loc: location!(none),
///         message: "no location".into(),
///     },
///     DiagRel {
///         loc: location!(span, source),
///         message: "located here".into(),
///     },
///     DiagRel {
///         loc: location!(source),
///         message: "parametrized 10".into(),
///     },
/// ];
///
/// assert_eq!(diags, result);
/// ```
#[macro_export]
macro_rules! related {
    (
        $(
            ($($loc:tt)*) => $message:literal $({
                $($message_arg:expr),* $(,)?
            })? $(,)?
        )*
    ) => {
        vec![
            $(
                $crate::DiagRel {
                    loc: $crate::location!($($loc)*),
                    message: format!($message $(, $($message_arg),*)?),
                },
            )*
        ]
    };
}

/// macro shorthand for [`Diag`] with formatted messages, optional
/// location and relations.
///
/// # Examples
/// ```
/// use diags::*;
/// use diags::raw::*;
/// use storage::*;
/// use lexing_t::*;
///
/// let mut interner = Interner::new();
/// let source = interner.intern_str("a.ctl");
/// let span = Span::new(0..0);
/// let span2 = Span::new(10..11);
///
/// let diag = diag!(
///     (span, source) warning => "{} code to {}" { "unpleasant", "loot at" },
///     (none) => "i mean it",
///     (source) => "this file",
/// );
///
/// let result = Diag {
///     severity: severity!(warning),
///     loc: location!(span, source),
///     message: "unpleasant code to loot at".into(),
///     related: related!(
///         (none) => "i mean it",
///         (source) => "this file",
///     ),
/// };
///
/// assert_eq!(diag, result);
/// ```
#[macro_export]
macro_rules! diag {
    (
        ($($loc:tt)*) $($severity:ident)? => $message:literal $({
            $($message_arg:expr),* $(,)?
        })?

        $(, $($related:tt)*)?
    ) => {
        $crate::Diag {
            severity: $crate::severity!($($severity)?),
            loc: $crate::location!($($loc)*),
            message: format!($message $(, $($message_arg),*)?),
            related: $crate::related!($($($related)*)?),
        }
    };
}

pub use types::{Diag, DiagRel, Doc, Loc, Workspace};

mod types {
    use std::fmt::Write;

    use ansi_coloring::*;
    use inner_lexing::*;
    use packaging_t::*;
    use storage::*;

    /// Represents diagnostic state of compiled project.
    #[derive(Default)]
    pub struct Workspace {
        files: Map<Doc>,
        global_diags: Vec<Diag>,
        has_errors: bool,
    }

    impl Workspace {
        pub fn new() -> Self {
            Self::default()
        }

        pub fn push(&mut self, diag: Diag) {
            self.has_errors |= diag.severity == raw::DiagnosticSeverity::ERROR;
            if let Some(loc) = diag.loc.expand() {
                if self.files.get(loc.source).is_none() {
                    self.files.insert(loc.source, Doc::new());
                }

                if loc.span.is_some() {
                    self.files
                        .entry(loc.source)
                        .or_default()
                        .code_diags
                        .push(diag);
                } else {
                    self.files
                        .entry(loc.source)
                        .or_default()
                        .global_diags
                        .push(diag);
                }
            } else {
                self.global_diags.push(diag);
            }
        }

        pub fn log(&self, packages: &Packages, style: &Style) {
            let mut to = String::new();
            self.display(packages, &mut to, style).unwrap();
            println!("{}", to);
        }

        pub fn display(
            &self,
            packages: &Packages,
            to: &mut dyn Write,
            style: &Style,
        ) -> std::fmt::Result {
            for diag in &self.global_diags {
                diag.display(packages, to, style)?;
            }

            for doc in self.files.values() {
                doc.display(packages, to, style)?;
            }

            Ok(())
        }

        pub fn has_errors(&self) -> bool {
            self.has_errors
        }
    }

    /// Represents diagnostic state of source file.
    #[derive(Default)]
    pub struct Doc {
        pub code_diags: Vec<Diag>,
        pub global_diags: Vec<Diag>,
    }

    impl Doc {
        pub fn new() -> Self {
            Self::default()
        }

        pub fn display(
            &self,
            packages: &Packages,
            to: &mut dyn Write,
            style: &Style,
        ) -> std::fmt::Result {
            for diag in &self.global_diags {
                diag.display(packages, to, style)?;
            }

            for diag in &self.code_diags {
                diag.display(packages, to, style)?;
            }

            Ok(())
        }
    }

    /// Represents singular error.
    #[derive(Debug, PartialEq, Eq)]
    pub struct Diag {
        pub severity: raw::DiagnosticSeverity,
        pub loc: Maybe<Loc>,
        pub message: String,
        pub related: Vec<DiagRel>,
    }

    impl Diag {
        pub fn display(
            &self,
            packages: &Packages,
            to: &mut dyn Write,
            style: &Style,
        ) -> std::fmt::Result {
            let color = color_of(self.severity, style);
            if let Some(loc) = self.loc.expand() {
                loc.display(color, packages, to, style)?;
            } else {
                write!(to, "| ")?;
            }
            writeln!(to, "{color}{}{}", self.message, style.end)?;

            for rel in &self.related {
                rel.display(packages, to, style)?;
            }

            writeln!(to)?;

            Ok(())
        }
    }

    /// Represents additional information about [`Diag`].
    #[derive(Debug, PartialEq, Eq)]
    pub struct DiagRel {
        pub loc: Maybe<Loc>,
        pub message: String,
    }

    impl DiagRel {
        pub fn display(
            &self,
            packages: &Packages,
            to: &mut dyn Write,
            style: &Style,
        ) -> std::fmt::Result {
            let color = style.weak;
            if let Some(loc) = self.loc.expand() {
                loc.display(color, packages, to, style)?;
            } else {
                write!(to, "| ")?;
            }
            writeln!(to, "{color}{}{}", self.message, style.end)?;

            Ok(())
        }
    }

    /// Represents location of [`Diag`].
    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    pub struct Loc {
        pub span: Maybe<Span>,
        pub source: Ident,
    }

    impl Loc {
        fn display(
            &self,
            color: &str,
            packages: &Packages,
            to: &mut dyn Write,
            style: &Style,
        ) -> std::fmt::Result {
            let Some(module) = packages.modules.get(self.source) else {
                writeln!(to, "| no source information")?;
                return Ok(());
            };

            if let Some(span) = self.span.expand() {
                let (line, col) = module.line_mapping.line_info_at(span.start());
                writeln!(to, "| {}:{}:{}", module.path.display(), line, col)?;
                span.underline(color, "^", &module.content, to, style)?;
                write!(to, " ")
            } else {
                write!(to, "| {}: ", module.path.display())
            }
        }
    }

    impl Invalid for Loc {
        unsafe fn invalid() -> Self {
            Loc {
                span: Maybe::default(),
                source: Ident::invalid(),
            }
        }

        fn is_invalid(&self) -> bool {
            self.source.is_invalid()
        }
    }

    fn color_of<'a>(severity: raw::DiagnosticSeverity, style: &'a Style) -> &'a str {
        match severity {
            raw::DiagnosticSeverity::ERROR => style.err,
            raw::DiagnosticSeverity::WARNING => style.warn,
            raw::DiagnosticSeverity::INFORMATION => style.info,
            raw::DiagnosticSeverity::HINT => style.weak,
            _ => unreachable!(),
        }
    }
}
