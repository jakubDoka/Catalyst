#![feature(let_chains)]
//! Crate provides simple constructs of lexical analysis.

#[macro_export]
macro_rules! diagnostic {
    (
        loc: ($span:expr, $source:expr), 
        message: $message:literal $(=> ($($message_arg:expr),* $(,)?))?,
        level: $level:ident,
        $(related: (
            $(
                ($related_span:expr, $related_source:expr) => $related_message:literal $(=> (
                    $($related_message_arg:expr),* $(,)?
                ))?,
            )*
        ),)?
    ) => {
        Diagnostic {
            span: $span,
            severity: errors::lsp_types::DiagnosticSeverity::$level,
            message: format!($message $($(, $message_arg)*)?),
            source: $source,
            related: vec![$($(
                RelatedDiagnostic {
                    span: $related_span,
                    source: $related_source,
                    message: format!($related_message $(, $related_message_arg)*),
                },
            )*)?],
        }
    };
}

pub mod token;
pub mod span;
pub mod lexer;
pub mod line_mapper;
pub mod diagnostic;

pub use token::{Token, TokenKind};
pub use span::Span;
pub use lexer::Lexer;
pub use line_mapper::LineMapping;
pub use diagnostic::{Diagnostic, Diagnostics, RelatedDiagnostic};