use errors::lsp_types::*;
use storage::*;

use crate::{Span, LineMapping};

pub struct Diagnostics {
    ents: Vec<Diagnostic>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Self {
            ents: Vec::new(),
        }
    }

    pub fn push(&mut self, ent: Diagnostic) {
        self.ents.push(ent);
    }
}

pub struct Diagnostic {
    pub span: Span,
    pub severity: DiagnosticSeverity,
    pub message: String,
    pub source: Ident,
    pub related: Vec<RelatedDiagnostic>,
}

impl Diagnostic {
    pub fn into_lsp_diagnostic(self, interner: &Interner, line_mapping: &LineMapping) -> errors::lsp_types::Diagnostic {
        errors::lsp_types::Diagnostic {
            range: self.span.into_lsp_range(line_mapping),
            severity: Some(self.severity),
            message: self.message,
            source: Some(interner[self.source].to_owned()),
            related_information: Some(self.related.into_iter().map(|related| related
                .into_lsp_related_diagnostic(interner, line_mapping)).collect()),

            ..Default::default()
        }
    }
}

pub struct RelatedDiagnostic {
    pub span: Span,
    pub source: Ident,
    pub message: String,
}

impl RelatedDiagnostic {
    pub fn into_lsp_related_diagnostic(self, interner: &Interner, line_mapping: &LineMapping) -> errors::lsp_types::DiagnosticRelatedInformation {
        let location = Location {
            uri: Url::parse(&interner[self.source]).unwrap(),
            range: self.span.into_lsp_range(line_mapping),
        };
        errors::lsp_types::DiagnosticRelatedInformation {
            location,
            message: self.message,
        }
    }
}