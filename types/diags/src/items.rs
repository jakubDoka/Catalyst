use std::borrow::Cow;

use lexing_t::Span;
use storage::{Ident, Maybe};

#[derive(Default)]
pub struct Workspace {
    sippets: Vec<Sippet>,
    error_count: usize,
}

impl Workspace {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, sippet: Sippet) {
        self.error_count += sippet
            .title
            .as_ref()
            .map_or(false, |t| t.annotation_type == AnnotationType::Error)
            as usize;
        self.sippets.push(sippet);
    }

    pub fn error_count(&self) -> ErrorCount {
        ErrorCount(self.error_count)
    }

    pub fn got_errors_since(&self, error_count: ErrorCount) -> bool {
        self.error_count > error_count.0
    }

    pub fn has_errors(&self) -> bool {
        self.error_count > 0
    }
}

#[derive(Clone, Copy)]
pub struct ErrorCount(usize);

impl ErrorCount {
    pub fn has_errors(&self) -> bool {
        self.0 > 0
    }
}

pub type Str = Cow<'static, str>;

#[derive(Default)]
pub struct Sippet {
    pub title: Option<Annotation>,
    pub footer: Vec<Option<Annotation>>,
    pub slices: Vec<Option<Slice>>,
    pub opt: FormatOptions,
}

#[derive(Default)]
pub struct Annotation {
    pub id: Option<Str>,
    pub label: Option<Str>,
    pub annotation_type: AnnotationType,
}

#[derive(Default)]
pub struct Slice {
    pub span: Span,
    pub origin: Maybe<Ident>,
    pub annotations: Vec<Option<SourceAnnotation>>,
    pub fold: bool,
}

#[derive(Default)]
pub struct FormatOptions {
    pub color: bool,
    pub anonymized_line_numbers: bool,
    pub margin: Option<Margin>,
}

#[derive(Default)]
pub struct Margin {
    pub whitespace_left: usize,
    pub span_left: usize,
    pub span_right: usize,
    pub label_right: usize,
    pub column_width: usize,
    pub max_line_len: usize,
}

#[derive(Default)]
pub struct SourceAnnotation {
    pub range: Span,
    pub label: Option<Str>,
    pub annotation_type: AnnotationType,
}

#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub enum AnnotationType {
    #[default]
    Error,
    Warning,
    Info,
    Note,
    Help,
}
