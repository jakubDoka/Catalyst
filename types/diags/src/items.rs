use std::borrow::Cow;

use lexing_t::*;
use packaging_t::{Resources, Source};
use storage::*;

pub trait SnippetDisplay {
    fn display_snippet(&mut self, packages: &Resources, snippet: &Snippet) -> String;
}

#[derive(Default)]
pub struct Workspace {
    sippets: Vec<Snippet>,
    error_count: usize,
    display: Option<Box<dyn SnippetDisplay>>,
}

impl Workspace {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn display<'a>(
        &'a mut self,
        packages: &Resources,
        display: &'a mut dyn SnippetDisplay,
    ) -> String {
        let display: &'a mut _ = if let Some(ref mut display) = self.display {
            &mut **display
        } else {
            display
        };
        self.sippets
            .iter()
            .map(|s| display.display_snippet(packages, s))
            .flat_map(|s| [s, "\n\n".to_string()])
            .collect()
    }

    pub fn push_or_display(&mut self, packages: &Resources, snippet: Snippet) {
        if let Some(ref mut display) = self.display {
            let out = display.display_snippet(packages, &snippet);
            println!("{}", out);
        } else {
            self.push(snippet);
        }
    }

    pub fn push(&mut self, sippet: Snippet) {
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

#[derive(Debug)]
pub struct Snippet {
    pub title: Option<Annotation>,
    pub footer: Vec<Option<Annotation>>,
    pub slices: Vec<Option<Slice>>,
    pub origin: String,
}

#[derive(Debug)]
pub struct Annotation {
    pub id: Option<Str>,
    pub label: Option<Str>,
    pub annotation_type: AnnotationType,
}

#[derive(Debug)]
pub struct Slice {
    pub span: Span,
    pub origin: VRef<Source>,
    pub annotations: Vec<Option<SourceAnnotation>>,
    pub fold: bool,
}

#[derive(Debug)]
pub struct SourceAnnotation {
    pub range: Span,
    pub label: Str,
    pub annotation_type: AnnotationType,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum AnnotationType {
    #[default]
    Error,
    Warning,
    Info,
    Note,
    Help,
}
