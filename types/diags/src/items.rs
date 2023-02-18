use std::mem;

use resources::*;
use span::*;
use storage::*;

pub trait SnippetDisplay {
    fn display(&mut self, error: &dyn CtlError, packages: &Resources, out: &mut String);
}

pub trait CtlError: Send + Sync + 'static {
    fn is_fatal(&self) -> bool;
    fn fill_snippet(&self, snippet: &mut CtlSnippet);
}

pub trait AddCtlError: Sized + CtlError {
    fn add(self, workspace: &mut Workspace) -> Option<!> {
        workspace.push(self)
    }
}

impl<T: CtlError> AddCtlError for T {}

#[derive(Default)]
pub struct Workspace {
    snippets: Vec<Box<dyn CtlError>>,
    error_count: usize,
}

impl Workspace {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn clear(&mut self) {
        self.snippets.clear();
        self.error_count = 0;
    }

    pub fn drain(&mut self) -> impl Iterator<Item = Box<dyn CtlError>> + '_ {
        self.error_count = 0;
        self.snippets.drain(..)
    }

    pub fn transfer(&mut self, other: &mut Self) {
        self.snippets.append(&mut other.snippets);
        self.error_count += mem::take(&mut other.error_count);
    }

    pub fn display(
        &self,
        packages: &Resources,
        display: &mut impl SnippetDisplay,
        out: &mut String,
    ) {
        for snippet in &self.snippets {
            display.display(snippet.as_ref(), packages, out);
            out.push_str("\n\n");
        }
    }

    #[cold]
    #[inline(never)]
    pub fn push(&mut self, error: impl CtlError) -> Option<!> {
        self.error_count += error.is_fatal() as usize;
        self.snippets.push(Box::new(error));
        None
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

#[derive(Debug, Default, Clone)]
pub struct CtlSnippet {
    pub title: CtlAnnotation,
    pub footer: Vec<CtlAnnotation>,
    pub source_annotations: Vec<CtlSourceAnnotation>,
}

impl CtlError for CtlSnippet {
    fn is_fatal(&self) -> bool {
        self.title.annotation_type == CtlAnnotationType::Error
    }

    fn fill_snippet(&self, snippet: &mut CtlSnippet) {
        self.clone_into(snippet);
    }
}

impl CtlSnippet {
    pub fn clear(&mut self) {
        self.footer.clear();
        self.source_annotations.clear();
    }
}

#[derive(Debug, Default, Clone)]
pub struct CtlAnnotation {
    pub id: Option<String>,
    pub label: String,
    pub annotation_type: CtlAnnotationType,
}

#[derive(Debug, Clone)]
pub struct CtlSourceAnnotation {
    pub span: Span,
    pub origin: VRef<Source>,
    pub label: String,
    pub annotation_type: CtlAnnotationType,
}

impl CtlSourceAnnotation {
    pub fn new(
        span: impl Into<Option<Span>>,
        origin: impl Into<Option<VRef<Source>>>,
        label: String,
        annotation_type: CtlAnnotationType,
    ) -> Option<Self> {
        Some(Self {
            span: span.into()?,
            origin: origin.into()?,
            label,
            annotation_type,
        })
    }

    pub fn from_source_loc(
        source_loc: impl Into<Option<SourceLoc>>,
        label: String,
        annotation_type: CtlAnnotationType,
    ) -> Option<Self> {
        let source_loc: SourceLoc = source_loc.into()?;
        Some(Self {
            span: source_loc.span,
            origin: source_loc.origin,
            label,
            annotation_type,
        })
    }
}

#[derive(Clone, Copy)]
pub struct SourceLoc {
    pub origin: VRef<Source>,
    pub span: Span,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum CtlAnnotationType {
    #[default]
    Error,
    Warning,
    Info,
    Note,
    Help,
}
