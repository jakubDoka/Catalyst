#![feature(let_else)]
#![feature(default_free_fn)]

use annotate_snippets::{
    display_list::{DisplayList, FormatOptions},
    snippet::*,
};
use lexing_t::Span;
use packaging_t::Resources;

#[derive(Default)]
pub struct SnippetDisplay {
    opts: FormatOptions,
    tab_width: usize,
}

impl diags::SnippetDisplay for SnippetDisplay {
    fn display_snippet(&mut self, packages: &Resources, snippet: &diags::Snippet) -> String {
        if self.tab_width == 0 {
            self.tab_width = 4;
        }
        let mut buffer = String::new();
        let snippet = self.snippet(&mut buffer, packages, snippet);
        let d_list: DisplayList = snippet.into();
        d_list.to_string()
    }
}

impl SnippetDisplay {
    pub fn snippet<'a>(
        &'a self,
        buffer: &'a mut String,
        packages: &'a Resources,
        snippet: &'a diags::Snippet,
    ) -> Snippet<'a> {
        let slices = snippet
            .slices
            .iter()
            .filter_map(|s| s.as_ref())
            .map(|s| (s, packages.sources[s.origin].reveal_span_lines(s.span)))
            .map(|(s, span)| (span, packages.sources[s.origin].span_str(span)))
            .map(|(s, str)| (s, self.replace_tabs_with_spaces(buffer, str)))
            .collect::<Vec<_>>();

        Snippet {
            title: snippet.title.as_ref().map(|s| self.annotation(s)),
            footer: snippet
                .footer
                .iter()
                .filter_map(|s| s.as_ref())
                .map(|s| self.annotation(s))
                .collect(),
            slices: snippet
                .slices
                .iter()
                .filter_map(|i| i.as_ref())
                .zip(slices)
                .map(|(s, (span, str))| self.slice(packages, str, span, buffer, s))
                .collect(),
            opt: self.opts,
        }
    }

    fn annotation<'a>(&'a self, s: &'a diags::Annotation) -> Annotation<'a> {
        Annotation {
            id: s.id.as_ref().map(|s| s.as_ref()),
            label: s.label.as_ref().map(|s| s.as_ref()),
            annotation_type: self.annotation_type(s.annotation_type),
        }
    }

    fn slice<'a>(
        &'a self,
        packages: &'a Resources,
        str: Span,
        span: Span,
        buffer: &'a str,
        slice: &'a diags::Slice,
    ) -> Slice<'a> {
        let source = &packages.sources[slice.origin];
        Slice {
            source: &buffer[str.range()],
            line_start: source.line_mapping.line_info_at(slice.span.start()).0,
            origin: Some(source.path.to_str().unwrap_or("<invalid path>")),
            annotations: slice
                .annotations
                .iter()
                .filter_map(|i| i.as_ref())
                .map(|i| self.source_annotation(span.start(), &source.content, i))
                .collect(),
            fold: slice.fold,
        }
    }

    fn compute_str_length(&self, s: &str) -> usize {
        let (mut len, mut local_len) = (0, 0);
        for c in s.chars() {
            match c {
                '\t' => {
                    let tab_len = self.tab_width - (local_len % self.tab_width);
                    local_len += tab_len;
                    len += tab_len;
                }
                '\n' => {
                    local_len = 0;
                    len += 1;
                }
                _ => {
                    local_len += 1;
                    len += 1;
                }
            }
        }
        len
    }

    fn replace_tabs_with_spaces(&self, buffer: &mut String, s: &str) -> Span {
        let prev_len = buffer.len();
        buffer.reserve(self.compute_str_length(s));
        let mut local_len = 0;
        for c in s.chars() {
            match c {
                '\t' => {
                    let tab_len = self.tab_width - (local_len % self.tab_width);
                    local_len += tab_len;
                    for _ in 0..tab_len {
                        buffer.push(' ');
                    }
                }
                '\n' => {
                    local_len = 0;
                    buffer.push('\n');
                }
                _ => {
                    local_len += 1;
                    buffer.push(c);
                }
            }
        }
        Span::new(prev_len..buffer.len())
    }

    fn source_annotation<'a>(
        &self,
        start: usize,
        source: &str,
        source_annotation: &'a diags::SourceAnnotation,
    ) -> SourceAnnotation<'a> {
        let end = self.compute_str_length(&source[..source_annotation.range.end() - start]);
        let start = self.compute_str_length(&source[..source_annotation.range.start() - start]);
        SourceAnnotation {
            range: (start, end),
            label: source_annotation.label.as_ref(),
            annotation_type: self.annotation_type(source_annotation.annotation_type),
        }
    }

    fn annotation_type(&self, annotation_type: diags::AnnotationType) -> AnnotationType {
        match annotation_type {
            diags::AnnotationType::Error => AnnotationType::Error,
            diags::AnnotationType::Warning => AnnotationType::Warning,
            diags::AnnotationType::Note => AnnotationType::Note,
            diags::AnnotationType::Help => AnnotationType::Help,
            diags::AnnotationType::Info => AnnotationType::Info,
        }
    }
}
