#![feature(default_free_fn)]
#![feature(slice_group_by)]

use std::{iter, mem};

use annotate_snippets::{
    display_list::{DisplayList, FormatOptions},
    snippet::*,
};
use diags::CtlSnippet;
use lexing_t::Span;
use packaging_t::Resources;
use std::fmt::Write;

pub use annotate_snippets;

#[derive(Default)]
pub struct SnippetDisplayImpl {
    pub opts: FormatOptions,
    pub tab_width: Option<usize>,
    pub snippet: CtlSnippet,
    pub buffer: String,
}

impl diags::SnippetDisplay for SnippetDisplayImpl {
    fn display(&mut self, error: &dyn diags::CtlError, packages: &Resources, out: &mut String) {
        let tab_width = self.tab_width.unwrap_or(4);
        let mut internal_snippet = mem::take(&mut self.snippet);
        error.fill_snippet(&mut internal_snippet);
        internal_snippet
            .source_annotations
            .sort_unstable_by_key(|a| a.origin);
        let mut buffer = mem::take(&mut self.buffer);
        let snippet = self.snippet(&mut buffer, packages, &internal_snippet, tab_width);
        let d_list: DisplayList = snippet.into();
        write!(out, "{d_list}").unwrap();
        buffer.clear();
        self.buffer = buffer;
        internal_snippet.clear();
        self.snippet = internal_snippet;
    }
}

impl SnippetDisplayImpl {
    pub fn snippet<'a>(
        &'a self,
        buffer: &'a mut String,
        packages: &'a Resources,
        snippet: &'a diags::CtlSnippet,
        tab_width: usize,
    ) -> Snippet<'a> {
        let mut slices = vec![];
        for g in snippet
            .source_annotations
            .group_by(|a, b| a.origin == b.origin)
        {
            let origin = g[0].origin;
            let span = g.iter().map(|a| a.span).reduce(|a, b| a.joined(b)).unwrap();
            let span = packages.sources[origin].reveal_span_lines(span);
            let span_str = packages.sources[origin].span_str(span);
            let fixed_span_str = Self::replace_tabs_with_spaces(buffer, span_str, tab_width);
            slices.push((span, origin, fixed_span_str));
        }

        let slices = snippet
            .source_annotations
            .group_by(|a, b| a.origin == b.origin)
            .zip(slices)
            .map(|(g, (span, origin, fixed_span_str))| {
                let source = &packages.sources[origin];
                Slice {
                    source: &buffer[fixed_span_str.range()],
                    line_start: source.line_mapping.line_info_at(span.start as usize).0,
                    origin: Some(
                        packages.sources[origin]
                            .path
                            .to_str()
                            .unwrap_or("<invalid path>"),
                    ),
                    annotations: g
                        .iter()
                        .map(|a| {
                            Self::source_annotation(
                                span.start as usize,
                                &source.content,
                                tab_width,
                                a,
                            )
                        })
                        .collect(),
                    fold: false,
                }
            })
            .collect();

        Snippet {
            title: Some(Self::annotation(&snippet.title)),
            footer: snippet.footer.iter().map(Self::annotation).collect(),
            slices,
            opt: self.opts,
        }
    }

    fn annotation(s: &diags::CtlAnnotation) -> Annotation {
        Annotation {
            id: s.id.as_ref().map(|s| s.as_ref()),
            label: Some(&s.label),
            annotation_type: Self::annotation_type(s.annotation_type),
        }
    }

    fn compute_str_length(s: &str, tab_width: usize) -> usize {
        let (mut len, mut local_len) = (0, 0);
        for c in s.chars() {
            match c {
                '\t' => {
                    let tab_len = tab_width - (local_len % tab_width);
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

    fn replace_tabs_with_spaces(buffer: &mut String, s: &str, tab_width: usize) -> Span {
        let prev_len = buffer.len();
        buffer.reserve(Self::compute_str_length(s, tab_width));
        let mut local_len = 0;
        for c in s.chars() {
            match c {
                '\t' => {
                    let tab_len = tab_width - (local_len % tab_width);
                    local_len += tab_len;
                    buffer.extend(iter::repeat(' ').take(tab_len));
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
        start: usize,
        source: &str,
        tab_width: usize,
        source_annotation: &'a diags::CtlSourceAnnotation,
    ) -> SourceAnnotation<'a> {
        let end = Self::compute_str_length(&source[start..source_annotation.span.end()], tab_width);
        let start =
            Self::compute_str_length(&source[start..source_annotation.span.start()], tab_width);
        SourceAnnotation {
            range: (start, end),
            label: source_annotation.label.as_ref(),
            annotation_type: Self::annotation_type(source_annotation.annotation_type),
        }
    }

    fn annotation_type(annotation_type: diags::CtlAnnotationType) -> AnnotationType {
        match annotation_type {
            diags::CtlAnnotationType::Error => AnnotationType::Error,
            diags::CtlAnnotationType::Warning => AnnotationType::Warning,
            diags::CtlAnnotationType::Note => AnnotationType::Note,
            diags::CtlAnnotationType::Help => AnnotationType::Help,
            diags::CtlAnnotationType::Info => AnnotationType::Info,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::SnippetDisplayImpl;

    #[test]
    fn test_str_len() {
        let check = |(input, output)| {
            assert_eq!(
                SnippetDisplayImpl::compute_str_length(input, 4),
                output,
                "for input: {input:?}"
            );
        };

        [
            ("\t", 4),
            ("   \t", 4),
            (" \n\t", 6),
            (" \n\thello", 11),
            (" \n \thello", 11),
            ("fn main {\n\treturn\n}", 22),
        ]
        .map(check);
    }
}
