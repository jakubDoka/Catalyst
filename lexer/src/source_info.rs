use std::{
    ops::{IndexMut, Range, RangeBounds},
    path::{Path, PathBuf},
};

use cranelift_entity::{packed_option::ReservedValue, PrimaryMap};

pub type Sources = PrimaryMap<Source, SourceEnt>;

impl SourcesExt for Sources {}

pub trait SourcesExt: IndexMut<Source, Output = SourceEnt> {
    fn display(&self, span: Span) -> &str {
        &self[span.source].content[span.range()]
    }
}

crate::gen_entity!(Source);

pub struct SourceEnt {
    pub path: PathBuf,
    pub content: String,
    pub mapping: LineMapping,
}

impl SourceEnt {
    pub fn new(path: PathBuf, content: String) -> Self {
        SourceEnt {
            path,
            mapping: LineMapping::new(&content),
            content,
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
}

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
        match self.new_lines.binary_search(&(pos as u32)) {
            Ok(i) | Err(i) => (i, pos - self.new_lines[i] as usize),
        }
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Span {
    start: u32,
    end: u32,
    source: Source,
}

impl Span {
    pub fn new(source: Source, start: usize, progress: usize) -> Span {
        Span {
            start: start as u32,
            end: progress as u32,
            source,
        }
    }

    pub fn strip_sides(&self) -> Span {
        Span {
            start: self.start + 1,
            end: self.end - 1,
            source: self.source,
        }
    }

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

    pub fn len(&self) -> usize {
        (self.end - self.start) as usize
    }

    pub fn range(&self) -> Range<usize> {
        self.start as usize..self.end as usize
    }

    pub fn pretty_print_with_line_info(
        &self,
        f: &mut impl std::fmt::Write,
        source: &str,
        path: &Path,
        line_mapping: &LineMapping,
    ) -> std::fmt::Result {
        let (line, col) = line_mapping.line_data_at(self.start as usize);

        writeln!(f, "|> {}:{}:{} ", path.display(), line + 1, col + 1)?;

        self.pretty_print(f, source)
    }

    pub fn pretty_print(&self, f: &mut impl std::fmt::Write, source: &str) -> std::fmt::Result {
        let range = self.range();
        let left = source[..range.start].rfind('\n').map_or(0, |i| i + 1);
        let right = source[range.end..]
            .find('\n')
            .map_or(source.len(), |i| i + range.end);

        let span = &source[range.clone()];

        if span == "\n" {
            writeln!(f, "| {}", &source[left..range.start])?;
            write!(f, "| ")?;
            for _ in left..range.start {
                write!(f, " ")?;
            }
            write!(f, "^")?;
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
            writeln!(f, "| {}", line)?;
        }

        write!(f, "| ")?;

        for _ in 0..min {
            write!(f, " ")?;
        }

        for _ in min..max {
            write!(f, "^")?;
        }

        Ok(())
    }

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

    pub fn source(&self) -> Source {
        self.source
    }
}

pub struct SpanLineDisplay<'a> {
    source_ent: &'a SourceEnt,
    span: Span,
}

impl<'a> SpanLineDisplay<'a> {
    pub fn new(source_ent: &'a SourceEnt, span: Span) -> Self {
        Self { source_ent, span }
    }
}

impl std::fmt::Display for SpanLineDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.span.pretty_print_with_line_info(
            f,
            self.source_ent.content(),
            self.source_ent.path(),
            self.source_ent.line_mapping(),
        )
    }
}

pub struct SpanDisplay<'a> {
    source: &'a str,
    span: Span,
}

impl<'a> SpanDisplay<'a> {
    pub fn new(source: &'a str, span: Span) -> Self {
        Self { source, span }
    }
}

impl std::fmt::Display for SpanDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.span.pretty_print(f, self.source)
    }
}
