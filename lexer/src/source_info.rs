use std::{
    ops::{IndexMut, Range, RangeBounds},
    path::{Path, PathBuf},
};

use cranelift_entity::{packed_option::ReservedValue, PrimaryMap};

use crate::Map;

pub type Sources = PrimaryMap<Source, SourceEnt>;

impl SourcesExt for Sources {}

pub trait SourcesExt: IndexMut<Source, Output = SourceEnt> {
    fn display(&self, span: Span) -> &str {
        &self[span.source].content[span.range()]
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

crate::gen_entity!(Source);

#[derive(Default)]
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
            return (0, 0);
        }

        match self.new_lines.binary_search(&(pos as u32)) {
            Ok(i) | Err(i) => (i - 1, pos - self.new_lines[i - 1] as usize),
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

    pub fn log(&self, sources: &Sources) -> String {
        let mut string = String::new();
        self.loc_to(sources, &mut string).unwrap();
        self.underline_to(
            ansi_term::Color::Red.normal(),
            '^',
            sources,
            &mut string,
            &|_| Ok(()),
        )
        .unwrap();
        string
    }

    pub fn underline_to(
        &self,
        color: ansi_term::Style,
        underline_char: char,
        sources: &Sources,
        to: &mut String,
        message: &dyn Fn(&mut String) -> std::fmt::Result,
    ) -> std::fmt::Result {
        use std::fmt::Write;
        if self.is_reserved_value() {
            return writeln!(to, "undefined span");
        }

        let (prefix, suffix) = (color.prefix(), color.suffix());

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
            line + 1,
            col + 1
        )
    }
}

impl ReservedValue for Span {
    fn reserved_value() -> Self {
        Span::default()
    }

    fn is_reserved_value(&self) -> bool {
        self.source.is_reserved_value()
    }
}
