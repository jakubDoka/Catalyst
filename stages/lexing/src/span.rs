use std::ops::Range;

use ansi_coloring::*;

/// Identical to [`std::ops::Range`]<[`u32`]> but a lot more ergonomic
/// since it is defined here.
#[derive(Default, Clone, Copy)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    /// Useful for short expressive construction.
    #[inline]
    pub fn new(range: Range<usize>) -> Self {
        Span {
            start: range.start as u32,
            end: range.end as u32,
        }
    }

    /// Simply start as usize but less wordy.
    #[inline]
    pub fn start(&self) -> usize {
        self.start as usize
    }

    /// Simply end as usize but less wordy.
    #[inline]
    pub fn end(&self) -> usize {
        self.end as usize
    }

    /// Distance between start and end.
    #[inline]
    pub fn len(&self) -> usize {
        (self.end - self.start) as usize
    }

    /// Opposite of [`Self::new`].
    #[inline]
    pub fn range(&self) -> Range<usize> {
        self.start()..self.end()
    }

    pub fn join(self, other: Self) -> Self {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    /// Underlines the span for error display, the performance is not important here.
    /// 
    /// # Examples
    /// ```
    /// use lexing::Span;
    /// use ansi_coloring::{ERR, END, HIGHLIGHT};
    /// 
    /// let source = r#"
    /// fn main() -> int { 
    ///     return 0 
    /// }
    /// "#;
    /// let span = Span::new(source.find('{').unwrap()..source.find('}').unwrap() + 1);
    /// let mut buff = String::new();
    /// span.underline(ERR, "^", source, &mut buff).unwrap();
    /// 
    /// let expected = format!(r#"
    /// | fn main() -> int {HIGHLIGHT}{{ 
    /// |     return 0 
    /// | }}{END}
    /// | {ERR}^^^^^^^^^^^^^^^^^^{END}
    /// "#);
    /// 
    /// assert_eq!(buff, &expected[1..]);
    /// ```
    #[inline(never)]
    pub fn underline(&self, color: &str, pattern: &str, source: &str, target: &mut dyn std::fmt::Write) -> std::fmt::Result {
        let Range { start, end } = self.range();
 
        let region_start = source[..start].rfind('\n').map(|i| i + 1).unwrap_or(0);
        let region_end = source[end..].find('\n').map(|i| i + end).unwrap_or(source.len());
        
        let line_count = source[start..end].lines().count();
        
        let mut underline_start = start - region_start;
        let mut underline_end = 0;
        let skip_range = (line_count >= 5)
            .then(|| 2..line_count - 2)
            .unwrap_or(0..0);

        for (i, line) in source[region_start..end].lines().enumerate() {
            if skip_range.contains(&i) {
                continue;
            }

            if i != 0 && let Some(start) = line.find(|c: char| !c.is_whitespace()) {
                underline_start = underline_start.min(start);
            }
            
            if let Some(end) = line.rfind(|c: char| !c.is_whitespace()) {
                underline_end = underline_end.max(end + 1);
            }
        }

        let display = format!(
            "{}{HIGHLIGHT}{}{END}{}",
            &source[region_start..start],
            &source[start..end],
            &source[end..region_end],
        )
        .replace("\n", "\n| ");

        writeln!(target, "| {}", display)?;
        writeln!(target, "| {}{color}{}{END}", " ".repeat(underline_start), pattern.repeat(underline_end - underline_start))?;

        Ok(())
    }

    pub fn into_lsp_range(&self, line_mapping: &crate::LineMapping) -> errors::lsp_types::Range {
        errors::lsp_types::Range::new(
            {
                let (lind, col) = line_mapping.line_info_at(self.start());
                errors::lsp_types::Position::new(lind as u32, col as u32)
            },
            {
                let (lind, col) = line_mapping.line_info_at(self.end());
                errors::lsp_types::Position::new(lind as u32, col as u32)
            }
        )
    }
}

