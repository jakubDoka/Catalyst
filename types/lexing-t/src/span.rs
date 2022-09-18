use std::ops::{Bound, Range, RangeBounds};

use storage::*;

/// Identical to [`std::ops::Range`]<[`u32`]> but a lot more ergonomic
/// since it is defined here.
#[derive(Default, Clone, Copy, Debug, PartialEq, Eq)]
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

    #[inline]
    pub fn within(&self, other: Span) -> bool {
        self.start >= other.start && self.end <= other.end
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

    /// Returns true if the span is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    /// Opposite of [`Self::new`].
    #[inline]
    pub fn range(&self) -> Range<usize> {
        self.start()..self.end()
    }

    #[inline]
    pub fn joined(self, other: Self) -> Self {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    #[inline]
    pub fn shifted(self, shift: usize) -> Self {
        Span {
            start: self.start + shift as u32,
            end: self.end + shift as u32,
        }
    }

    #[inline]
    pub fn shrink(self, shift: usize) -> Self {
        Span {
            start: self.start + shift as u32,
            end: self.end - shift as u32,
        }
    }

    #[inline]
    pub fn sliced(self, range: impl RangeBounds<usize>) -> Self {
        let start = self.start()
            + match range.start_bound() {
                Bound::Included(i) => *i,
                Bound::Excluded(i) => *i + 1,
                Bound::Unbounded => 0,
            };
        let end = self.start()
            + match range.end_bound() {
                Bound::Included(i) => *i + 1,
                Bound::Excluded(i) => *i,
                Bound::Unbounded => self.len(),
            };
        Span {
            start: start as u32,
            end: end as u32,
        }
    }

    pub fn reveal_lines(&self, source: &str) -> Span {
        let start = self.start();
        let end = self.end();

        let start = source[..start].rfind('\n').map_or(0, |i| i + 1);
        let end = source[end..].find('\n').map_or(source.len(), |i| i + end);

        Span::new(start..end)
    }
}

impl Invalid for Span {
    unsafe fn invalid() -> Self {
        Span {
            start: u32::MAX,
            end: u32::MAX,
        }
    }

    fn is_invalid(&self) -> bool {
        self.start == u32::MAX && self.end == u32::MAX
    }
}
