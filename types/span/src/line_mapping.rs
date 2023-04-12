use std::{iter::once, ops::Range};

#[derive(Debug)]
pub struct LineMapping {
    indices: Vec<u32>,
}

impl LineMapping {
    pub fn new(source: &str) -> Self {
        LineMapping {
            indices: once(0)
                .chain(source.match_indices('\n').map(|(i, _)| i as u32))
                .collect(),
        }
    }

    /// Returns line number and column number of the given index inside mapped file.
    ///
    /// # Examples
    /// ```
    /// let source = "
    /// alfa
    /// beta gama
    /// delta
    /// epsilon
    /// ";
    ///
    /// let mapper = lexing::LineMapper::new(source);
    ///
    /// let i = |arg: &str| source.find(arg).unwrap();
    ///
    /// assert_eq!(mapper.line_info_at(i("alfa")), (2, 0));
    /// assert_eq!(mapper.line_info_at(i("beta")), (3, 0));
    /// assert_eq!(mapper.line_info_at(i("gama")), (3, 5));
    /// assert_eq!(mapper.line_info_at(i("delta")), (4, 0));
    /// assert_eq!(mapper.line_info_at(i("epsilon")), (5, 0));
    /// ```
    pub fn line_info_at(&self, index: usize) -> (usize, usize) {
        let pos = self
            .indices
            .binary_search(&(index as u32))
            .unwrap_or_else(|i| i);
        let pos = pos.checked_sub(1).unwrap_or(pos);

        (
            pos + 1,
            (index - self.indices[pos] as usize).saturating_sub(1),
        )
    }

    pub fn clear(&mut self) {
        self.indices.clear();
    }

    pub fn width(&self, range: Range<usize>) -> usize {
        let (start_line, start_col) = self.line_info_at(range.start);
        let (end_line, end_col) = self.line_info_at(range.end);

        if start_line == end_line {
            return end_col - start_col;
        }

        (start_line - 1..end_line - 1)
            .map(|i| self.indices[i + 1] - self.indices[i])
            .max()
            .unwrap_or(end_col as u32) as usize
    }
}

impl Default for LineMapping {
    fn default() -> Self {
        LineMapping::new("")
    }
}
