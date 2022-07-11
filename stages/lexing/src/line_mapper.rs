use std::iter::once;

pub struct LineMapping {
    indices: Vec<usize>,
}

impl LineMapping {
    pub fn new(source: &str) -> Self {
        LineMapping { 
            indices: once(0).chain(source
                .match_indices('\n')
                .map(|(i, _)| i))
                .collect(),
        }
    }

    /// Returns line number anc column number of the given index inside mapped file.
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
        let pos = self.indices.binary_search(&index).unwrap_or_else(|i| i);
        let pos = pos.checked_sub(1).unwrap_or(pos);

        (pos + 1, index - self.indices[pos] - 1)        
    }
}

