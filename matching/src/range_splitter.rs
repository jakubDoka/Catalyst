use crate::ranges::{Border, PatternRange};

#[derive(Clone)]
pub struct RangeSplitter {
    range: PatternRange,
    indices: Vec<Border>,
}

impl RangeSplitter {
    pub fn new() -> Self {
        Self {
            range: PatternRange::new(0..0),
            indices: Vec::new(),
        }
    }

    pub fn split(
        &mut self,
        range: PatternRange,
        ranges: impl Iterator<Item = PatternRange> + Clone,
    ) {
        self.range = range;
        let (start, end) = self.range.into_borders();

        let ranges = ranges
            .filter_map(|r| r.intersect(&self.range))
            .map(|r| r.into_borders())
            .flat_map(|(start, end)| [start, end].into_iter());

        self.indices.clear();

        self.indices.push(start);
        self.indices.extend(ranges);
        self.indices.push(end);

        self.indices.sort_unstable();
        self.indices.dedup();
    }

    /// Range start ens end must be contained within the RangeSplitter.
    /// This means range mush have been split before.
    pub fn segments_of(&self, range: PatternRange) -> impl Iterator<Item = PatternRange> + '_ {
        let (start, end) = range.into_borders();

        let start_index = self.indices.binary_search(&start).unwrap();
        let end_index = self.indices.binary_search(&end).unwrap();

        self.indices[start_index..=end_index]
            .windows(2)
            .map(|window| PatternRange::from_borders(window[0], window[1]))
    }

    pub fn len(&self) -> usize {
        self.indices.len()
    }
}

#[cfg(test)]
mod test {}
