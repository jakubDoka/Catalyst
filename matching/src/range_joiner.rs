use crate::ranges::PatternRange;

#[derive(Clone)]
pub struct RangeJoiner {
    range: PatternRange,
    indices: Vec<PatternRange>,
}

impl RangeJoiner {
    pub fn new() -> Self {
        Self {
            range: PatternRange::new(0..0),
            indices: Vec::new(),
        }
    }

    pub fn prepare_for(&mut self, range: PatternRange) {
        self.indices.clear();
        self.range = range;
    }

    pub fn add_range(&mut self, range: PatternRange) -> bool {
        let Some(range) = range.intersect(&self.range) else {
            return false;
        };

        let (start, end) = (range.start, range.end);

        if self.exhausted() {
            return false;
        }

        let start_space = self
            .indices
            .binary_search_by(|e| e.start.cmp(&start))
            .into_ok_or_err();
        let end_space = self
            .indices
            .binary_search_by(|e| e.end.cmp(&end))
            .into_ok_or_err();

        if start_space > end_space {
            return false;
        }

        self.indices.drain(start_space..end_space);

        let (l, r) = self.indices.split_at_mut(start_space);

        match (l.last_mut(), r.first_mut()) {
            (None, None) => {
                self.indices.push(range);
            }
            (None, Some(e)) => {
                if e.start <= end + 1 {
                    if e.start == start {
                        return false;
                    }
                    e.start = start;
                } else {
                    self.indices.insert(0, range);
                }
            }
            (Some(e), None) => {
                if e.end >= start - 1 {
                    if e.end == end {
                        return false;
                    }
                    e.end = end;
                } else {
                    self.indices.push(range);
                }
            }
            (Some(p), Some(a)) => {
                if a.start <= end + 1 && p.end >= start - 1 {
                    p.end = a.end;
                    self.indices.remove(start_space);
                } else if a.start <= end + 1 {
                    if a.start == start {
                        return false;
                    }
                    a.start = start;
                } else if p.end >= start - 1 {
                    if a.end == end {
                        return false;
                    }
                    p.end = end;
                } else {
                    self.indices.insert(start_space, range);
                }
            }
        }

        true
    }

    pub fn exhausted(&self) -> bool {
        (self.indices.len() == 1 && self.indices[0] == self.range)
            || self.range.start == self.range.end
    }

    pub fn missing(&self) -> Vec<PatternRange> {
        let mut missing = Vec::new();
        self.missing_to(&mut missing);
        missing
    }

    pub fn missing_to(&self, buffer: &mut Vec<PatternRange>) {
        if self.exhausted() {
            return;
        }

        if self.indices.is_empty() {
            buffer.push(self.range);
            return;
        }

        let edge = self.indices.first().unwrap();
        if self.range.start != edge.start {
            buffer.push(PatternRange::new(self.range.start..edge.start - 1));
        }

        buffer.extend(
            self.indices
                .windows(2)
                .map(|w| PatternRange::new(w[0].end + 1..w[1].start - 1)),
        );

        let edge = self.indices.last().unwrap();
        if self.range.end != edge.end {
            buffer.push(PatternRange::new(edge.end + 1..self.range.end));
        }
    }
}
