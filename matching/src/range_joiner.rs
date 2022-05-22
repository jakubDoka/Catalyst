use crate::ranges::IntRange;

#[derive(Clone)]
pub struct RangeJoiner {
    range: IntRange,
    indices: Vec<IntRange>,
}

impl RangeJoiner {
    pub fn new() -> Self {
        Self {
            range: IntRange::new(0..=0, 0),
            indices: Vec::new(),
        }
    }

    pub fn prepare_for(&mut self, range: IntRange) {
        self.indices.clear();
        self.range = range;
    }

    pub fn exhaust(&mut self, range: IntRange) {
        let (start, end) = (range.start, range.end);

        if self.exhausted() {
            return;
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
            return;
        }

        self.indices.drain(start_space..end_space);

        let (l, r) = self.indices.split_at_mut(start_space);

        match (l.last_mut(), r.first_mut()) {
            (None, None) => {
                self.indices.push(range);
            }
            (None, Some(e)) => {
                if e.start <= end + 1 {
                    e.start = start;
                } else {
                    self.indices.insert(0, range);
                }
            }
            (Some(e), None) => {
                if e.end >= start - 1 {
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
                    a.start = start;
                } else if p.end >= start - 1 {
                    p.end = end;
                } else {
                    self.indices.insert(start_space, range);
                }
            }
        }
    }

    pub fn exhausted(&self) -> bool {
        (self.indices.len() == 1 && self.indices[0] == self.range) || self.range.bias == 0
    }

    pub fn missing(&self) -> Vec<IntRange> {
        let mut missing = Vec::new();
        self.missing_to(&mut missing);
        missing
    }

    pub fn missing_to(&self, buffer: &mut Vec<IntRange>) {
        if self.exhausted() {
            return;
        }

        if self.indices.is_empty() {
            buffer.push(self.range);
            return;
        }

        let edge = self.indices.first().unwrap();
        if self.range.start != edge.start {
            buffer.push(IntRange::new(
                self.range.start..=edge.start - 1,
                self.range.bias,
            ));
        }

        buffer.extend(
            self.indices
                .windows(2)
                .map(|w| IntRange::new(w[0].end + 1..=w[1].start - 1, self.range.bias)),
        );

        let edge = self.indices.last().unwrap();
        if self.range.end != edge.end {
            buffer.push(IntRange::new(
                edge.end + 1..=self.range.end,
                self.range.bias,
            ));
        }
    }
}
