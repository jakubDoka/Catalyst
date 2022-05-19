/*
    Option<Option<(i32, i64)>>

    0..=1 | 0..=1 | -1 << 31..=1 << 31 | -1 << 63..=1 << 63

    Some(None        )
    None
    Some(Some((1, 1)))
    Some(Some((_, 0)))
    Some(Some((_, _)))
    Some(Some((3, 3)))
*/

fn goo() {
    match (1u8, 1u8) {
        (0..=50, 40..=255) |
        (51..=255, 0..=39) |
        (0..=50, 0..=39) | 
        (51..=255, 40..=255) => {}
    }
}

use std::ops::{RangeBounds, Bound};

use storage::*;

pub type ExhaustInt = i128; 

fn range_is_empty(range: &impl RangeBounds<i128>) -> bool {
    match (range.start_bound(), range.end_bound()) {
        (Bound::Included(a), Bound::Excluded(b)) => a == b,
        _ => false,
    }
}

pub struct ExhaustMap {
    map: StackMap<PatternNodeList, PatternNodeEnt, PatternNode>,
    pool: Vec<Exhaust>,
    root: Option<PatternNode>,
    stack: Vec<PatternNode>,
}

impl ExhaustMap {
    pub fn new() -> Self {
        ExhaustMap {
            map: StackMap::new(),
            pool: Vec::new(),
            root: None,
            stack: Vec::new(),
        }
    }

    pub fn clear(&mut self) {
        self.pool.extend(self.map.drain().map(|(_, node)| node.exhaust));
        self.root.take();
    }

    pub fn children(&self, node: PatternNode) -> impl Iterator<Item = PatternNode> {
        let children = self.map[node].children;
        self.map.slice_keys(children)
    }

    pub fn declare_root(
        &mut self,
        node_length: usize,
        full_range: impl RangeBounds<ExhaustInt>,
    ) -> PatternNode {
        self.clear();

        let node = self.map.push_one(PatternNodeEnt {
            id: ID(0),
            not_exhausted: node_length + !range_is_empty(&full_range) as usize,
            exhaust: self.pool.pop().unwrap_or_default(),
            ..Default::default()
        });
        let children = self.create_children(node, node_length);
        self.map[node].exhaust.prepare_for(full_range);
        self.map[node].children = children;
        self.root = Some(node);
        node
    }

    fn create_children(&mut self, node: PatternNode, amount: usize) -> PatternNodeList {
        self.map.alloc(amount, PatternNodeEnt { 
            parent: Some(node),
            not_exhausted: usize::MAX,
            ..Default::default()
        })
    }

    pub fn exhaust(
        &mut self, 
        node: PatternNode, 
        id: ID,
        node_length: usize,
        full_range: impl RangeBounds<ExhaustInt>,
        exhausted_range: impl RangeBounds<ExhaustInt>,
    ) -> bool {
        let ent = &self.map[node];
        
        if ent.not_exhausted == 0 {
            return true;
        }
        
        if ent.children.is_reserved_value() && ent.not_exhausted == usize::MAX {
            let children = self.create_children(node, node_length);
            let ent = &mut self.map[node];
            
            ent.id = id;
            ent.not_exhausted = node_length + !range_is_empty(&full_range) as usize;
            ent.children = children;
            
            ent.exhaust.prepare_for(full_range);
        } else {
            assert_eq!(ent.id, id);
        }

        let ent = &mut self.map[node];
        ent.exhaust.exhaust(exhausted_range);
        let exhausted = ent.exhaust.exhausted();

        if exhausted {
            ent.not_exhausted -= 1;
            let mut current = ent.parent;
            while let Some(parent) = current {
                let parent = &mut self.map[parent];
                parent.not_exhausted -= 1;
                if parent.not_exhausted != 0 {
                    break;
                }
                current = parent.parent;
            }
        }

        exhausted
    }

    pub fn missing(&mut self) -> Vec<PatternExhaustError> {
        let Some(root) = self.root else {
            return Vec::new();
        };

        let mut errors = Vec::new();

        self.stack.push(root);

        while let Some(node) = self.stack.pop() {
            let ent = &self.map[node];
            
            if ent.not_exhausted == 0 {
                continue;
            }

            self.stack.extend(self.map.slice_keys(ent.children));

            if ent.exhaust.exhausted() {
                continue;
            }

            errors.push(PatternExhaustError {
                missing: ent.exhaust.missing(),
                ids: self.rebuild_ids(node),
            });
        }

        errors
    }

    pub fn rebuild_ids(&mut self, node: PatternNode) -> Vec<ID> {
        let mut ids = Vec::new();

        let mut current = Some(node);
        while let Some(node) = current {
            ids.push(self.map[node].id);
            current = self.map[node].parent;
        }

        ids
    }
}

#[derive(Debug)]
pub struct PatternExhaustError {
    pub ids: Vec<ID>, 
    pub missing: Vec<(ExhaustInt, ExhaustInt)>,
}

#[derive(Clone, Default)]
struct PatternNodeEnt {
    id: ID,
    children: PatternNodeList,
    exhaust: Exhaust,
    not_exhausted: usize,
    parent: Option<PatternNode>,
}

gen_entity!(PatternNode);
gen_entity!(PatternNodeList);

#[derive(Default, Clone)]
pub struct Exhaust {
    start: ExhaustInt,
    end: ExhaustInt,
    indices: Vec<(ExhaustInt, ExhaustInt)>,
}

impl Exhaust {
    pub fn new() -> Self {
        Self {
            start: 0,
            end: 0,
            indices: Vec::new(),
        }
    }

    pub fn conv_range(&self, range: impl RangeBounds<ExhaustInt>) -> (ExhaustInt, ExhaustInt) {
        let start = match range.start_bound() {
            Bound::Included(start) => *start,
            Bound::Excluded(start) => *start + 1,
            Bound::Unbounded => self.start,
        };

        let end = match range.end_bound() {
            Bound::Included(end) => *end,
            Bound::Excluded(end) => *end - 1,
            Bound::Unbounded => self.end,
        };

        (start, end)
    }

    pub fn prepare_for(&mut self, range: impl RangeBounds<ExhaustInt>) {
        let range = self.conv_range(range);
        self.prepare_for_low(range);
    }

    pub fn prepare_for_low(&mut self, (start, end): (ExhaustInt, ExhaustInt)) {
        self.indices.clear();
        self.start = start;
        self.end = end;
    }

    pub fn exhaust_one(&mut self, index: ExhaustInt) {
        self.exhaust(index..=index);
    }

    pub fn exhaust(&mut self, range: impl RangeBounds<ExhaustInt>) {
        let range = self.conv_range(range);

        self.exhaust_low(range);
    }

    pub fn exhaust_low(&mut self, (start, end): (ExhaustInt, ExhaustInt)) {
        assert!(start <= end);
        assert!(start >= self.start);
        assert!(end <= self.end);

        if self.exhausted() {
            return;
        }

        let start_space = self.indices.binary_search_by(|&(a, _)| a.cmp(&start))
            .into_ok_or_err();
        let end_space = self.indices.binary_search_by(|&(_, b)| b.cmp(&end))
            .into_ok_or_err();

        if start_space > end_space {
            return;
        }

        self.indices.drain(start_space..end_space);

        let (l, r) = self.indices.split_at_mut(start_space);

        match (l.last_mut(), r.first_mut()) {
            (None, None) => {
                self.indices.push((start, end));
            }
            (None, Some((left, _))) => {
                if *left <= end + 1 {
                    *left = start;
                } else {
                    self.indices.insert(0, (start, end));
                }
            },
            (Some((_, right)), None) => {
                if *right >= start - 1 {
                    *right = end;
                } else {
                    self.indices.push((start, end));
                }
            },
            (Some((_, right)), Some((left, right_r))) => {
                if *left <= end + 1 && *right >= start - 1 {
                    *right = *right_r;
                    self.indices.remove(start_space);
                } else if *left <= end + 1 {
                    *left = start;
                } else if *right >= start - 1 {
                    *right = end;
                } else {
                    self.indices.insert(start_space, (start, end));
                }
            },
        }
    }

    pub fn exhausted(&self) -> bool {
        (self.indices.len() == 1 && self.indices[0] == (self.start, self.end))
            || self.start == self.end 
    }

    pub fn missing(&self) -> Vec<(ExhaustInt, ExhaustInt)> {
        let mut missing = Vec::new();

        if self.indices.is_empty() {
            missing.push((self.start, self.end));
            return missing;
        }

        let edge = self.indices.first().unwrap().0;
        if self.start != edge {
            missing.push((self.start, edge - 1));
        }

        for w in self.indices.windows(2) {
            missing.push((w[0].1 + 1, w[1].0 - 1));
        }
        
        let edge = self.indices.last().unwrap().1; 
        if self.end != edge {
            missing.push((edge + 1, self.end));
        }

        missing
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn exhaust_full() {
        let mut exhaust = Exhaust::new();
        exhaust.prepare_for(0..=10);
        exhaust.exhaust(0..=10);
        assert!(exhaust.exhausted());
    }

    #[test]
    fn exhaust_overlap() {
        let mut exhaust = Exhaust::new();
        exhaust.prepare_for(0..=10);
        exhaust.exhaust(0..=7);
        exhaust.exhaust(5..=10);
        assert!(exhaust.exhausted());
    }

    #[test]
    fn exhaust_one() {
        let mut exhaust = Exhaust::new();
        exhaust.prepare_for(0..=10);
        for i in 0..=10 {
            exhaust.exhaust_one(i as ExhaustInt);
        }
        assert!(exhaust.exhausted(), "{:?}", exhaust.indices);
    }

    #[test]
    fn exhaust_incremental() {
        let mut exhaust = Exhaust::new();
        exhaust.prepare_for(0..=10);
        for i in 0..=10 {
            exhaust.exhaust(0..=i as ExhaustInt);
        }
        assert!(exhaust.exhausted());

        exhaust.prepare_for(0..=10);
        for i in (0..=10).rev() {
            exhaust.exhaust(i as ExhaustInt..=10);
        }
        assert!(exhaust.exhausted());

        exhaust.prepare_for(0..=10);
        for (i, j) in (0..=5).rev().zip(5..=10) {
            exhaust.exhaust(i as ExhaustInt..=j as ExhaustInt);
        }
        assert!(exhaust.exhausted());

        exhaust.prepare_for(0..=10);
        for (i, j) in (0..=5).zip((5..=10).rev()) {
            exhaust.exhaust_one(i as ExhaustInt);
            exhaust.exhaust_one(j as ExhaustInt);
        }
        assert!(exhaust.exhausted(), "{:?}", exhaust.indices);
    }

    #[test]
    fn exhaust_zigzag() {
        let mut exhaust = Exhaust::new();
        exhaust.prepare_for(0..=10);
        for i in (1..=9).step_by(2) {
            exhaust.exhaust_one(i as ExhaustInt);
        }
        for i in (0..=10).step_by(2) {
            exhaust.exhaust_one(i as ExhaustInt);
        }
        assert!(exhaust.exhausted());
    }

    #[test]
    fn fuzz_exhaust() {
        let mut exhaust = Exhaust::new();
        exhaust.prepare_for(-100..=100);
        let mut i = 1;
        for _ in 0..1000 {
            exhaust.exhaust_one(i);
            i = i.wrapping_shl(i as u32).wrapping_add(i);
            i = i % 101;
            i = -i;
        }
        for i in -100..=100 {
            exhaust.exhaust_one(i);
        }
        assert!(exhaust.exhausted());
    }
}
