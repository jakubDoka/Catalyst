#![feature(result_into_ok_or_err)]
#![feature(let_else)]
#![feature(bool_to_option)]
#![feature(let_chains)]

pub mod range_joiner;
pub mod range_splitter;
pub mod ranges;

pub use range_joiner::RangeJoiner;
pub use range_splitter::RangeSplitter;
use ranges::AnalyzableRange;
pub use ranges::IntRange;

use std::{collections::HashMap, fmt::Debug};

use storage::*;

pub struct PatternGraph {
    nodes: PrimaryMap<PatternNode, PatternNodeEnt>,
    slices: ListPool<PatternNode>,
    existing: HashMap<(u32, IntRange), PatternNode>,
    splitter: RangeSplitter,
    joiner: RangeJoiner,
    temp: Vec<PatternNode>,
    branches: PrimaryMap<PatternBranch, PatternBranchEnt>,
}

impl PatternGraph {
    pub fn new() -> Self {
        Self {
            nodes: PrimaryMap::new(),
            slices: ListPool::new(),
            existing: HashMap::new(),
            splitter: RangeSplitter::new(),
            joiner: RangeJoiner::new(),
            temp: Vec::new(),
            branches: PrimaryMap::new(),
        }
    }

    pub fn solve(&mut self, root: PatternNode) -> Result<(), ErrorGraph> {
        self.existing.clear();

        if let Some(error) = self.solve_recurse(root) {
            return Err(error);
        }

        Ok(())
    }

    fn solve_recurse(&mut self, root: PatternNode) -> Option<ErrorGraph> {
        let root_ent = self.nodes[root];

        let mut errors = vec![];
        for i in 0..root_ent.children.len(&self.slices) {
            let child = root_ent.children.get(i, &self.slices).unwrap();
            if let Some(error) = self.solve_recurse(child) {
                errors.resize(errors.len(), Default::default());
                errors.push(error);
            }
        }

        if !errors.is_empty() {
            return Some(ErrorGraph {
                children: errors,
                missing: vec![],
            });
        }

        let iter = root_ent
            .children
            .as_slice(&self.slices)
            .iter()
            .map(|&child| self.nodes[child].coverage);
        self.splitter.split(root_ent.range, iter);

        self.temp.clear();
        for i in 0..root_ent.children.len(&self.slices) {
            let child = root_ent.children.get(i, &self.slices).unwrap();
            let child_ent = self.nodes[child];
            for sub_coverage in self.splitter.segments_of(child_ent.coverage) {
                if let Some(&node) = self.existing.get(&(root.as_u32(), sub_coverage)) {
                    let check_point = self.temp.len();
                    self.temp.extend(child_ent.children.as_slice(&self.slices));
                    for child in self.temp.drain(check_point..) {
                        self.nodes[node].children.push(child, &mut self.slices);
                    }
                    continue;
                }
                let new_child_ent = PatternNodeEnt {
                    coverage: sub_coverage,
                    // this is tme most important of all
                    children: child_ent.children.deep_clone(&mut self.slices),
                    ..child_ent
                };
                let new_child = self.nodes.push(new_child_ent);
                self.existing
                    .insert((root.as_u32(), sub_coverage), new_child);
                self.temp.push(new_child);
            }
        }

        self.nodes[root].children = EntityList::from_slice(&self.temp, &mut self.slices);

        let mut errors = vec![];
        for (i, &child) in self.temp.iter().enumerate() {
            let child_ent = self.nodes[child];
            self.joiner.prepare_for(child_ent.range);

            if child_ent.children.len(&self.slices) == 0 {
                continue;
            }

            for &child in child_ent.children.as_slice(&self.slices) {
                let usefulness = if self.joiner.exhausted() {
                    Usefulness::Useless
                } else {
                    let child_ent = self.nodes[child];
                    self.joiner.exhaust(child_ent.coverage);
                    if self.joiner.exhausted() {
                        Usefulness::LastUseful
                    } else {
                        Usefulness::Useful
                    }
                };
                for &data_bind in child_ent.data_binds.as_slice(&self.slices) {
                    self.branches[child_ent.branch].nodes[data_bind] = usefulness;
                }
            }

            if !self.joiner.exhausted() {
                errors.resize(i, Default::default());
                errors.push(ErrorGraph {
                    missing: self.joiner.missing(),
                    children: vec![],
                });
            }
        }

        if errors.is_empty() {
            return None;
        }

        Some(ErrorGraph {
            children: errors,
            missing: vec![],
        })
    }

    pub fn create_root(&mut self, range: IntRange) -> PatternNode {
        self.clear();
        self.create_node(
            PatternBranch::reserved_value(),
            None,
            IntRange::new(0..=0, 0),
            range,
        )
    }

    pub fn push<I: EntityRef>(
        &mut self,
        parent: PatternNode,
        branch: PatternBranch,
        id: I,
        coverage: IntRange,
        range: IntRange,
        end: bool,
    ) -> PatternNode {
        let id = PatternNode::new(id.index());
        if let Some(&node) = self.existing.get(&(parent.as_u32(), coverage)) {
            self.nodes[node].data_binds.push(id, &mut self.slices);
            if end {
                self.branches[branch].nodes[id] = Usefulness::Useless;
            }
            return node;
        }

        let node = self.create_node(branch, Some(id), coverage, range);
        self.existing.insert((parent.as_u32(), coverage), node);
        self.nodes[parent].children.push(node, &mut self.slices);
        node
    }

    fn create_node(
        &mut self,
        branch: PatternBranch,
        self_id: Option<PatternNode>,
        coverage: IntRange,
        range: IntRange,
    ) -> PatternNode {
        self.nodes.push(PatternNodeEnt {
            coverage,
            range,
            branch,
            data_binds: EntityList::from_slice(
                &[self_id.unwrap_or(self.nodes.next_key())],
                &mut self.slices,
            ),
            ..Default::default()
        })
    }

    pub fn clear(&mut self) {
        self.nodes.clear();
        self.slices.clear();
        self.existing.clear();
        self.temp.clear();
        self.branches.clear();
    }

    pub fn log<T: AnalyzableRange + Debug>(&self, root: PatternNode, offset: usize) {
        let root_ent = self.nodes[root];
        println!(
            "{}{}{:?}",
            "  ".repeat(offset),
            root,
            root_ent.coverage.decode::<T>()
        );
        for i in 0..root_ent.children.len(&self.slices) {
            let child = root_ent.children.get(i, &self.slices).unwrap();
            if child == root {
                println!("{}{}", "  ".repeat(offset + 1), "what?");
                continue;
            }
            self.log::<T>(child, offset + 1);
        }
    }

    pub fn create_branch(&mut self) -> PatternBranch {
        // should probably reuse instead of recreating
        self.branches.push(PatternBranchEnt::new())
    }
}

#[derive(Debug, Default, Clone)]
pub struct ErrorGraph {
    pub missing: Vec<IntRange>,
    pub children: Vec<ErrorGraph>,
}

#[derive(Clone, Copy, Default)]
struct PatternNodeEnt {
    coverage: IntRange,
    range: IntRange,
    branch: PatternBranch,
    data_binds: EntityList<PatternNode>,
    children: EntityList<PatternNode>,
}

#[derive(Clone, Copy)]
pub enum Usefulness {
    Useful,
    LastUseful,
    Useless,
    None,
}

impl Default for Usefulness {
    fn default() -> Self {
        Usefulness::None
    }
}

struct PatternBranchEnt {
    nodes: SecondaryMap<PatternNode, Usefulness>,
}

impl PatternBranchEnt {
    fn new() -> Self {
        PatternBranchEnt {
            nodes: SecondaryMap::new(),
        }
    }
}

gen_entity!(PatternNode);
gen_entity!(PatternBranch);

#[cfg(test)]
mod test {
    use std::ops::RangeInclusive;

    use crate::{IntRange, PatternNode};

    use super::PatternGraph;

    #[test]
    fn base_test_case() {
        run_test(
            0..=255,
            &[
                &[0..=10, 10..=11],
                &[0..=2, 0..=9],
                &[0..=2, 12..=255],
                &[3..=10, 41..=255],
                &[11..=255, 41..=255],
                &[3..=255, 0..=40],
            ],
        );
    }

    #[test]
    fn one_exhaust() {
        run_test(0..=255, &[&[0..=255, 0..=255]]);
    }

    #[test]
    fn long_pattern() {
        run_test(
            0..=255,
            &[
                &[0..=255, 0..=255, 0..=1],
                &[0..=255, 0..=1, 2..=255],
                &[0..=255, 0..=255, 2..=255],
            ],
        );
    }

    fn run_test(range: RangeInclusive<i32>, dataset: &[&[RangeInclusive<i32>]]) {
        let mut graph = PatternGraph::new();
        let range = IntRange::new_bounds(&range);
        let root = graph.create_root(range);
        for &ranges in dataset {
            let mut parent = root;
            let branch = graph.create_branch();
            for (i, coverage) in ranges.iter().enumerate() {
                let child = graph.push(
                    parent,
                    branch,
                    PatternNode(i as u32),
                    range.coverage(coverage),
                    range,
                    i == ranges.len() - 1,
                );
                parent = child;
            }
        }
        graph.solve(root).unwrap();
    }
}
