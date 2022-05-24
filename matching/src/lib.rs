#![feature(result_into_ok_or_err)]
#![feature(let_else)]
#![feature(bool_to_option)]
#![feature(let_chains)]

pub mod range_joiner;
pub mod range_splitter;
pub mod ranges;

pub use range_joiner::RangeJoiner;
pub use range_splitter::RangeSplitter;
pub use ranges::{IntRange, AnalyzableRange};

use std::{collections::HashMap, fmt::Debug, vec};

use storage::*;

pub struct PatternGraph {
    ranges: Vec<IntRange>,
    cons: StackMap<NodeList, PatternNode>,
    nodes: PrimaryMap<PatternNode, NodeEnt>,
    root: PatternNode,

    joiner: RangeJoiner,
    splitter: RangeSplitter,
    range_lookup: HashMap<IntRange, PatternNode>,
    temp: Vec<PatternNode>,
    branch_counter: usize,
}

impl PatternGraph {
    pub fn new() -> Self {
        Self {
            ranges: Vec::new(),
            cons: StackMap::new(),
            nodes: PrimaryMap::new(),
            root: PatternNode::reserved_value(),

            joiner: RangeJoiner::new(),
            splitter: RangeSplitter::new(),
            range_lookup: HashMap::new(),
            temp: Vec::new(),
            branch_counter: 0,
        }
    }

    pub fn get_reachability<'a>(
        &'a mut self,
        branch: usize,
        nodes: impl IntoIterator<Item = IntRange> + 'a,
    ) -> impl Iterator<Item = (Reachability, PatternNode)> + 'a {
        let mut current = self.root;
        nodes
            .into_iter()
            .map(move |range| {
                let children = self.nodes[current].children;   
                let children = self.cons.get(children);

                let child = children.binary_search_by_key(
                    &range.end, 
                    |&child| self.nodes[child].coverage.end,
                ).unwrap();

                current = children[child];

                if self.nodes[current].branch as usize != branch {
                    (Reachability::Unreachable, current)
                } else if child == children.len() - 1 {
                    (Reachability::ReachableUnchecked, current)
                } else {
                    (Reachability::Reachable, current)
                }
            })
    }

    pub fn solve(&mut self) -> Result<(), MissingPatterns> {
        self.split_ranges(self.root)
    }

    pub fn split_ranges(&mut self, node: PatternNode) -> Result<(), MissingPatterns> {
        let node_ent = self.nodes[node];

        self.range_lookup.clear();
        self.temp.clear();
        self.splitter.split(
            self.ranges[node_ent.depth as usize],
            self.cons
                .get(node_ent.children)
                .iter()
                .map(|&child| self.nodes[child].coverage),
        );

        for i in 0..self.cons.len(node_ent.children) {
            let child = self.cons.get(node_ent.children)[i];
            let child_ent = self.nodes[child];
            for segment in self.splitter.segments_of(child_ent.coverage) {
                if let Some(&node) = self.range_lookup.get(&segment) {
                    self.nodes[node].children = self
                        .cons
                        .join(self.nodes[node].children, self.nodes[child].children);
                    continue;
                }

                let ent = NodeEnt {
                    coverage: segment,
                    ..child_ent
                };
                let node = self.nodes.push(ent);
                self.range_lookup.insert(segment, node);
                self.temp.push(node);
            }
        }

        self.nodes[node].children = self.cons.push(&self.temp);

        let mut children = vec![];

        for i in 0..self.temp.len() {
            let child = self.cons.get(self.nodes[node].children)[i];
            if let Err(missing) = self.split_ranges(child) {
                children.resize(i, Default::default());
                children.push(missing);
            }
        }

        self.joiner
            .prepare_for(self.ranges[node_ent.depth as usize]);

        for &child in self.cons.get(self.nodes[node].children).iter() {
            self.joiner.exhaust(self.nodes[child].coverage);
        }

        let missing = if self.cons.len(self.nodes[node].children) == 0 {
            vec![]
        } else {
            self.joiner.missing()
        };

        if children.is_empty() && missing.is_empty() {
            return Ok(());
        }

        Err(MissingPatterns { missing, children })
    }

    pub fn set_ranges(&mut self, ranges: impl IntoIterator<Item = IntRange>) {
        assert!(self.ranges.is_empty());
        self.ranges.extend(ranges.into_iter());
        self.root = self.nodes.push(NodeEnt::default());
    }

    pub fn set_branch_count(&mut self, branch_count: usize) {
        self.nodes.reserve(branch_count + self.ranges.len());
        self.nodes[self.root].children = self.cons.alloc(branch_count, PatternNode::reserved_value());
    }

    pub fn add_branch(&mut self, nodes: impl IntoIterator<Item = IntRange>) {
        assert!(!self.ranges.is_empty());
        let branch = self.branch_counter as u32;
        let mut current = self.root;
        let mut depth = 0;
        for coverage in nodes.into_iter() {
            let ent = NodeEnt {
                coverage,
                depth,
                branch,
                ..Default::default()
            };
            let node = self.nodes.push(ent);
            if current == self.root {
                self.cons.get_mut(self.nodes[current].children)[branch as usize] = node;
            } else {
                self.nodes[current].children = self.cons.push(&[node]);
            }
            current = node;
            depth += 1;
        }
        assert_eq!(self.ranges.len(), depth as usize);
        self.branch_counter += 1;
    }

    pub fn clear(&mut self) {
        self.ranges.clear();
        self.cons.clear();
        self.nodes.clear();
        self.root = PatternNode::reserved_value();
        self.branch_counter = 0;
    }
}

#[derive(Clone, Copy, Default)]
struct NodeEnt {
    coverage: IntRange,
    depth: u32,
    branch: u32,
    children: NodeList,
}

gen_entity!(NodeList);
gen_entity!(PatternNode);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Reachability {
    Reachable,
    ReachableUnchecked,
    Unreachable,
}

#[derive(Default, Clone, Debug)]
pub struct MissingPatterns {
    pub missing: Vec<IntRange>,
    pub children: Vec<MissingPatterns>,
}

#[cfg(test)]
mod test {
    use std::{ops::RangeInclusive, iter::repeat};

    use super::{*, Reachability::*};

    #[test]
    fn base_case() {
        run_test(
            0..=255,
            [
                [0..=20, 0..=20],
                [0..=50, 0..=50],
                [0..=255, 0..=255],
                [30..=255, 30..=255],
            ],
            [
                [Reachable, Reachable],
                [Reachable, Reachable],
                [ReachableUnchecked, ReachableUnchecked],
                [Unreachable, Unreachable],
            ]
        );
    }

    fn run_test<const W: usize, const H: usize>(
        range: RangeInclusive<i32>, 
        branches: [[RangeInclusive<i32>; W]; H],
        reachability: [[Reachability; W]; H],
    ) {
        let mut tree = PatternGraph::new();
        let range = IntRange::new_bounds(&range);

        tree.set_ranges(repeat(range).take(W));
        tree.set_branch_count(branches.len());

        for branch in branches.clone() {
            tree.add_branch(branch.iter().map(|coverage| range.coverage(&coverage)));
        }

        tree.solve().unwrap();

        for (i, (reach, branch)) in reachability.into_iter().zip(branches).enumerate() {
            let iter = tree.get_reachability(i, branch.iter().map(|coverage| range.coverage(&coverage)));
            for (j, (a, b)) in iter.zip(reach).enumerate() {
                assert_eq!(a.0, b, "branch {} at {}", i, j);
            }
        }
    } 
}

/*
pub struct PatternGraph {
    nodes: PrimaryMap<PatternNode, PatternNodeEnt>,
    slices: ListPool<PatternNode>,
    usefulness_slices: ListPool<Usefulness>,
    existing: HashMap<(u32, IntRange), PatternNode>,
    splitter: RangeSplitter,
    joiner: RangeJoiner,
    temp: Vec<PatternNode>,
    usefulness: SecondaryMap<Usefulness, UsefulnessEnt>,
}

macro_rules! mark_usefulness {
    ($self:expr, $root:expr) => {
        $self.joiner.prepare_for($self.nodes[$root].range);
        for &child in $self.nodes[$root].children.as_slice(&$self.slices) {
            let child_ent = $self.nodes[child];
            let usefulness = if $self.joiner.exhaust(child_ent.coverage) {
                if $self.joiner.exhausted() {
                    UsefulnessEnt::LastUseful
                } else {
                    UsefulnessEnt::Useful
                }
            } else {
                UsefulnessEnt::Useless
            };
            for &data_bind in child_ent.data_binds.as_slice(&$self.usefulness_slices) {
                if $self.usefulness[data_bind] == UsefulnessEnt::ObviouslyUseless {
                    continue;
                }
                $self.usefulness[data_bind] = usefulness;
            }
        }
    };
}

impl PatternGraph {
    pub fn new() -> Self {
        Self {
            nodes: PrimaryMap::new(),
            slices: ListPool::new(),
            usefulness_slices: ListPool::new(),
            existing: HashMap::new(),
            splitter: RangeSplitter::new(),
            joiner: RangeJoiner::new(),
            temp: Vec::new(),
            usefulness: SecondaryMap::new(),
        }
    }

    pub fn solve(&mut self, root: PatternNode) -> Result<(), ErrorGraph> {
        self.existing.clear();

        if let Some(error) = self.solve_recurse(root) {
            return Err(error);
        }

        mark_usefulness!(self, root);

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
                    for i in 0..child_ent.children.len(&self.slices) {
                        let child = child_ent.children.get(i, &self.slices).unwrap();
                        self.nodes[node].children.push(child, &mut self.slices);
                    }
                    for i in 0..child_ent.data_binds.len(&self.usefulness_slices) {
                        let child = child_ent.data_binds.get(i, &self.usefulness_slices).unwrap();
                        self.nodes[node].data_binds.push(child, &mut self.usefulness_slices);
                    }
                    continue;
                }
                let new_child_ent = PatternNodeEnt {
                    coverage: sub_coverage,
                    // this is tme most important of all
                    children: child_ent.children.deep_clone(&mut self.slices),
                    data_binds: child_ent.data_binds.deep_clone(&mut self.usefulness_slices),
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

            mark_usefulness!(self, child);

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
            Usefulness::reserved_value(),
            IntRange::new(0..=0, 0),
            range,
        )
    }

    pub fn push(
        &mut self,
        parent: PatternNode,
        id: impl EntityRef,
        coverage: IntRange,
        range: IntRange,
        end: bool,
    ) -> PatternNode {
        let id = Usefulness::new(id.index());
        if let Some(&node) = self.nodes[parent].children.as_slice(&self.slices).last()
            && self.nodes[node].coverage == coverage {

            self.nodes[node].data_binds.push(id, &mut self.usefulness_slices);
            if end {
                self.usefulness[id] = UsefulnessEnt::ObviouslyUseless;
            }
            return node;
        }

        let node = self.create_node(id, coverage, range);
        self.existing.insert((parent.as_u32(), coverage), node);
        self.nodes[parent].children.push(node, &mut self.slices);
        node
    }

    fn create_node(
        &mut self,
        data_bind: Usefulness,
        coverage: IntRange,
        range: IntRange,
    ) -> PatternNode {
        self.nodes.push(PatternNodeEnt {
            coverage,
            range,
            data_binds: EntityList::from_slice(
                &[data_bind],
                &mut self.usefulness_slices,
            ),
            ..Default::default()
        })
    }

    pub fn clear(&mut self) {
        self.nodes.clear();
        self.slices.clear();
        self.usefulness_slices.clear();
        self.existing.clear();
        self.temp.clear();
        self.usefulness.clear();
    }

    pub fn log<T: AnalyzableRange + Debug>(&self, root: PatternNode, offset: usize) {
        let root_ent = self.nodes[root];
        println!(
            "{}{}{:?} data_bind [{:?}]",
            "  ".repeat(offset),
            root,
            root_ent.coverage.decode::<T>(),
            root_ent.data_binds.as_slice(&self.usefulness_slices),
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
}

#[derive(Debug, Default, Clone)]
pub struct ErrorGraph {
    pub missing: Vec<IntRange>,
    pub children: Vec<ErrorGraph>,
}

#[derive(Clone, Copy, Default, Debug)]
struct PatternNodeEnt {
    coverage: IntRange,
    range: IntRange,
    data_binds: EntityList<Usefulness>,
    children: EntityList<PatternNode>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UsefulnessEnt {
    Useful,
    LastUseful,
    Useless,
    ObviouslyUseless,
    None,
}

impl Default for UsefulnessEnt {
    fn default() -> Self {
        Self::None
    }
}

gen_entity!(PatternNode);
gen_entity!(Usefulness);

#[cfg(test)]
mod test {
    use std::ops::RangeInclusive;

    use storage::EntityRef;

    use crate::{IntRange, UsefulnessEnt::{*, self}, Usefulness};

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
            &[
                &[Useful, Useful],
                &[Useful, Useful],
                &[Useful, LastUseful],
                &[Useful, Useful],
                &[LastUseful, Useful],
                &[LastUseful, LastUseful],
            ],
        );
    }

    #[test]
    fn one_exhaust() {
        run_test(0..=255, &[&[0..=255, 0..=255]], &[&[LastUseful, LastUseful]]);
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
            &[
                &[LastUseful, LastUseful, Useful],
                &[LastUseful, Useful, LastUseful],
                &[LastUseful, LastUseful, LastUseful],
            ],
        );
    }

    #[test]
    fn test_useless() {
        run_test(
            0..=255,
            &[
                &[0..=255, 0..=255, 0..=255],
                &[0..=255, 0..=1, 2..=255],
            ],
            &[
                &[LastUseful, LastUseful, LastUseful],
                &[LastUseful, Useful, Useless],
            ],
        );

        run_test(
            0..=255,
            &[
                &[0..=255, 0..=255, 0..=255],
                &[0..=255, 0..=255, 0..=255],
            ],
            &[
                &[LastUseful, LastUseful, LastUseful],
                &[LastUseful, LastUseful, ObviouslyUseless],
            ],
        );

        run_test(
            0..=255,
            &[
                &[0..=4, 10..=11],
                &[2..=6, 10..=11],
                &[0..=6, 10..=11],
                &[0..=255, 0..=255],
            ],
            &[
                &[Useful, Useful],
                &[Useful, Useful],
                &[Useful, Useless],
                &[LastUseful, LastUseful],
            ],
        );


    }

    fn run_test(range: RangeInclusive<i32>, dataset: &[&[RangeInclusive<i32>]], resulting_usefulness: &[&[UsefulnessEnt]]) {
        let mut graph = PatternGraph::new();
        let range = IntRange::new_bounds(&range);
        let root = graph.create_root(range);
        for (i, &ranges) in dataset.iter().enumerate() {
            let mut parent = root;
            for (j, coverage) in ranges.iter().enumerate() {
                let child = graph.push(
                    parent,
                    Usefulness::new(i * ranges.len() + j),
                    range.coverage(coverage),
                    range,
                    j == ranges.len() - 1,
                );
                parent = child;
            }
        }

        // graph.log::<i32>(root, 0);

        graph.solve(root).unwrap();

        // graph.log::<i32>(root, 0);

        for (i, &ranges) in dataset.iter().enumerate() {
            for j in 0..ranges.len() {
                let usefulness = Usefulness::new(i * ranges.len() + j);
                let ent = graph.usefulness[usefulness];
                assert_eq!(resulting_usefulness[i][j], ent, "i={}, j={}", i, j);
            }
        }
    }
}
*/
