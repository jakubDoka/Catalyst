#![feature(result_into_ok_or_err)]
#![feature(let_else)]
#![feature(let_chains)]
#![feature(result_option_inspect)]

pub mod range_joiner;
pub mod range_splitter;
pub mod ranges;

use std::{
    collections::{hash_map::DefaultHasher, HashMap, VecDeque},
    fmt::Debug,
    hash::{Hash, Hasher},
};

pub use range_joiner::RangeJoiner;
pub use range_splitter::RangeSplitter;
pub use ranges::{PatternRange, RangeSerde};

use storage::*;

pub struct PatternGraph<E: EntityRef + Default + ReservedValue, K: Default + Clone> {
    column_ranges: Vec<PatternRange>,
    nodes: PrimaryMap<PatternNode, PatternNodeEnt<E>>,
    slices: StackMap<PatternNodeList, PatternNode>,
    branches: StackMap<Branch, PatternLevelData<E, K>>,
    meta_data: SecondaryMap<E, PatternMetaData<K>>,
    redirects: ListPool<E>,
}

impl<E: EntityRef + Default + ReservedValue, K: Default + Clone> PatternGraph<E, K> {
    const ROOT_NODE: PatternNode = PatternNode(0);

    pub fn new() -> Self {
        Self {
            column_ranges: Vec::new(),
            nodes: PrimaryMap::new(),
            slices: StackMap::new(),
            branches: StackMap::new(),
            meta_data: SecondaryMap::new(),
            redirects: ListPool::new(),
        }
    }

    pub fn clear(&mut self) {
        self.column_ranges.clear();
        self.nodes.clear();
        self.slices.clear();
        self.branches.clear();
        self.meta_data.clear();
        self.redirects.clear();
    }

    pub fn root(&self) -> PatternNode {
        Self::ROOT_NODE
    }

    pub fn node(&self, node: PatternNode) -> PatternNodeEnt<E> {
        self.nodes[node]
    }

    pub fn slice(&self, slice: PatternNodeList) -> &[PatternNode] {
        self.slices.get(slice)
    }

    pub fn meta_data_of(&self, entity: E) -> &PatternMetaData<K> {
        &self.meta_data[entity]
    }

    pub fn add_branch(
        &mut self,
        branch_nodes: impl IntoIterator<Item = PatternLevelData<E, K>>,
    ) -> Branch {
        self.branches.push_iter(branch_nodes)
    }

    pub fn build_graph(&mut self) -> Result<(), PatternError> {
        self.normalize_branches();

        let mut roots = VecDeque::new();
        roots.push_back(Self::ROOT_NODE);
        let mut temp = vec![];
        let mut lookup = HashMap::new();
        let mut exhaustive = true;
        let mut range_splitter = RangeSplitter::new();

        for &column in self.column_ranges.iter() {
            while let Some(root) = roots.pop_front() {
                let root_ent = self.nodes[root];

                range_splitter.split(
                    column,
                    self.slices
                        .get(root_ent.children)
                        .iter()
                        .map(|&child| self.nodes[child].coverage),
                );

                lookup.clear();
                let check_point = temp.len();
                for i in 0..self.slices.len_of(root_ent.children) {
                    let child = self.slices.get(root_ent.children)[i];
                    let child_ent = self.nodes[child];
                    for segment in range_splitter.segments_of(child_ent.coverage) {
                        if let Some(&node) = lookup.get(&segment) {
                            self.nodes[node].children = self
                                .slices
                                .join(self.nodes[node].children, child_ent.children);
                            continue;
                        }

                        let ent = PatternNodeEnt {
                            coverage: segment,
                            ..child_ent
                        };
                        let node = self.nodes.push(ent);
                        lookup.insert(segment, node);
                        temp.push(node);
                    }
                }

                let temp = &mut temp[check_point..];

                let Some(&last) = temp.last() else {
                    continue;
                };

                for meta in temp.iter().map(|&node| self.nodes[node].meta) {
                    self.meta_data[meta]
                        .reachability
                        .upgrade(Reachability::Reachable);
                }

                self.meta_data[self.nodes[last].meta]
                    .reachability
                    .upgrade(Reachability::ReachableUnchecked);

                let new_children = self.slices.push(&temp);
                self.nodes[root].children = new_children;

                temp.sort_unstable_by_key(|&node| self.nodes[node].coverage.end);
                exhaustive &=
                    is_exhaustive(column, temp.iter().map(|&node| self.nodes[node].coverage));
            }

            roots.extend(temp.drain(..));
        }

        if !exhaustive {
            self.log();
            return Err(self.construct_error_report(Self::ROOT_NODE, 0));
        }

        let mut temp = vec![];
        self.eliminate_duplicates(Self::ROOT_NODE, &mut temp);

        Ok(())
    }

    fn construct_error_report(&self, root: PatternNode, depth: usize) -> PatternError {
        let missing = if let Some(&range) = self.column_ranges.get(depth) {
            collect_missing(
                range,
                self.slice(self.nodes[root].children)
                    .iter()
                    .map(|&node| self.nodes[node].coverage),
            )
        } else {
            vec![]
        };

        let mut children = Vec::with_capacity(self.slices.len_of(self.nodes[root].children));
        for &child in self.slice(self.nodes[root].children) {
            children.push(self.construct_error_report(child, depth + 1));
        }

        PatternError { missing, children }
    }

    fn eliminate_duplicates(&mut self, root: PatternNode, stack: &mut Vec<PatternNode>) -> u64 {
        let node = self.nodes[root];

        let Some(&first) = self.slices.get(node.children).first() else {
            return node.meta.index() as u64;
        };

        let check_point = stack.len();
        let mut hasher = DefaultHasher::new();
        let mut prev_hash = self.eliminate_duplicates(first, stack);
        let mut prev = first;
        prev_hash.hash(&mut hasher);

        stack.push(first);
        for i in 1..self.slices.len_of(node.children) {
            let child = self.slices.get(node.children)[i];
            let next_hash = self.eliminate_duplicates(child, stack);
            if prev_hash == next_hash {
                self.nodes[prev].coverage.end = self.nodes[child].coverage.end;
                continue;
            }

            stack.push(child);
            next_hash.hash(&mut hasher);
            prev_hash = next_hash;
            prev = child;
        }

        self.nodes[root].children = self.slices.push_iter(stack.drain(check_point..));

        for &child in self.slices.get(self.nodes[root].children) {
            self.nodes[child].coverage.hash(&mut hasher);
        }

        hasher.finish()
    }

    fn normalize_branches(&mut self) {
        let mut temp = vec![];
        let mut progress = vec![0; self.branches.len()];

        loop {
            let max = self
                .branches
                .values()
                .enumerate()
                .filter_map(|(i, value)| value.get(progress[i]))
                .max_by_key(|value| value.depth)
                .unwrap();

            self.column_ranges.push(max.range);

            for (i, branch) in self.branches.values().enumerate() {
                let Some(value) = branch.get(progress[i]) else {
                    temp.push((max.range, branch.last().unwrap().meta));
                    continue;
                };
                if value.depth == max.depth {
                    temp.push((value.coverage, value.meta));
                    progress[i] += 1;
                } else {
                    temp.push((max.range, value.meta));
                }
            }

            // check if all progresses are at the end
            let should_stop = self
                .branches
                .values()
                .enumerate()
                .all(|(i, value)| value.len() == progress[i]);

            if should_stop {
                break;
            }
        }

        let root_children = self
            .slices
            .alloc(self.branches.len(), PatternNode::reserved_value());
        let root_node = self.nodes.push(PatternNodeEnt {
            children: root_children,
            ..PatternNodeEnt::default()
        });
        assert!(root_node == Self::ROOT_NODE);

        // Materialize branches to graph.
        for i in 0..self.branches.len() {
            let mut root = PatternNode::reserved_value();
            for (coverage, meta) in temp
                .iter()
                .skip(i)
                .step_by(self.branches.len())
                .copied()
                .rev()
            {
                let children = if root.is_reserved_value() {
                    PatternNodeList::reserved_value()
                } else {
                    self.slices.push(&[root])
                };
                let node = self.nodes.push(PatternNodeEnt {
                    coverage,
                    children,
                    meta,
                });
                root = node;
            }
            self.slices.get_mut(root_children)[i] = root;
        }
    }

    pub fn log(&self) {
        self.log_recursive(Self::ROOT_NODE, 0);
    }

    pub fn log_recursive(&self, node: PatternNode, depth: usize) {
        let ent = self.nodes[node];

        let meta = self.meta_data[ent.meta].reachability;

        println!(
            "{} {:?} {:?} {:?}",
            " ".repeat(depth),
            ent.coverage,
            meta,
            ent.meta.index()
        );

        for &child in self.slices.get(ent.children) {
            self.log_recursive(child, depth + 1);
        }
    }
}

fn is_exhaustive(
    range: PatternRange,
    ascending_disjoint_sub_ranges: impl IntoIterator<Item = PatternRange>,
) -> bool {
    let mut sub_ranges = ascending_disjoint_sub_ranges.into_iter();

    let Some(first) = sub_ranges.next() else {
        return true;
    };

    if first.start != range.start {
        return false;
    }

    let mut prev = first;
    for range in sub_ranges {
        if prev.end + 1 != range.start {
            return false;
        }
        prev = range;
    }

    prev.end == range.end
}

fn collect_missing(
    range: PatternRange,
    disjoint_sub_ranges: impl IntoIterator<Item = PatternRange>,
) -> Vec<PatternRange> {
    let mut sub_ranges: Vec<_> = disjoint_sub_ranges.into_iter().collect();
    sub_ranges.sort_by_key(|range| range.start);
    // println!("{:?}", sub_ranges);
    let mut sub_ranges = sub_ranges.into_iter();

    let Some(first) = sub_ranges.next() else {
        return vec![];
    };

    let mut missing = vec![];

    if first.start != range.start {
        missing.push(PatternRange {
            start: range.start,
            end: first.start - 1,
        });
    }

    let mut prev = first;
    for range in sub_ranges {
        if prev.end + 1 != range.start {
            missing.push(PatternRange {
                start: prev.end + 1,
                end: range.start - 1,
            });
        }
        prev = range;
    }

    if prev.end != range.end {
        missing.push(PatternRange {
            start: prev.end + 1,
            end: range.end,
        });
    }

    missing
}

#[derive(Clone, Copy, Default)]
pub struct PatternLevelData<E: EntityRef + Default + ReservedValue, K: Default + Clone> {
    pub coverage: PatternRange,
    pub range: PatternRange,
    pub depth: u32,
    pub meta: E,
    pub value: K,
}

#[derive(Clone, Copy, Default)]
pub struct PatternNodeEnt<E: EntityRef + ReservedValue> {
    pub children: PatternNodeList,
    pub coverage: PatternRange,
    pub meta: E,
}

#[derive(Clone, Copy, Default)]
pub struct PatternMetaData<K: Default + Clone> {
    pub reachability: Reachability,
    pub value: K,
}

impl Reachability {
    fn upgrade(&mut self, other: Reachability) {
        *self = other;
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Reachability {
    Unreachable,
    Reachable,
    ReachableUnchecked,
}

impl Default for Reachability {
    fn default() -> Self {
        Reachability::Unreachable
    }
}

#[derive(Debug)]
pub struct PatternError {
    pub missing: Vec<PatternRange>,
    pub children: Vec<PatternError>,
}

gen_entity!(Branch);
gen_entity!(PatternNode);
gen_entity!(PatternNodeList);

#[cfg(test)]
mod test {
    use std::ops::Range;

    use super::Reachability::*;
    use super::*;

    #[test]
    fn base_case() {
        run_test(0..1, [&[(0..1, 0)]], [&[ReachableUnchecked]]);

        run_test(
            0..1,
            [&[(0..0, 0), (0..1, 1)], &[(1..1, 0)]],
            [&[Reachable, ReachableUnchecked], &[ReachableUnchecked]],
        );

        run_test(
            0..1,
            [
                &[(0..0, 0), (0..0, 1)],
                &[(0..0, 0), (0..1, 1)],
                &[(1..1, 0)],
            ],
            [
                &[Reachable, Reachable],
                &[Unreachable, ReachableUnchecked],
                &[ReachableUnchecked],
            ],
        );

        run_test(
            0..1,
            [
                &[(0..0, 0), (0..0, 1)],
                &[(0..1, 0), (1..1, 1)],
                &[(0..1, 0)],
            ],
            [
                &[Reachable, Reachable],
                &[ReachableUnchecked, Reachable],
                &[ReachableUnchecked],
            ],
        );

        run_test(
            0..3,
            [
                &[(0..2, 0), (0..2, 0)],
                &[(2..3, 0), (3..3, 0)],
                &[(2..2, 0), (0..3, 0)],
                &[(0..3, 0), (0..3, 0)],
            ],
            [
                &[Reachable, Reachable],
                &[ReachableUnchecked, Reachable],
                &[Unreachable, Unreachable],
                &[Unreachable, ReachableUnchecked],
            ],
        );

        run_test(
            0..3,
            [
                &[(0..0, 0), (0..3, 1), (0..3, 0)],
                &[(1..2, 0), (0..2, 0)],
                &[(1..2, 0), (0..3, 0)],
                &[(3..3, 0), (0..3, 1), (0..3, 0)],
            ],
            [
                &[Reachable, ReachableUnchecked, ReachableUnchecked],
                &[Reachable, Reachable],
                &[Unreachable, ReachableUnchecked],
                &[ReachableUnchecked, ReachableUnchecked, ReachableUnchecked],
            ],
        );
    }

    #[test]
    fn test_unconditional_distribution() {
        run_test(
            0..3,
            [
                &[(0..3, 0), (0..1, 0)],
                &[(1..3, 0), (0..3, 0)],
                &[(2..2, 0), (3..3, 0)],
                &[(0..3, 0), (0..3, 0)],
            ],
            [
                &[ReachableUnchecked, Reachable],
                &[Unreachable, ReachableUnchecked],
                &[Unreachable, Unreachable],
                &[Unreachable, ReachableUnchecked],
            ],
        );
    }

    gen_entity!(TestEntity);

    fn run_test<const H: usize>(
        range: Range<i32>,
        branches: [&[(Range<i32>, u32)]; H],
        reachability: [&[Reachability]; H],
    ) {
        let mut tree = PatternGraph::new();
        let range = PatternRange::new(range);

        let mut node_counter = 0;

        for branch in branches.clone() {
            tree.add_branch(branch.iter().map(|(coverage, id)| PatternLevelData {
                coverage: PatternRange::new(coverage.clone()),
                range: range.clone(),
                depth: *id,
                meta: {
                    node_counter += 1;
                    TestEntity::new(node_counter - 1)
                },
                value: (),
            }));
        }

        tree.build_graph().unwrap();

        let mut node_counter = 0;

        let mut failed = false;
        for (_i, reach) in reachability.into_iter().enumerate() {
            for (_j, &reach) in reach.iter().enumerate() {
                let id = TestEntity::new(node_counter);
                let got = tree.meta_data[id].reachability;
                if got != reach {
                    print!("{:?} ", got);
                    failed = true;
                } else {
                    print!("OK ");
                }
                node_counter += 1;
            }
            println!();
        }
        println!();

        if failed {}
        tree.log();

        assert!(!failed);
    }
}
