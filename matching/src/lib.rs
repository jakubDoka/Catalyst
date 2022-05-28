#![feature(result_into_ok_or_err)]
#![feature(let_else)]
#![feature(bool_to_option)]
#![feature(let_chains)]
#![feature(result_option_inspect)]

pub mod range_joiner;
pub mod range_splitter;
pub mod ranges;

use std::{collections::{HashMap, VecDeque}, fmt::Debug};

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

    pub fn meta_data_of(&self, entity: E) -> &PatternMetaData<K> {
        &self.meta_data[entity]
    }

    pub fn add_branch(&mut self, branch_nodes: impl IntoIterator<Item = PatternLevelData<E, K>>) -> Branch {
        self.branches.push_iter(branch_nodes)
    }

    pub fn build_graph(&mut self) {
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
                    self.slices.get(root_ent.children)
                        .iter()
                        .map(|&child| self.nodes[child].coverage)
                );

                lookup.clear();
                temp.clear();
                for i in 0..self.slices.len_of(root_ent.children) {
                    let child = self.slices.get(root_ent.children)[i];
                    let child_ent = self.nodes[child];
                    for segment in range_splitter.segments_of(child_ent.coverage) {
                        if let Some(&node) = lookup.get(&segment) {
                            self.nodes[node].children = self.slices.join(self.nodes[node].children, child_ent.children);
                            continue;
                        }

                        let ent = PatternNodeEnt {
                            coverage: segment,
                            ..child_ent
                        };
                        let node = self.nodes.push(ent);
                        lookup.insert(segment, node);
                        roots.push_back(node);
                        temp.push(node);
                    }
                }

                let Some(&last) = temp.last() else {
                    continue;
                };

                for meta in temp.iter().filter_map(|&node| self.nodes[node].meta.expand()) {
                    self.meta_data[meta].reachability.upgrade(Reachability::Reachable);
                }

                if let Some(meta) = self.nodes[last].meta.expand() {
                    self.meta_data[meta].reachability.upgrade(Reachability::ReachableUnchecked);
                } 

                let new_children = self.slices.push(&temp);
                self.nodes[root].children = new_children;
                
                temp.sort_unstable_by_key(|&node| self.nodes[node].coverage.end);
                exhaustive &= is_exhaustive(
                    column, 
                    temp
                        .iter()
                        .map(|&node| self.nodes[node].coverage),
                );
            }
        }

        assert!(exhaustive)
    }

    fn normalize_branches(&mut self) {
        let mut temp = vec![];
        let mut progress = vec![0; self.branches.len()];

        loop {
            let max = self.branches
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
            let should_stop = self.branches
                .values()
                .enumerate()
                .all(|(i, value)| value.len() == progress[i]);

            if should_stop {
                break;
            }
        }

        let root_children = self.slices.alloc(self.branches.len(), PatternNode::reserved_value());
        let root_node = self.nodes.push(PatternNodeEnt {
            children: root_children,
            ..PatternNodeEnt::default()
        });
        assert!(root_node == Self::ROOT_NODE);

        // Materialize branches to graph.
        for i in 0..self.branches.len() {
            let mut root = PatternNode::reserved_value();
            for (coverage, meta) in temp.iter().skip(i).step_by(self.branches.len()).copied().rev() {
                let children = if root.is_reserved_value() {
                    PatternNodeList::reserved_value()
                } else {
                    self.slices.push(&[root])
                };
                let node = self.nodes.push(PatternNodeEnt {
                    coverage,
                    children,
                    meta
                });
                root = node;
            }
            self.slices.get_mut(root_children)[i] = root;
        }
    }

    pub fn log(&self) where E: Debug {
        self.log_recursive(Self::ROOT_NODE, 0);
    }

    fn log_recursive(&self, node: PatternNode, depth: usize) where E: Debug {
        let ent = self.nodes[node];
        
        let meta = ent.meta.map(|meta| self.meta_data[meta].reachability);
     
        println!("{} {:?} {:?} {:?}", " ".repeat(depth), ent.coverage.decode::<i32>(), meta, ent.meta);

        for &child in self.slices.get(ent.children) {
            self.log_recursive(child, depth + 1);
        }
    }
}

fn is_exhaustive(range: PatternRange, ascending_disjoint_sub_ranges: impl IntoIterator<Item = PatternRange>) -> bool {
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

#[derive(Clone, Copy, Default)]
pub struct PatternLevelData<E: EntityRef + Default + ReservedValue, K: Default + Clone> {
    pub coverage: PatternRange,
    pub range: PatternRange,
    pub depth: u32,
    pub meta: PackedOption<E>,
    pub value: K,
}

#[derive(Clone, Copy, Default)]
struct PatternNodeEnt<E: EntityRef + ReservedValue> {
    children: PatternNodeList,
    coverage: PatternRange,
    meta: PackedOption<E>,
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

gen_entity!(Branch);
gen_entity!(PatternNode);
gen_entity!(PatternNodeList);

#[cfg(test)]
mod test {
    use std::ops::Range;

    use super::*;
    use super::Reachability::*;

    #[test]
    fn base_case() {
        run_test(
            0..1,
            [
                &[(0..1, 0)],
            ],
            [
                &[ReachableUnchecked],
            ],
        );

        run_test(
            0..1,
            [
                &[(0..0, 0), (0..1, 1)],
                &[(1..1, 0)],
            ],
            [
                &[Reachable, ReachableUnchecked],
                &[ReachableUnchecked],
            ],
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
                    Some(TestEntity::new(node_counter - 1)).into()
                },
                value: (),
            }));
        }

        tree.build_graph();

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
        
        if failed {
        }
        tree.log();

        assert!(!failed);
    } 
}

/*
pub struct PatternGraphBuilder<'a> {
    graph: &'a mut PatternGraph,
}

impl<'a> PatternGraphBuilder<'a> {
    pub fn new(graph: &'a mut PatternGraph) -> Self {
        graph.clear();
        Self { graph }
    }

    pub fn create_node(&mut self, kind: PatternNodeKind) -> PatternNode {
        self.graph.nodes.push(PatternNodeEnt { 
            kind, 
            ..Default::default()
        })
    }

    pub fn create_range(&mut self, range: Range<impl RangeSerde>) -> PatternRange {
        
        self.graph.range_storage.insert(PatternRangeEnt::new(range))
    }

    pub fn create_list(&mut self, slice: &[PatternNode]) -> PatternNodeList {
        self.graph.slices.push(slice)
    }
}

pub struct PatternGraph {
    nodes: PrimaryMap<PatternNode, PatternNodeEnt>,
    slices: StackMap<PatternNodeList, PatternNode>,

    range_storage: RangeStorage,
    range_joiner: RangeJoiner,
    range_splitter: RangeSplitter,
    range_lookup: HashMap<PatternRangeEnt, PatternNode>,
}

impl PatternGraph {
    pub fn new() -> Self {
        Self {
            nodes: PrimaryMap::new(),
            slices: StackMap::new(),

            range_storage: RangeStorage::new(),
            range_joiner: RangeJoiner::new(),
            range_splitter: RangeSplitter::new(),
            range_lookup: HashMap::new(),
        }
    }

    pub fn solve(&mut self, root: PatternNode) {
        let PatternNodeKind::Or(options) = self.nodes[root].kind else {
            panic!("Or node must be an Or node");
        };

        let Some(&first) = self.slices.get(options).first() else {
            panic!("Or must have at least one option");
        };
        
        match self.nodes[first].kind {
            PatternNodeKind::And(root, ..) => {
                let root = Self::root_of(root, &self.nodes);

                let PatternNodeKind::Range(.., range) = self.nodes[root].kind else {
                    unreachable!();
                };

                self.range_splitter.split(
                    self.range_storage[range],
                    self.slices
                        .get(options)
                        .iter()
                        .map(|&node| self.range_storage[Self::coverage_of(node, &self.nodes)]),
                );

                self.range_lookup.clear();
                for i in 0..self.slices.len(options) {
                    let child = self.slices.get(options)[i];
                    let coverage = self.range_storage[Self::coverage_of(child, &self.nodes)];
                    for segment in self.range_splitter.segments_of(coverage) {
                        if let Some(&node) = self.range_lookup.get(&segment) {
                            Self::join(node, child, &mut self.nodes, &mut self.slices);
                            continue;
                        }
                    }
                }
            },

            PatternNodeKind::Or(..) => panic!("Or node cannot directly contain Or nodes"),

            PatternNodeKind::Range(.., ty) => {
                self.range_joiner.prepare_for(self.range_storage[ty]);

                for &node in self.slices.get(options).iter() {
                    if let PatternNodeKind::Wildcard = self.nodes[node].kind {
                        self.nodes[node].reachability = Reachability::ReachableUnchecked;
                        return;
                    }

                    let PatternNodeKind::Range(range, ..) = self.nodes[node].kind else {
                        unreachable!();
                    };

                    self.range_joiner.add_range(self.range_storage[range]);
                    if self.range_joiner.exhausted() {
                        self.nodes[node].reachability = Reachability::ReachableUnchecked;
                        return;
                    }

                    self.nodes[node].reachability = Reachability::Reachable;
                }

                todo!("Range is not fully covered: {:?}", self.range_joiner.missing());
            },
            
            PatternNodeKind::Wildcard => {
                // we only need to set reachability of firs wildcard, 
                // nodes are by default unreachable
                self.nodes[first].reachability = Reachability::ReachableUnchecked;
            },
        }
    }

    fn join(
        mut target: PatternNode,
        mut source: PatternNode,
        nodes: &mut PrimaryMap<PatternNode, PatternNodeEnt>,
        slices: &mut StackMap<PatternNodeList, PatternNode>,
    ) {

        /*
            And(
                0,
                And(
                    1,
                    1,
                ),
            )
            And(
                0, // must be same
                And(
                    1,
                    2,
                ),
            )

            And(
                0,
                Or(
                    And(
                        1,
                        1,
                    ),
                    And(
                        1,
                        2,
                    ),
                ),
            )

            And(
                0,                
                    And(
                        1,
                        Or(1, 2),
                    ),
                ),
            )
        */ 

        loop {
            todo!()
        }
    }

    fn coverage_of(
        mut node: PatternNode,
        nodes: &PrimaryMap<PatternNode, PatternNodeEnt>,
    ) -> PatternRange {
        node = Self::root_of(node, nodes);
        
        let PatternNodeKind::Range(range, ..) = nodes[node].kind else {
            unreachable!();
        };

        range
    }

    fn root_of(
        mut node: PatternNode,
        nodes: &PrimaryMap<PatternNode, PatternNodeEnt>,
    ) -> PatternNode {
        while let PatternNodeKind::And(root, ..) = nodes[node].kind {
            node = root;
        }

        node
    }

    pub fn reachability(&self, node: PatternNode) -> Reachability {
        self.nodes[node].reachability
    }
    
    pub fn clear(&mut self) {
        self.nodes.clear();
        self.slices.clear();

        self.range_storage.clear();
    }
}

#[derive(Clone, Copy, Default)]
pub struct PatternNodeEnt {
    reachability: Reachability,
    kind: PatternNodeKind,
}

#[derive(Clone, Copy)]
pub enum PatternNodeKind {
    And(PatternNode, PatternNode),
    Or(PatternNodeList),
    Range(PatternRange, PatternRange),
    Wildcard,
}

impl Default for PatternNodeKind {
    fn default() -> Self {
        PatternNodeKind::Wildcard
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Reachability {
    Reachable,
    ReachableUnchecked,
    Unreachable,
}

impl Default for Reachability {
    fn default() -> Self {
        Reachability::Unreachable
    }
}

gen_entity!(PatternRange);
gen_entity!(PatternNode);
gen_entity!(PatternNodeList);

struct RangeStorage {
    lookup: HashMap<u64, PatternRange>,
    ranges: PrimaryMap<PatternRange, PatternRangeEnt>,
}

impl RangeStorage {
    pub fn new() -> Self {
        Self {
            lookup: HashMap::new(),
            ranges: PrimaryMap::new(),
        }
    }

    pub fn insert(&mut self, range: PatternRangeEnt) -> PatternRange {
        let hash = {
            let mut hasher = DefaultHasher::new();
            range.hash(&mut hasher);
            hasher.finish()
        };

        *self.lookup.entry(hash).or_insert_with(|| {
            self.ranges.push(range)
        })
    }

    pub fn clear(&mut self) {
        self.lookup.clear();
        self.ranges.clear();
    }
}

impl Index<PatternRange> for RangeStorage {
    type Output = PatternRangeEnt;

    fn index(&self, id: PatternRange) -> &Self::Output {
        &self.ranges[id]
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! build_pattern_graph {
        ($target:expr, $(types [ $($range_name:ident: $range_value:expr),* $(,)? ] ,)? root $body:tt) => {
            {
                let mut builder = PatternGraphBuilder::new(&mut $target);
                
                $($(
                    let __rng: Range<i32> = $range_value;
                    let $range_name = builder.create_range(__rng);
                )*)?
                
                build_pattern_graph!(builder, or $body)
            }
        };
    
        (build_list $builder:expr, [ $($name:ident $($inner:tt)?)* ]) => {
            {
                #[allow(unused_mut)]
                let mut nodes = vec![];
                $(
                    nodes.push(build_pattern_graph!($builder, $name $($inner)?));
                )*
                $builder.create_list(nodes.as_slice())
            }
        };
        
        ($builder:expr, or $body:tt) => {
            {
                let nodes = build_pattern_graph!(build_list $builder, $body);
                $builder.create_node(PatternNodeKind::Or(nodes))
            }
        };
    
        ($builder:expr, tuple [ $($body:tt)* ]) => {
            build_pattern_graph!(recur_and $builder, $($body)*)
        };

        (recur_and $builder:expr, $name:ident $body:tt $($other:tt)+) => {
            {
                let next = build_pattern_graph!(recur_and $builder, $($other)+);    
                let current = build_pattern_graph!($builder, $name $body);
                $builder.create_node(PatternNodeKind::And(current, next)) 
            }
        };
        
        (recur_and $builder:expr, $name:ident $body:tt) => {
            build_pattern_graph!($builder, $name $body)
        };

        ($builder:expr, enum [ id $id:literal ty $ty:ident $($body:tt)* ]) => {
            {
                let nodes = build_pattern_graph!(build_list $builder, [ range [$id..$id, $ty] $($body)* ]);
                $builder.create_node(PatternNodeKind::Enum(nodes))
            }
        };
    
        ($builder:expr, range [$range:expr, $ty:ident]) => {
            {
                let range: Range<i32> = $range;
                let range = $builder.create_range(range);                
                $builder.create_node(PatternNodeKind::Range(range, $ty))
            }
        };
    
        ($builder:expr, wildcard[]) => {
            {
                $builder.create_node(PatternNodeKind::Wildcard)
            }
        };
    }    

    #[test]
    fn test_range_and_wildcard() {
        let mut graph = PatternGraph::new();
        let root = build_pattern_graph!(
            graph, 
            types [ 
                i8: 0..255,
            ], 
            root [
                // tuple [ 
                    range [0..2, i8]
                    range [0..2, i8]
                // ]    
                // tuple [ 
                    wildcard[]
                    wildcard[]
                // ]
            ]
        );

        graph.solve(root);
    }

    #[test]
    fn test_tuple() {
        let mut graph = PatternGraph::new();
        let root = build_pattern_graph!(
            graph, 
            types [ 
                i8: 0..255,
            ], 
            root [
                tuple [ 
                    range [0..2, i8]
                    range [0..2, i8]
                ]    
                tuple [ 
                    wildcard[]
                    wildcard[]
                ]
            ]
        );

        graph.solve(root);
    }
}
*/

/////////////////////////////////////////////////////////////////////////////////////////////////////////

/*#[derive(Clone)]
pub struct PatternGraph {
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
        let mut nodes = PrimaryMap::new();
        let root = nodes.push(Default::default());

        Self {
            cons: StackMap::new(),
            nodes,
            root,

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
                self.joiner.prepare_for(range);
                
                let children = self.nodes[current].children;   
                let children = self.cons.get(children);

                let child = children.iter()
                    .position(|&child| {
                        self.joiner.exhaust(self.nodes[child].coverage);
                        self.joiner.exhausted()
                    })
                    .unwrap_or_else(|| {
                        panic!("{:?}", self.joiner.missing());
                    });

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
            node_ent.range,
            self.cons
                .get(node_ent.children)
                .iter()
                .map(|&child| self.nodes[child].coverage),
        );

        for i in 0..self.cons.len(node_ent.children) {
            let child = self.cons.get(node_ent.children)[i];
            let mut child_ent = self.nodes[child];
            let wildcard = child_ent.coverage.is_wildcard();
            let mut wildcard_clone = None;
            for segment in self.splitter.segments_of(child_ent.coverage) {
                if let Some(&node) = self.range_lookup.get(&segment) {
                    if wildcard
                        && let Some(&first) = self.cons.get(self.nodes[node].children).first() 
                        && self.nodes[first].depth > child_ent.depth
                    {
                        let clone = wildcard_clone.unwrap_or_else(|| { 
                            let clone = self.nodes.push(NodeEnt { 
                                depth: child_ent.depth + 1, 
                                range: self.nodes[first].range, 
                                ..child_ent 
                            });
                            child_ent.children = self.cons.push(&[clone]);
                            wildcard_clone = Some(clone);
                            clone
                        });

                        self.cons.push_to(&mut self.nodes[node].children, clone);
                    } else {
                        self.nodes[node].children = self
                            .cons
                            .join(self.nodes[node].children, self.nodes[child].children);
                    }
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

        self.joiner.prepare_for(node_ent.range);

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

    pub fn set_branch_count(&mut self, branch_count: usize) {
        self.nodes[self.root].children = self.cons.alloc(branch_count, PatternNode::reserved_value());
    }

    pub fn add_branch(&mut self, nodes: impl IntoIterator<Item = (IntRange, IntRange, u32)>) {
        assert!(!self.root.is_reserved_value());
        let branch = self.branch_counter as u32;
        let mut current = self.root;
        for (coverage, range, depth) in nodes.into_iter() {
            self.nodes[current].range = range;
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
        }
        self.branch_counter += 1;
    }

    pub fn clear(&mut self) {
        self.cons.clear();
        self.nodes.clear();
        self.nodes.push(Default::default());
        self.branch_counter = 0;
    }

    pub fn log(&self) {
        self.log_recursive(self.root, 0);
    }

    fn log_recursive(&self, node: PatternNode, depth: usize) {
        let ent = self.nodes[node];
        println!("{}{:?} {} {}", " ".repeat(depth), ent.coverage.decode::<i32>(), ent.branch, ent.depth);
        for child in self.cons.get(ent.children) {
            self.log_recursive(*child, depth + 1);
        }
    }
}

#[derive(Clone, Copy, Default)]
struct NodeEnt {
    range: IntRange,
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
    use std::ops::RangeInclusive;

    use super::{*, Reachability::*};

    #[test]
    fn base_case() {
        run_test(
            0..=255,
            [
                &[(0..=20, 0), (0..=20, 0)],
                &[(0..=50, 0), (0..=50, 0)],
                &[(0..=255, 0), (0..=255, 0)],
                &[(30..=255, 0), (30..=255, 0)],
            ],
            [
                &[Reachable, Reachable],
                &[Reachable, Reachable],
                &[ReachableUnchecked, ReachableUnchecked],
                &[Unreachable, Unreachable],
            ],
        );

        run_test(
            0..=255,
            [
                &[(0..=0, 0)],
                &[(1..=1, 0)],
                &[(2..=2, 0)],
                &[(0..=2, 0)],
                &[(0..=255, 0)],
            ],
            [
                &[Reachable],
                &[Reachable],
                &[Reachable],
                &[Unreachable],
                &[ReachableUnchecked],
            ],
        );

        run_test(
            0..=255,
            [
                &[(0..=6, 0), (0..=5, 0)],
                &[(4..=10, 0), (5..=10, 0)],
                &[(5..=5, 0), (0..=10, 0)],
                &[(0..=255, 0), (0..=255, 0)],
            ],
            [
                &[Reachable, Reachable],
                &[Reachable, Reachable],
                &[Unreachable, Unreachable],
                &[ReachableUnchecked, ReachableUnchecked],
            ],
        );

        run_test(
            0..=255,
            [
                &[(0..=0, 1), (0..=255, 2), (0..=0, 1)],
                &[(1..=255, 1), (0..=255, 1)],
                &[(0..=0, 1), (0..=255, 2), (1..=255, 1)],
            ],
            [
                &[Reachable, ReachableUnchecked, Reachable],
                &[ReachableUnchecked, ReachableUnchecked],
                &[Unreachable, Unreachable, ReachableUnchecked],
            ],
        );

        run_test(
            0..=255,
            [
                &[(0..=0, 1), (0..=0, 1)],
                &[(1..=255, 1), (0..=255, 1)],
                &[(0..=255, 1), (0..=255, 1)],
            ],
            [
                &[Reachable, Reachable],
                &[ReachableUnchecked, ReachableUnchecked],
                &[Unreachable, Unreachable],
            ],
        );
    }

    #[test]
    fn unreachable_by_combination() {
        run_test(
            0..=255,
            [
                &[(0..=4, 0), (10..=11, 0)],
                &[(2..=6, 0), (10..=11, 0)],
                &[(0..=6, 0), (10..=11, 0)],
                &[(0..=255, 0), (0..=255, 0)],
            ],
            [
                &[Reachable, Reachable],
                &[Reachable, Reachable],
                &[Unreachable, Unreachable],
                &[ReachableUnchecked, ReachableUnchecked],
            ],
        );
    }

    fn run_test<const H: usize>(
        range: RangeInclusive<i32>, 
        branches: [&[(RangeInclusive<i32>, u32)]; H],
        reachability: [&[Reachability]; H],
    ) {
        let mut tree = PatternGraph::new();
        let range = IntRange::new_bounds(&range);

        tree.set_branch_count(branches.len());

        for branch in branches.clone() {
            tree.add_branch(branch.iter().map(|(coverage, id)| (range.coverage(&coverage), range, *id)));
        }

        tree.solve().map_err(|e| {
            tree.log();
            e
        }).unwrap();

        let for_logging = tree.clone();

        for (i, (reach, branch)) in reachability.into_iter().zip(branches).enumerate() {
            let iter = tree.get_reachability(i, branch.iter().map(|(coverage, _)| range.coverage(&coverage)));
            for (j, (a, &b)) in iter.zip(reach).enumerate() {
                if a.0 != b {
                    for_logging.log();
                    assert_eq!(a.0, b, "branch {} at {}", i, j);
                    return;
                }
            }
        }
    } 
}*/

///////////////////////////////////////////////////////////////////////////////////////////////////////////////

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
