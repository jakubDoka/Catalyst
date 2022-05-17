use std::fmt::Debug;

use cranelift_entity::EntityRef;

pub trait TreeStorage<I: EntityRef + 'static>
where
    Self: Sized,
{
    /// Return child of node at given index.
    fn child(&self, id: I, idx: usize) -> I;

    /// Return number of children of node.
    fn node_len(&self, id: I) -> usize;

    /// Return number of nodes in tree.
    fn max_node(&self) -> usize;

    fn nodes(&self, dump: &mut Vec<I>) {
        dump.extend((0..self.max_node()).map(|i| I::new(i)))
    }

    fn detect_cycles(&self, root: I, ordering: Option<&mut Vec<I>>) -> Result<(), Vec<I>> {
        self.detect_cycles_with_resources(root, &mut CycleDetectResources::new(), ordering)
    }

    fn total_ordering(&self, ordering: &mut Vec<I>) -> Result<(), Vec<I>> {
        let mut resources = CycleDetectResources::new();
        let mut nodes = vec![];
        self.nodes(&mut nodes);
        nodes
            .into_iter()
            .map(|i| self.detect_cycles_with_resources(i, &mut resources, Some(ordering)))
            .find(|r| r.is_err())
            .unwrap_or(Ok(()))
    }

    /// Returns none if no cycles found, otherwise returns sequence
    /// of nodes creating the cycle. `stack` should be empty, lookup
    /// has to be as long as the number of nodes. Optionally, ordering
    /// can be passed to create order in which no children is preceding
    /// its parents.
    fn detect_cycles_with_resources(
        &self,
        root: I,
        CycleDetectResources { stack, lookup }: &mut CycleDetectResources<I>,
        mut ordering: Option<&mut Vec<I>>,
    ) -> Result<(), Vec<I>> {
        lookup.resize(self.max_node(), (false, false));
        stack.push((root, 0));

        while let Some(&(node, index)) = stack.last() {
            let (seen, in_recurse) = lookup[node.index()];

            if in_recurse {
                return Err(stack
                    .drain(stack.iter().position(|i| i.0 == node).unwrap()..)
                    .map(|i| i.0)
                    .collect());
            }

            let done = self.node_len(node) == index;
            if done || seen {
                if !seen {
                    ordering.as_mut().map(|o| o.push(node));
                }
                lookup[node.index()].0 = true;
                stack.pop().unwrap();
                if stack.len() != 0 {
                    lookup[stack[stack.len() - 1].0.index()].1 = false;
                }
                continue;
            }

            let len = stack.len();
            stack[len - 1].1 += 1;
            lookup[node.index()] = (false, true);
            stack.push((self.child(node, index), 0));
        }

        Ok(())
    }
}

impl<T: EntityRef + 'static + Debug> TreeStorage<T> for GenericGraph {
    fn child(&self, id: T, idx: usize) -> T {
        assert!(idx < self.node_len(id));
        T::new(self.edges[self.hints[id.index()] as usize + idx] as usize)
    }

    fn node_len(&self, id: T) -> usize {
        (self.hints[id.index() + 1] - self.hints[id.index()]) as usize
    }

    fn max_node(&self) -> usize {
        self.hints.len() - 1
    }
}

#[derive(Debug)]
pub struct GenericGraph {
    pub hints: Vec<u32>,
    pub edges: Vec<u32>,
    pub offset: u32,
}

impl GenericGraph {
    pub fn new() -> Self {
        Self {
            hints: vec![0],
            edges: Vec::new(),
            offset: 0,
        }
    }

    pub fn close_node(&mut self, node: u32) {
        let node = (node - self.offset) as usize;
        let &last = self.hints.last().unwrap();
        while self.hints.len() <= node {
            self.hints.push(last);
        }
        self.hints.push(self.edges.len() as u32);
    }

    pub fn add_edge(&mut self, to: u32) {
        if to < self.offset {
            return;
        }
        self.edges.push(to - self.offset);
    }

    pub fn clear(&mut self) {
        self.hints.truncate(1);
        self.edges.clear();
        self.offset = 0;
    }

    pub fn len(&self) -> usize {
        self.hints.len() - 1
    }

    pub fn children(&self, id: usize) -> &[u32] {
        let start = self.hints[id] as usize;
        let end = self.hints[id + 1] as usize;
        &self.edges[start..end]
    }

    pub fn inverted(&self) -> Self {
        let mut previous_edges = Vec::with_capacity(self.edges.len());

        for i in 0..self.len() {
            for &j in self.children(i) {
                previous_edges.push((j, i as u32));
            }
        }

        previous_edges.sort();

        let mut edges = Vec::with_capacity(previous_edges.len());
        let mut hints = vec![0];
        let mut last = 0;
        for (i, &(to, from)) in previous_edges.iter().enumerate() {
            edges.push(from);

            while to != last {
                hints.push(i as u32);
                last += 1;
            }
        }

        Self { hints, edges, offset: self.offset }
    }
}

pub struct CycleDetectResources<I> {
    pub stack: Vec<(I, usize)>,
    pub lookup: Vec<(bool, bool)>,
}

impl<I> CycleDetectResources<I> {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            lookup: Vec::new(),
        }
    }

    pub fn clear(&mut self) {
        self.lookup.clear();
    }
}
