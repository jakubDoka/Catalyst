use std::fmt::Debug;

use cranelift_entity::EntityRef;

pub trait TreeStorage<I: EntityRef + 'static + Debug>
where
    Self: Sized,
{
    /// Return child of node at given index.
    fn child(&self, id: I, idx: usize) -> I;

    /// Return number of children of node.
    fn node_len(&self, id: I) -> usize;

    /// Return number of nodes in tree.
    fn len(&self) -> usize;

    fn detect_cycles(&self, root: I, ordering: Option<&mut Vec<I>>) -> Option<Vec<I>> {
        self.detect_cycles_with_resources(root, &mut CycleDetectResources::new(), ordering)
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
    ) -> Option<Vec<I>> {
        lookup.resize(self.len(), (false, false));
        stack.push((root, 0));

        while let Some(&(node, index)) = stack.last() {
            let (seen, in_recurse) = lookup[node.index()];

            if in_recurse {
                return Some(
                    stack
                        .drain(stack.iter().position(|i| i.0 == node).unwrap()..)
                        .map(|i| i.0)
                        .collect(),
                );
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

        None
    }
}

#[derive(Debug)]
pub struct GenericGraph {
    hints: Vec<u32>,
    edges: Vec<u32>,
}

impl GenericGraph {
    pub fn new() -> Self {
        Self {
            hints: vec![0],
            edges: Vec::new(),
        }
    }

    pub fn close_node(&mut self) {
        self.hints.push(self.edges.len() as u32);
    }

    pub fn add_edge(&mut self, to: u32) {
        self.edges.push(to);
    }

    pub fn clear(&mut self) {
        self.hints.truncate(1);
        self.edges.clear();
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

    fn len(&self) -> usize {
        self.hints.len() - 1
    }
}

pub struct CycleDetectResources<I> {
    stack: Vec<(I, usize)>,
    lookup: Vec<(bool, bool)>,
}

impl<I> CycleDetectResources<I> {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            lookup: Vec::new(),
        }
    }

    pub fn clear(&mut self) {
        self.stack.clear();
        self.lookup.clear();
    }
}
