use std::ops::Range;

use storage::{bumpvec, BumpVec, Map};

/// Struct is wrapper around [`CycleDetector`] that provides allows for easy
/// index projection.
pub struct ProjectedCycleDetector<T> {
    mapping: Map<T, usize>,
    nodes: Vec<T>,
    node_cursor: usize,
    inner: CycleDetector,
}

impl<T: std::hash::Hash + Eq + Copy> ProjectedCycleDetector<T> {
    /// Will allocate little bit of memory.
    pub fn new() -> Self {
        Self {
            mapping: Map::default(),
            nodes: Vec::new(),
            inner: CycleDetector::new(),
            node_cursor: 0,
        }
    }

    /// Pass an enumeration of all nodes relevant to cycle detection.
    /// The node mapping will be built so you can proceed with building
    /// the graph. For example see [`Self::ordering`].
    ///
    /// # Panics
    ///
    /// Panics if called twice without clear. Also when there are duplicates.
    pub fn load_nodes(&mut self, nodes: impl Iterator<Item = T>) {
        debug_assert!(
            self.nodes.is_empty(),
            "load_nodes called twice without clear"
        );
        self.nodes.extend(nodes.enumerate().map(|(i, n)| {
            let res = self.mapping.insert(n, i);
            debug_assert!(res.is_none(), "duplicate node");
            n
        }));
    }

    /// Creates [`ProjectedCycleDetectorNode`] instance that will
    /// finalize node insertion upon drop.
    /// For example and more info see [`Self::ordering`].
    ///
    /// # Panics
    ///
    /// Panics if index is not expected as next. Order of inserting must be consistent with
    /// what you passed to [`Self::load_nodes`].
    pub fn new_node(&mut self, index: T) -> ProjectedCycleDetectorNode<T> {
        debug_assert!({
            let res = self.nodes.get(self.node_cursor) == Some(&index);
            self.node_cursor += 1;
            res
        });
        ProjectedCycleDetectorNode {
            mapping: &self.mapping,
            inner: self.inner.new_node(),
        }
    }

    /// As with every method, if you called [`Self::load_nodes`] before all other calls,
    /// this method should behave as [`CycleDetector::ordering`], but proper projected,
    /// possibly sparse nodes are outputted.
    ///
    /// # Examples
    ///
    /// ```
    /// use graphs::*;
    ///
    /// let mut pcd = ProjectedCycleDetector::new();
    /// pcd.load_nodes([10, 8, 3, 7].into_iter());
    /// { pcd.new_node(10).add_edges([8, 3, 7]); }
    /// { pcd.new_node(8).add_edges([3]); }
    /// { pcd.new_node(3).add_edges([10]); } // cycle happens here
    /// { pcd.new_node(7); } // unimportant disjoint node
    /// let mut ordering = vec![];
    ///
    /// let result = pcd.ordering(std::iter::once(10), &mut ordering);
    ///
    /// assert_eq!(result, Err(vec![10, 8, 3, 10]));
    /// ```
    ///
    /// # Panics
    ///
    /// Panics if mapping is empty, otherwise this call would do effectively nothing.
    pub fn ordering(
        &mut self,
        roots: impl IntoIterator<Item = T>,
        buffer: &mut BumpVec<T>,
    ) -> Result<(), Vec<T>> {
        assert!(
            !self.mapping.is_empty(),
            "You must call `load_nodes` before calling `ordering`."
        );

        let mut temp_buffer = bumpvec![];

        self.inner
            .ordering(
                roots
                    .into_iter()
                    .filter_map(|r| self.mapping.get(&r).copied()),
                &mut temp_buffer,
            )
            .map_err(|e| e.into_iter().map(|n| self.nodes[n]).collect::<Vec<_>>())?;

        buffer.extend(temp_buffer.into_iter().map(|n| self.nodes[n]));

        Ok(())
    }

    /// Prepares struct for reuse. Allocations are preserved.
    pub fn clear(&mut self) {
        self.mapping.clear();
        self.nodes.clear();
        self.inner.clear();
        self.node_cursor = 0;
    }
}

impl<T: std::hash::Hash + Eq + Copy> Default for ProjectedCycleDetector<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// Struct is wrapper around [`CycleDetectorNode`] that just projects
/// inputted edges.
pub struct ProjectedCycleDetectorNode<'a, T> {
    mapping: &'a Map<T, usize>,
    inner: CycleDetectorNode<'a>,
}

impl<T: std::hash::Hash + Eq> ProjectedCycleDetectorNode<'_, T> {
    pub fn add_edge(&mut self, to: T) {
        if let Some(&mapped) = self.mapping.get(&to) {
            self.inner.add_edge(mapped);
        }
    }

    pub fn add_edges(&mut self, to: impl IntoIterator<Item = T>) {
        self.inner
            .add_edges(to.into_iter().filter_map(|t| self.mapping.get(&t).copied()));
    }
}

pub struct CycleDetector {
    // graph repr
    edges: Vec<usize>,
    indices: Vec<usize>,

    // solving resources
    meta: Vec<NodeMeta>,
    stack: Vec<StackFrame>,
}

impl Default for CycleDetector {
    fn default() -> Self {
        Self {
            edges: Vec::new(),
            indices: vec![0],
            meta: Vec::new(),
            stack: Vec::new(),
        }
    }
}

impl CycleDetector {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn new_node(&mut self) -> CycleDetectorNode {
        CycleDetectorNode { inner: self }
    }

    fn close_node(&mut self) {
        self.indices.push(self.edges.len());
    }

    fn len(&self) -> usize {
        self.indices.len() - 1
    }

    /// Method performs cycle detection and outputs graph node ordering.
    /// In case of cycle existence, it is returned as error. Only first
    /// cycle is returned.
    ///
    /// # Examples
    /// ```
    /// use graphs::CycleDetector;
    ///
    /// let mut cd = CycleDetector::new();
    /// { cd.new_node().add_edges([1, 2, 3]); }
    /// { cd.new_node().add_edge(2); }
    /// { cd.new_node().add_edge(0); } // cycle is created
    /// { cd.new_node(); }             // unrelated reachable node
    /// let mut buffer = Vec::new();
    ///
    /// let result = cd.ordering(std::iter::once(0), &mut buffer);
    ///
    /// assert_eq!(result, Err(vec![0, 1, 2, 0]));
    ///
    /// let mut cd = CycleDetector::new();
    /// { cd.new_node().add_edge(1); }
    /// { cd.new_node().add_edge(2); }
    /// { cd.new_node().add_edge(3); } // cycle is created
    /// { cd.new_node(); }             // unrelated reachable node
    /// let mut buffer = Vec::new();
    ///
    /// let result = cd.ordering(std::iter::once(0), &mut buffer);
    ///
    /// assert_eq!(result, Ok(()));
    /// assert_eq!(buffer, vec![3, 2, 1, 0]);
    /// ```
    pub fn ordering(
        &mut self,
        roots: impl IntoIterator<Item = usize>,
        buffer: &mut BumpVec<usize>,
    ) -> Result<(), Vec<usize>> {
        dbg!(&self.edges, &self.indices);

        self.meta.clear();
        self.meta.resize(self.len(), NodeMeta::default());

        for root in roots {
            dbg!(root);
            self.sub_ordering(root, buffer)?;
        }

        Ok(())
    }

    fn sub_ordering(&mut self, root: usize, buffer: &mut BumpVec<usize>) -> Result<(), Vec<usize>> {
        self.stack.push(StackFrame::new(
            root,
            Self::children_indices(&self.indices, root),
        ));

        while let Some(StackFrame { node, children }) = self.stack.last_mut() {
            let node = *node;
            let NodeMeta { seen, is_recursive } = self.meta[node];

            if is_recursive {
                return Err(self
                    .stack
                    .drain(self.stack.iter().position(|i| i.node == node).unwrap()..)
                    .map(|i| i.node)
                    .collect());
            }

            if !seen {
                if let Some(neighbor) = children.next() {
                    self.meta[node].is_recursive = true;
                    let edge = self.edges[neighbor];
                    self.stack.push(StackFrame::new(
                        edge,
                        Self::children_indices(&self.indices, edge),
                    ));
                    continue;
                } else {
                    buffer.push(node);
                }
            }

            self.meta[node].seen = true;
            self.stack.pop().unwrap();
            if let Some(&StackFrame { node, .. }) = self.stack.last() {
                self.meta[node].is_recursive = false;
            }
        }

        Ok(())
    }

    fn children_indices(indices: &[usize], node: usize) -> Range<usize> {
        let start = indices[node];
        let end = indices[node + 1];
        start..end
    }

    pub fn clear(&mut self) {
        self.edges.clear();
        self.indices.truncate(1);
        self.meta.clear();
    }
}

#[derive(Debug)]
struct StackFrame {
    node: usize,
    children: Range<usize>,
}

impl StackFrame {
    fn new(node: usize, children: Range<usize>) -> Self {
        Self { node, children }
    }
}

#[derive(Default, Clone, Copy)]
pub struct NodeMeta {
    seen: bool,
    is_recursive: bool,
}

pub struct CycleDetectorNode<'a> {
    inner: &'a mut CycleDetector,
}

impl CycleDetectorNode<'_> {
    pub fn add_edges(&mut self, edges: impl IntoIterator<Item = usize>) {
        self.inner.edges.extend(edges);
    }

    pub fn add_edge(&mut self, to: usize) {
        self.inner.edges.push(to);
    }
}

impl Drop for CycleDetectorNode<'_> {
    fn drop(&mut self) {
        self.inner.close_node();
    }
}
