use std::{collections::HashMap, ops::Range};

/// Struct is wrapper around [`CycleDetector`] that provides allows for easy
/// index projection.
#[derive(Default)]
pub struct ProjectedCycleDetector {
    mapping: HashMap<(u32, bool), u32>,
    nodes: Vec<u32>,
    inner: CycleDetector,
}

impl ProjectedCycleDetector {
    /// Will allocate little bit of memory.
    pub fn new() -> Self {
        ProjectedCycleDetector::default()
    }

    /// Pass an enumeration of all nodes relevant to cycle detection.
    /// The node mapping will be built so you can proceed with building
    /// the graph. For example see [`Self::ordering`].
    ///
    /// # Panics
    ///
    /// Panics if called twice without clear. Also when there are duplicates.
    pub fn load_nodes(&mut self, nodes: impl Iterator<Item = u32>) {
        assert!(
            self.nodes.is_empty(),
            "load_nodes called twice without clear"
        );
        self.nodes.extend(nodes.enumerate().map(|(i, n)| {
            assert!(
                self.mapping.insert((i as u32, false), n).is_none(),
                "node already loaded"
            );
            self.mapping.insert((n, true), i as u32);
            n
        }));
        self.nodes.reverse();
    }

    /// Creates [`ProjectedCycleDetectorNode`] instance that will
    /// finalize node insertion upon drop.
    /// For example and more info see [`Self::ordering`].
    ///
    /// # Panics
    ///
    /// Panics if index is not expected as next. Order of inserting must be consistent with
    /// what you passed to [`Self::load_nodes`].
    pub fn new_node(&mut self, index: u32) -> ProjectedCycleDetectorNode {
        assert!(
            Some(index) == self.nodes.pop(),
            "Incorrect ordering or node instantiation."
        );
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
        roots: impl IntoIterator<Item = u32>,
        buffer: &mut Vec<u32>,
    ) -> Result<(), Vec<u32>> {
        assert!(
            !self.mapping.is_empty(),
            "You must call `load_nodes` before calling `ordering`."
        );

        self.inner
            .ordering(
                roots
                    .into_iter()
                    .filter_map(|r| self.mapping.get(&(r, true)).copied()),
                buffer,
            )
            .map_err(|mut e| {
                e.iter_mut().for_each(|n| {
                    if let Some(&mapped) = self.mapping.get(&(*n, false)) {
                        *n = mapped;
                    }
                });
                e
            })?;

        buffer.iter_mut().for_each(|n| {
            if let Some(&mapped) = self.mapping.get(&(*n, false)) {
                *n = mapped;
            }
        });

        Ok(())
    }

    /// Prepares struct for reuse. Allocations are preserved.
    pub fn clear(&mut self) {
        self.mapping.clear();
        self.nodes.clear();
        self.inner.clear();
    }
}

/// Struct is wrapper around [`CycleDetectorNode`] that just projects
/// inputted edges.
pub struct ProjectedCycleDetectorNode<'a> {
    mapping: &'a HashMap<(u32, bool), u32>,
    inner: CycleDetectorNode<'a>,
}

impl ProjectedCycleDetectorNode<'_> {
    pub fn add_edge(&mut self, to: u32) {
        if let Some(&mapped) = self.mapping.get(&(to, true)) {
            self.inner.add_edge(mapped);
        }
    }

    pub fn add_edges(&mut self, to: impl IntoIterator<Item = u32>) {
        self.inner.add_edges(
            to.into_iter()
                .filter_map(|t| self.mapping.get(&(t, true)).copied()),
        );
    }
}

pub struct CycleDetector {
    // graph repr
    edges: Vec<u32>,
    indices: Vec<u32>,

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
        self.indices.push(self.edges.len() as u32);
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
        roots: impl IntoIterator<Item = u32>,
        buffer: &mut Vec<u32>,
    ) -> Result<(), Vec<u32>> {
        self.meta.clear();
        self.meta.resize(self.len(), NodeMeta::default());

        for root in roots {
            self.sub_ordering(root, buffer)?;
        }

        Ok(())
    }

    fn sub_ordering(&mut self, root: u32, buffer: &mut Vec<u32>) -> Result<(), Vec<u32>> {
        self.stack.push(StackFrame::new(
            root,
            Self::children_indices(&self.indices, root),
        ));

        while let Some(StackFrame { node, children }) = self.stack.last_mut() {
            let node = *node as usize;
            let NodeMeta { seen, is_recursive } = self.meta[node];

            if is_recursive {
                return Err(self
                    .stack
                    .drain(
                        self.stack
                            .iter()
                            .position(|i| i.node == node as u32)
                            .unwrap()..,
                    )
                    .map(|i| i.node)
                    .collect());
            }

            if !seen {
                if let Some(neighbor) = children.next() {
                    self.meta[node].is_recursive = true;
                    let edge = self.edges[neighbor as usize];
                    self.stack.push(StackFrame::new(
                        edge,
                        Self::children_indices(&self.indices, edge),
                    ));
                    continue;
                } else {
                    buffer.push(node as u32);
                }
            }

            self.meta[node].seen = true;
            self.stack.pop().unwrap();
            if let Some(&StackFrame { node, .. }) = self.stack.last() {
                self.meta[node as usize].is_recursive = false;
            }
        }

        Ok(())
    }

    fn children_indices(indices: &[u32], node: u32) -> Range<u32> {
        let start = indices[node as usize];
        let end = indices[node as usize + 1];
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
    node: u32,
    children: Range<u32>,
}

impl StackFrame {
    fn new(node: u32, children: Range<u32>) -> Self {
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
    pub fn add_edges(&mut self, edges: impl IntoIterator<Item = u32>) {
        self.inner.edges.extend(edges);
    }

    pub fn add_edge(&mut self, to: u32) {
        self.inner.edges.push(to);
    }
}

impl Drop for CycleDetectorNode<'_> {
    fn drop(&mut self) {
        self.inner.close_node();
    }
}
