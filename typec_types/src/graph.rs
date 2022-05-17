use storage::*;

pub struct Graph<E: EntityRef + ReservedValue> {
    vertices: SparseMap<E, EntityList<E>>,
    edges: ListPool<E>,
    max: usize,
}

impl<E: EntityRef + ReservedValue> Graph<E> {
    pub fn new() -> Self {
        Self {
            vertices: SparseMap::new(),
            edges: ListPool::new(),
            max: 0,
        }
    }

    pub fn add_vertex(&mut self, id: E) {
        self.max = self.max.max(id.index());
        if let Some(shadow) = self.vertices.insert(id, EntityList::new()) {
            self.vertices.insert(id, shadow);
        };
    }

    pub fn add_edge(&mut self, from: E, to: E) {
        self.max = self.max.max(from.index()).max(to.index());
        let mut list = self.vertices.get(from).copied().unwrap_or_default();
        list.push(to, &mut self.edges);
        self.vertices.insert(from, list);
    }

    pub fn clear(&mut self) {
        self.vertices.clear();
        self.edges.clear();
        self.max = 0;
    }

    pub fn vertices(&self) -> impl Iterator<Item = E> + '_ {
        self.vertices.keys()
    }
}

impl<E: EntityRef + ReservedValue + 'static> TreeStorage<E> for Graph<E> {
    fn child(&self, id: E, idx: usize) -> E {
        self.vertices
            .get(id)
            .copied()
            .unwrap_or_default()
            .get(idx, &self.edges)
            .unwrap()
    }

    fn node_len(&self, id: E) -> usize {
        self.vertices
            .get(id)
            .map(|list| list.len(&self.edges))
            .unwrap_or(0)
    }

    fn max_node(&self) -> usize {
        self.max + 1
    }

    fn nodes(&self, dump: &mut Vec<E>) {
        dump.extend(self.vertices.keys());
    }
}