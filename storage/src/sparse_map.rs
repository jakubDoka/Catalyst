use cranelift_entity::{EntityRef, SparseMapValue};

pub struct SparseMap<K: EntityRef, V> {
    inner: cranelift_entity::SparseMap<K, SparseWrapper<K, V>>,
}

impl<K: EntityRef, V> SparseMap<K, V> {
    pub fn new() -> Self {
        SparseMap {
            inner: cranelift_entity::SparseMap::new(),
        }
    }

    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        self.inner.insert(SparseWrapper(key, value)).map(|v| v.1)
    }

    pub fn remove(&mut self, key: K) -> Option<V> {
        self.inner.remove(key).map(|s| s.1)
    }

    pub fn get(&self, key: K) -> Option<&V> {
        self.inner.get(key).map(|s| &s.1)
    }

    pub fn clear(&mut self) {
        self.inner.clear();
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn keys(&self) -> impl Iterator<Item = K> + '_ {
        self.inner.values().map(|s| s.0)
    }
}

pub struct SparseWrapper<K: EntityRef, V>(K, V);

impl<K: EntityRef, V> SparseMapValue<K> for SparseWrapper<K, V> {
    fn key(&self) -> K {
        self.0
    }
}
