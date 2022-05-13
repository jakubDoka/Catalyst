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

    pub fn insert(&mut self, key: K, value: V) {
        self.inner.insert(SparseWrapper(key, value));
    }

    pub fn remove(&mut self, key: K) -> Option<V> {
        self.inner.remove(key).map(|s| s.1)
    }
}

pub struct SparseWrapper<K: EntityRef, V>(K, V);

impl<K: EntityRef, V> SparseMapValue<K> for SparseWrapper<K, V> {
    fn key(&self) -> K {
        self.0
    }
}
