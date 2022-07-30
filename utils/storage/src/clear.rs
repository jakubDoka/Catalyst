/// Clear trait represents object that can be cleared
/// to its initial state while preserving resources for next use.
pub trait Clear {
    fn clear(&mut self);
}

impl Clear for () {
    fn clear(&mut self) {}
}
