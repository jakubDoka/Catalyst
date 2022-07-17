pub trait Clear {
    fn clear(&mut self);
}

impl Clear for () {
    fn clear(&mut self) {}
}
