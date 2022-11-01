use std::ptr;

/// Clear trait represents object that can be cleared
/// to its initial state while preserving resources for next use.
pub trait Clear {
    fn clear(&mut self);
}

impl Clear for () {
    fn clear(&mut self) {}
}

pub fn map_in_place<T>(value: &mut T, mapper: impl FnOnce(T) -> T) {
    unsafe {
        let old_value = ptr::read(value);
        let new_value = mapper(old_value);
        ptr::write(value, new_value);
    }
}
