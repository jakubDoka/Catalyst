use crate::Invalid;

/// Trait defines a virtual pointer which usually means index into vector.
pub trait VPtr: Eq + PartialEq + Copy + Invalid + Sized {
    fn new(index: usize) -> Self;
    fn index(&self) -> usize;
}