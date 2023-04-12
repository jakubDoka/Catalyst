#![allow(incomplete_features)]
#![feature(auto_traits, negative_impls, specialization, core_intrinsics)]

mod increment;
#[cfg(test)]
#[allow(dead_code)]
mod tests;

pub use increment::{Deserializer, Increment, IncrementSafe, Serializer};
