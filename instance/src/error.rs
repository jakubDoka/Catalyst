use parser::prelude::AnyError;

pub type Error = AnyError<Kind>;

pub enum Kind {}
