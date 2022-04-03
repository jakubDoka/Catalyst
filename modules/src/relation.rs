use cranelift_entity::{packed_option::ReservedValue, EntityRef};

use crate::unit::Unit;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Rel<A>(pub Unit, pub A);

impl<A: EntityRef> EntityRef for Rel<A> {
    fn new(index: usize) -> Self {
        Rel(Unit::new(index), A::new(0))
    }

    fn index(self) -> usize {
        self.0.index()
    }
}

impl<A: ReservedValue> ReservedValue for Rel<A> {
    fn reserved_value() -> Self {
        Rel(Unit::reserved_value(), A::reserved_value())
    }

    fn is_reserved_value(&self) -> bool {
        self.0.is_reserved_value()
    }
}
