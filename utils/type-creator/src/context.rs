use storage::*;
use types::*;

pub struct TypeCreator<'ctx> {
    pub types: &'ctx mut Types,
    pub interner: &'ctx mut Interner,
}

#[macro_export]
macro_rules! type_creator {
    ($self:expr) => {
        $crate::TypeCreator {
            types: &mut $self.types,
            interner: &mut $self.interner,
        }
    };
}
