use storage::*;
use typec_t::*;

pub struct TypeCreator<'ctx> {
    pub typec: &'ctx mut Typec,
    pub interner: &'ctx mut Interner,
}

#[macro_export]
macro_rules! type_creator {
    ($self:expr) => {
        $crate::TypeCreator {
            typec: &mut $self.typec,
            interner: &mut $self.interner,
        }
    };
}
