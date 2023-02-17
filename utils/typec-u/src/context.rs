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

pub struct FolderValue(u64);

impl FolderValue {
    pub fn new_register(value: u64) -> Self {
        FolderValue(value)
    }

    pub fn as_array_size(&self) -> Option<ArraySize> {
        Some(self.0 as ArraySize)
    }
}

pub trait ConstFolder {
    fn fold(&mut self, code: TirNode, typec: &mut Typec, interner: &mut Interner) -> FolderValue;
}
