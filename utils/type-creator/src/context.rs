use storage::*;
use types::*;

pub struct TypeCreator<'ctx, 'arena> {
    pub types: &'ctx mut Types,
    pub interner: &'ctx mut Interner,
    pub arena: &'ctx mut ProxyArena<'arena>,
}

#[macro_export]
macro_rules! type_creator {
    ($self:expr) => {
        $crate::TypeCreator {
            types: &mut $self.types,
            interner: &mut $self.interner,
            arena: &$self.arena,
        }
    };

    (let $name:ident = $self:expr) => {
        proxy_arena!(let arena = $self.arena);
        let $name = $crate::TypeCreator {
            types: &mut $self.types,
            interner: &mut $self.interner,
            arena: &mut arena,
        };
    };
}
