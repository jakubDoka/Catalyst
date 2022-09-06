use std::default::default;

use storage::*;
use typec_t::*;

use crate::*;

impl TyUtils<'_> {
    pub fn nth_param(&mut self, index: usize, on_type: bool) -> VRef<Ty> {
        let key = self.interner.intern(ident!(
            if on_type { "ty_param " } else { "param " },
            index as u32
        ));

        let fallback = |_: &mut Types| Ty {
            kind: TyKind::Param(index as u32),
            flags: TyFlags::GENERIC,
            loc: default(),
        };

        self.typec.types.get_or_insert(key, fallback)
    }
}
