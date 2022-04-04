use lexer::prelude::Sources;
use typec;

use crate::error::Error;

type Result<T = ()> = std::result::Result<T, Error>;

typec::gen_context!(Translator<'a> {
    t_functions: &'a typec::Functions,
    t_types: &'a mut typec::Types,
    sources: &'a Sources,
});

impl<'a> Translator<'a> {
    pub fn translate_func_ir(&mut self, func: typec::Func) -> Result<()> {
        for (id, block) in self.t_functions.blocks_of(func) {
            for (id, inst) in self.t_functions.insts_of(id) {}
        }

        Ok(())
    }
}
