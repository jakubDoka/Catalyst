use crate::*;

impl DeadCodeElim<'_> {
    // compiled is expected to have the root functions
    pub fn collect_used_funcs(
        &mut self,
        compiled: &mut Vec<Func>,
        linked: &mut Vec<Func>, 
        used_funcs: &mut EntitySet<Func>,
    ) {
        let mut i = 0;
        while let Some(&func) = compiled.get(i) {
            for reloc in &self.compile_results[func].relocs {
                let ExternalName::User { namespace, index } = reloc.name else {
                    unreachable!();
                };

                assert!(namespace == 0);

                let func = Func(index);

                if !used_funcs.insert(func) {
                    continue;
                }

                compiled.push(func);
            }

            i += 1;
        }


        compiled.retain(|&f| {
            if self.funcs[f].flags.contains(FuncFlags::EXTERNAL) {
                linked.push(f);
                return false;
            }
            true
        });
    }
}