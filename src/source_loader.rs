use crate::*;

impl SourceLoader<'_> {
    pub fn load(&mut self) -> Vec<Source> {
        let Subcommand::Compile(path) = &self.subcommand else {
            unreachable!();
        };

        let Ok(unit_order) = unit_builder!(self).load_units(&path) else {
            return vec![];
        };

        let mut module_order = vec![];

        for unit in unit_order {
            let Ok(local_module_order) = module_builder!(self).load_unit_modules(unit) else {
                continue;
            };

            module_order.extend(local_module_order.into_iter().rev());
        }

        module_order.reverse();

        for (i, &id) in module_order.iter().enumerate() {
            self.modules[id].ordering = i;
        }

        module_order
    }
}