use std::{mem, path::Path};

use parsing::ItemsAst;
use parsing_t::{AstData, ParsingCtx, ParsingState};
use storage::VRef;

use crate::PackageLoader;

pub trait Scheduler {
    fn resources(&mut self) -> PackageLoader;
    fn init(&mut self) {}
    fn before_parsing(&mut self, _module: VRef<str>) {}
    fn parse_segment(&mut self, _module: VRef<str>, _items: ItemsAst) {}
    fn finally(&mut self) {}

    fn execute(&mut self, path: &Path) {
        let mut res = self.resources();
        res.load(path);
        let order = mem::take(&mut res.packages.module_order);
        self.init();

        let mut parse_state = ParsingState::new();
        let mut ast_data = AstData::new();
        for module in order {
            self.before_parsing(module);

            let mod_ent = self.resources().packages.modules.get(&module).unwrap();
            parse_state.start(&mod_ent.content, module);
            loop {
                let res = self.resources();
                ast_data.clear();
                let mod_ent = res.packages.modules.get(&module).unwrap();
                let items = ParsingCtx::new(
                    &mod_ent.content,
                    &mut parse_state,
                    &ast_data,
                    res.workspace,
                    res.interner,
                )
                .parse::<ItemsAst>();

                let Some(items) = items else {
                    break;
                };

                let finished = items.end.is_empty();

                self.parse_segment(module, items);

                if finished {
                    break;
                }
            }
        }

        self.finally();
    }
}
