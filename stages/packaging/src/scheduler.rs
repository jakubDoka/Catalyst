use std::{mem, path::Path};

use packaging_t::Module;
use parsing::*;
use parsing_t::*;
use storage::*;

use crate::{packages::Context, *};

pub trait Scheduler {
    fn resources(&mut self) -> PackageLoader;
    fn init(&mut self, _: &Path) {}
    fn before_parsing(&mut self, _module: VRef<Module>) {}
    fn parse_segment(&mut self, _module: VRef<Module>, _items: GroupedItemsAst) {}
    fn finally(&mut self) {}

    fn execute(&mut self, path: &Path) {
        let mut ctx = Context::default();
        let mut res = self.resources();
        res.load(path, &mut ctx);
        let order = mem::take(&mut res.resources.module_order);
        self.init(path);

        let mut parse_state = ParsingState::new();
        let mut ast_data = AstData::new();
        for module in order {
            self.before_parsing(module);

            let res = self.resources();
            let source = res.resources.modules[module].source;
            let content = &res.resources.sources[source].content;
            parse_state.start(content);
            ParsingCtx::new(
                content,
                &mut parse_state,
                &ast_data,
                res.workspace,
                res.interner,
                source,
            )
            .parse::<UseAstSkip>();

            loop {
                let res = self.resources();
                ast_data.clear();
                let source = res.resources.modules[module].source;
                let content = &res.resources.sources[source].content;
                let items = ParsingCtx::new(
                    content,
                    &mut parse_state,
                    &ast_data,
                    res.workspace,
                    res.interner,
                    source,
                )
                .parse::<GroupedItemsAst>();

                let Some(items) = items else {
                    break;
                };

                self.parse_segment(module, items);

                if items.last {
                    break;
                }
            }
        }

        self.finally();
    }
}
