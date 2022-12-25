use std::{mem, path::Path};

use packaging_t::{Module, ResourceDb};
use parsing::*;
use storage::*;

use crate::{packages::ResourceLoaderCtx, *};

pub trait Scheduler {
    fn loader<'a>(&'a mut self, resources: &'a mut dyn ResourceDb) -> PackageLoader<'a>;

    fn init(&mut self, _: &Path) {}
    fn before_parsing(&mut self, _module: VRef<Module>) {}
    fn parse_segment(&mut self, _module: VRef<Module>, _items: GroupedItemsAst) {}
    fn finally(&mut self) {}

    fn execute(&mut self, path: &Path, resources: &mut dyn ResourceDb) {
        let mut ctx = ResourceLoaderCtx::default();
        let order = {
            let mut res = self.loader(resources);
            res.reload(path, &mut ctx);
            mem::take(&mut res.resources.module_order)
        };
        self.init(path);

        let mut ast_data = Arena::new();
        for module in order {
            self.before_parsing(module);

            let res = self.loader(resources);
            let source = res.resources.modules[module].source;
            let content = &res.resources.sources[source].content;
            let mut parser_ctx = ParserCtx::new(content);

            Parser::new(
                res.interner,
                res.workspace,
                &mut parser_ctx,
                &ast_data,
                source,
                content,
            )
            .skip_imports();

            loop {
                let items = {
                    let res = self.loader(resources);
                    ast_data.clear();
                    let source = res.resources.modules[module].source;
                    let content = &res.resources.sources[source].content;
                    Parser::new(
                        res.interner,
                        res.workspace,
                        &mut parser_ctx,
                        &ast_data,
                        source,
                        content,
                    )
                    .grouped_items()
                };

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
