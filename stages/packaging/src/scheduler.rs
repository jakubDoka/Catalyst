use std::{mem, path::Path};

use packaging_t::Module;
use parsing::*;
use parsing_t::*;
use storage::*;

use crate::{packages::PackageLoaderCtx, *};

pub trait Scheduler {
    fn loader(&mut self) -> PackageLoader;

    fn init(&mut self, _: &Path) {}
    fn before_parsing(&mut self, _module: VRef<Module>) {}
    fn parse_segment(&mut self, _module: VRef<Module>, _items: GroupedItemsAst) {}
    fn finally(&mut self) {}

    fn execute(&mut self, path: &Path) {
        let mut ctx = PackageLoaderCtx::default();
        let order = {
            let mut res = self.loader();
            res.reload(path, &mut ctx);
            mem::take(&mut res.resources.module_order)
        };
        self.init(path);

        let mut parse_state = ParsingState::new();
        let mut ast_data = AstData::new();
        for module in order {
            self.before_parsing(module);

            {
                let res = self.loader();
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
            }

            loop {
                let items = {
                    let res = self.loader();
                    ast_data.clear();
                    let source = res.resources.modules[module].source;
                    let content = &res.resources.sources[source].content;
                    // hu boy
                    let mut p = ParsingCtx::new_with_macros(
                        content,
                        &mut parse_state,
                        &ast_data,
                        res.workspace,
                        res.interner,
                        res.token_macro_ctx.as_ref(),
                        source,
                    );
                    let r = p.parse::<GroupedItemsAst>();
                    drop(p);
                    r
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
