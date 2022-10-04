use std::{mem, path::Path};

use parsing::*;
use parsing_t::*;
use storage::*;

use crate::*;

// pub trait Scheduler {
//     fn resources(&mut self) -> PackageLoader;
//     fn init(&mut self, _: &Path) {}
//     fn before_parsing(&mut self, _module: VRef<str>) {}
//     fn parse_segment(&mut self, _module: VRef<str>, _items: GroupedItemsAst) {}
//     fn finally(&mut self) {}

//     fn execute(&mut self, path: &Path) {
//         let mut res = self.resources();
//         res.load(path);
//         let order = mem::take(&mut res.resources.module_order);
//         self.init(path);

//         let mut parse_state = ParsingState::new();
//         let mut ast_data = AstData::new();
//         for module in order {
//             self.before_parsing(module);

//             let res = self.resources();
//             let mod_ent = &res.resources.modules[&module];
//             parse_state.start(&mod_ent.content, module);
//             ParsingCtx::new(
//                 &mod_ent.content,
//                 &mut parse_state,
//                 &ast_data,
//                 res.workspace,
//                 res.interner,
//             )
//             .parse::<UseAstSkip>();

//             loop {
//                 let res = self.resources();
//                 ast_data.clear();
//                 let mod_ent = &res.resources.modules[&module];
//                 let items = ParsingCtx::new(
//                     &mod_ent.content,
//                     &mut parse_state,
//                     &ast_data,
//                     res.workspace,
//                     res.interner,
//                 )
//                 .parse::<GroupedItemsAst>();

//                 let Some(items) = items else {
//                     break;
//                 };

//                 self.parse_segment(module, items);

//                 if items.last {
//                     break;
//                 }
//             }
//         }

//         self.finally();
//     }
// }
