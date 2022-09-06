use std::default::default;

use lexing_t::Loc;
use packaging_t::*;
use parsing_t::*;
use scope::*;
use storage::*;
use typec_t::*;

use crate::*;

impl ItemCollector<'_> {
    pub fn types(&mut self, ast: VSlice<Ast>, passed_data: &mut Vec<(Ast, VRef<Ty>)>) {
        for &ast in &self.ast_data[ast] {
            let res = match ast.kind {
                AstKind::Struct { vis } => self.r#struct(ast, vis),
                _ => continue,
            };

            let Ok((item, ty)) = res else {
                continue;
            };

            self.insert_scope_item(item);
            passed_data.push((ast, ty));
        }
    }

    fn r#struct(&mut self, ast: Ast, vis: Vis) -> errors::Result<(ModItem, VRef<Ty>)> {
        let [generics, ast_name, _body] = self.ast_data[ast.children] else {
            unreachable!();
        };

        let generics = ty_parser!(self, self.current_file).generics(generics, 0, true, false);

        let name = span_str!(self, ast_name.span);
        let name_ident = self.interner.intern_str(name);
        let key = intern_scoped_ident!(self, name_ident);

        let ty = Ty {
            kind: TyStruct {
                generics,
                ..default()
            }
            .into(),
            flags: TyFlags::GENERIC & !generics.is_empty(),
            loc: Loc::new(ast_name.span, self.current_file, name_ident),
        };
        let id = self.typec.types.insert_unique(key, ty);

        let item = ModItem::new(name_ident, id, ast_name.span, vis);

        Ok((item, id))
    }

    insert_scope_item!();
}
