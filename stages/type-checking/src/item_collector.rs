use crate::*;
use packaging_t::*;
use parsing_t::*;
use scope::*;
use storage::*;
use type_checking_t::*;

impl ItemCollector<'_> {
    pub fn collect(&mut self, ast: Maybe<AstList>) -> errors::Result {
        for &item in &self.ast_data[ast] {
            let res = match item.kind {
                AstKind::Struct { vis } => self.collect_struct(item, vis),
                _ => unimplemented!(),
            };

            let Ok(Some(res)) = res else {
                continue;
            };

            insert_scope_item!(self, res);
        }

        Ok(())
    }

    pub fn collect_struct(&mut self, item: AstEnt, vis: Vis) -> errors::Result<Option<ModItem>> {
        let [generics, ident, ..] = self.ast_data[item.children] else {
            unreachable!();
        };

        let local = self.interner.intern_str(span_str!(self, ident.span));
        let id = intern_scoped_ident!(self, local);
        self.visibility[id] = vis;

        let ent = TyEnt {
            kind: TyKind::Inferrable,
            flags: TyFlags::GENERIC & generics.children.is_some(),
            file: self.current_file.into(),
            span: ident.span.into(),
        };
        let ty = self.types.ents.insert_unique(id, ent);
        self.item_context.types.push((item, ty));

        Ok(Some(ModItem {
            id: local,
            ptr: ScopePtr::new(ty),
            span: ident.span,
        }))
    }
}
