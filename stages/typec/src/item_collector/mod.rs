use std::default::default;

use lexing_t::Loc;
use packaging_t::*;
use parsing::{ItemAst, ItemsAst, StructAst};
use scope::*;
use storage::*;
use typec_t::*;

use crate::*;

pub type Structs<'a> = Vec<(StructAst<'a>, VRef<Ty>)>;

impl ItemCollector<'_> {
    pub fn types<'a>(&mut self, items: ItemsAst<'a>, structs: &mut Structs<'a>) {
        for &item in items.iter() {
            let res = match item {
                ItemAst::Struct(r#struct) => self.r#struct(*r#struct, structs),
                ItemAst::Func(..) => continue,
            };

            let Ok(item) = res else {
                continue;
            };

            self.insert_scope_item(item);
        }
    }

    fn r#struct<'a>(
        &mut self,
        r#struct @ StructAst {
            vis,
            generics,
            name,
            span,
            ..
        }: StructAst<'a>,
        structs: &mut Structs<'a>,
    ) -> errors::Result<ModItem> {
        let generics = ty_parser!(self, self.current_file).generics(generics);

        let key = intern_scoped_ident!(self, name.ident);

        let ty = Ty {
            kind: TyStruct {
                generics,
                ..default()
            }
            .into(),
            flags: TyFlags::GENERIC & !generics.is_empty(),
            loc: Loc::new(name.ident, self.current_file, name.span, span),
        };
        let (id, shadow) = self.typec.types.insert(key, ty);

        if shadow.is_some() {
            return Err(());
        }

        structs.push((r#struct, id));

        Ok(ModItem::new(name.ident, id, name.span, span, vis))
    }

    insert_scope_item!();
}
