use std::default::default;

use lexing_t::*;
use packaging_t::*;
use parsing::{StructAst, StructBodyAst, StructFieldAst};
use parsing_t::*;
use scope::*;
use storage::*;
use typec_t::*;

use crate::{item_collector::Structs, *};

impl TyChecker<'_> {
    pub fn build_structs(&mut self, types: &mut Structs) -> &mut Self {
        for (ast, ty) in types.drain(..) {
            self.build_struct(ty, ast);
        }

        self
    }

    fn build_struct(&mut self, ty: VRef<Ty>, StructAst { generics, body, .. }: StructAst) {
        self.scope.start_frame();

        self.insert_generics(generics, 0, true);
        let fields = self.struct_fields(ty, body);
        self.typec.types[ty].kind.cast_mut::<TyStruct>().fields = fields;

        self.scope.end_frame();
    }

    fn struct_fields(&mut self, ty: VRef<Ty>, body: StructBodyAst) -> VSlice<Field> {
        let loc = self.typec.types[ty].loc;
        let mut fields = self.typec.fields.reserve(body.len());
        for ast_field @ &StructFieldAst {
            vis,
            used,
            mutable,
            name,
            ty,
            ..
        } in body.iter()
        {
            let scope_id = self.interner.intern(scoped_ident!(loc.name, name.ident));

            let Ok(ty) = self.ty(ty) else {
                continue;
            };

            let field = Field {
                ty,
                flags: FieldFlags::MUTABLE & mutable | FieldFlags::USED & used,
                loc: Loc::new(name.ident, self.current_file, name.span, ast_field.span()),
            };
            let field_id = self.typec.fields.push_to_reserved(&mut fields, field);

            let item = ModItem::new(scope_id, field_id, name.span, ast_field.span(), vis);
            self.insert_scope_item(item);
        }
        self.typec.fields.fill_reserved(fields, default())
    }

    insert_scope_item!();
}
