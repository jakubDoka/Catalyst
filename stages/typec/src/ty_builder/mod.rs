use std::default::default;

use lexing_t::*;
use packaging_t::*;
use parsing_t::*;
use scope::*;
use storage::*;
use typec_t::*;

use crate::{item_collector::PassedData, *};

impl TyBuilder<'_> {
    pub fn types(&mut self, types: &mut PassedData) {
        for (ast, ty) in types.drain(..) {
            match ast.kind {
                AstKind::Struct { .. } => self.r#struct(ty, ast),
                kind => unimplemented!("{:?}", kind),
            }
        }
    }

    fn r#struct(&mut self, ty: VRef<Ty>, ast: Ast) {
        let [generics, .., body] = self.ast_data[ast.children] else {
            unreachable!();
        };

        self.scope.start_frame();

        ty_parser!(self, self.current_file).insert_generics(generics, 0, true);
        let fields = self.struct_fields(ty, body);
        self.typec.types[ty].kind.cast_mut::<TyStruct>().fields = fields;

        self.scope.end_frame();
    }

    fn struct_fields(&mut self, ty: VRef<Ty>, body: Ast) -> VSlice<Field> {
        let loc = self.typec.types[ty].loc;
        let mut fields = self
            .typec
            .fields
            .reserve(self.ast_data[body.children].len());
        for &ast_field in &self.ast_data[body.children] {
            let [ast_name, ast_ty] = self.ast_data[ast_field.children] else {
                unreachable!();
            };
            let AstKind::StructField { vis, mutable, exported } = ast_field.kind else {
                unreachable!();
            };

            let name = span_str!(self, ast_name.span);
            let name_ident = self.interner.intern_str(name);
            let scope_id = self.interner.intern(scoped_ident!(loc.name, name_ident));

            let Ok(ty) = ty_parser!(self, self.current_file).ty(ast_ty) else {
                continue;
            };

            let field = Field {
                ty,
                flags: FieldFlags::MUTABLE & mutable | FieldFlags::USED & exported,
                loc: Loc::new(name_ident, self.current_file, ast_name.span, ast_field.span),
            };
            let field_id = self.typec.fields.push_to_reserved(&mut fields, field);

            let item = ModItem::new(scope_id, field_id, ast_name.span, ast_field.span, vis);
            self.insert_scope_item(item);
        }
        self.typec.fields.fill_reserved(fields, default())
    }

    insert_scope_item!();
}
