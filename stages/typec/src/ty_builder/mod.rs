use lexing_t::*;
use parsing::{StructAst, StructBodyAst, StructFieldAst};
use parsing_t::*;
use storage::*;
use typec_t::*;

use crate::{item_collector::Structs, *};

impl TyChecker<'_, '_> {
    pub fn build_structs(&mut self, types: &mut Structs) -> &mut Self {
        for (ast, ty) in types.drain(..) {
            self.build_struct(ty, ast);
        }

        self
    }

    fn build_struct(&mut self, ty: VRef<Ty>, StructAst { generics, body, .. }: StructAst) {
        self.scope.start_frame();

        self.insert_generics(generics, 0, true);
        let fields = self.struct_fields(body);
        self.typec.types[ty].kind.cast_mut::<TyStruct>().fields = fields;

        self.scope.end_frame();
    }

    fn struct_fields(&mut self, body: StructBodyAst) -> VSlice<Field> {
        let fields = body
            .iter()
            .map(
                |field @ &StructFieldAst {
                     vis,
                     used,
                     mutable,
                     name,
                     ty,
                     ..
                 }| {
                    Some(Field {
                        vis,
                        ty: self.ty(ty)?,
                        flags: FieldFlags::MUTABLE & mutable | FieldFlags::USED & used,
                        loc: Loc::new(name.ident, self.current_file, name.span, field.span()),
                    })
                },
            )
            .nsc_collect::<Option<BumpVec<_>>>()
            .unwrap_or_default();
        self.typec.fields.bump(fields)
    }

    insert_scope_item!();
}
