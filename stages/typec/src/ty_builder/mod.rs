use std::default::default;

use lexing_t::*;
use parsing::*;
use parsing_t::*;
use storage::*;
use typec_t::*;

use crate::*;

impl TyChecker<'_> {
    pub fn build<T: CollectGroup>(
        &mut self,
        items: GroupedItemSlice<T>,
        builder: fn(&mut Self, VRef<T::Output>, T),
        types: &TypecOutput<T::Output>,
    ) -> &mut Self {
        for &(i, ty) in types {
            let (ast, ..) = items[i];
            builder(self, ty, ast);
        }

        self
    }

    pub fn build_spec(&mut self, spec: VRef<Spec>, SpecAst { generics, body, .. }: SpecAst) {
        let frame = self.scope.start_frame();

        self.insert_generics(generics, 0);
        let methods = self.build_spec_methods(spec, body, generics.len());
        let generics = self.generics(generics);
        self.typec.specs[spec].kind = SpecBase {
            inherits: default(),
            generics,
            methods,
        }
        .into();

        self.scope.end_frame(frame);
    }

    fn build_spec_methods(
        &mut self,
        parent: VRef<Spec>,
        body: SpecBodyAst,
        offset: usize,
    ) -> VSlice<SpecFunc> {
        let mut methods = bumpvec![cap body.len()];
        for &func in body.iter() {
            let Some((signature, generics)) = self.collect_signature(func, offset) else {
                continue;
            };

            let func = SpecFunc {
                generics,
                signature,
                loc: Loc::new(func.name.ident, self.source, func.name.span, func.span()),
                parent,
            };

            methods.push(func);
        }
        self.typec.spec_funcs.bump(methods)
    }

    pub fn build_struct(&mut self, ty: VRef<Ty>, StructAst { generics, body, .. }: StructAst) {
        let frame = self.scope.start_frame();

        self.insert_generics(generics, 0);
        let fields = self.struct_fields(body);
        let generics = self.generics(generics);
        self.typec.types[ty].kind = TyStruct { generics, fields }.into();

        self.scope.end_frame(frame);
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
                        loc: Loc::new(name.ident, self.source, name.span, field.span()),
                    })
                },
            )
            .nsc_collect::<Option<BumpVec<_>>>()
            .unwrap_or_default();
        self.typec.fields.bump(fields)
    }

    insert_scope_item!();
}
