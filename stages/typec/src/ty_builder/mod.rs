use std::default::default;

use parsing::*;
use storage::*;
use typec_t::*;

use crate::*;

impl TyChecker<'_> {
    pub fn build<T: CollectGroup>(
        &mut self,
        builder: fn(&mut Self, VRef<T::Output>, T),
        types: &TypecOutput<T, T::Output>,
    ) -> &mut Self {
        for &(ast, ty) in types {
            builder(self, ty, ast);
        }

        self
    }

    pub fn build_spec(
        &mut self,
        spec: VRef<Ty>,
        SpecAst {
            generics,
            body,
            name,
            ..
        }: SpecAst,
    ) {
        let frame = self.scope.start_frame();

        self.insert_generics(generics, 0);
        let self_param = self.typec.nth_param(generics.len(), self.interner);
        self.scope
            .push(self.interner.intern_str("Self"), self_param, name.span);
        let methods = self.build_spec_methods(spec, body, generics.len() + 1);
        let generics = self.generics(generics);
        self.typec.types[spec].kind = TySpec {
            inherits: default(),
            generics,
            methods,
        }
        .into();

        self.scope.end_frame(frame);
    }

    fn build_spec_methods(
        &mut self,
        parent: VRef<Ty>,
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
                name: func.name.ident,
                span: func.name.span.into(),
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
                |&StructFieldAst {
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
                        span: name.span.into(),
                        name: name.ident,
                    })
                },
            )
            .nsc_collect::<Option<BumpVec<_>>>()
            .unwrap_or_default();
        self.typec.fields.bump(fields)
    }
}
