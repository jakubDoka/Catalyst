use parsing::*;
use storage::*;
use typec_t::*;

use crate::*;

impl TyChecker<'_> {
    pub fn build<T: CollectGroup>(
        &mut self,
        builder: fn(&mut Self, FragRef<T::Output>, T),
        types: &TypecOutput<T, T::Output>,
    ) -> &mut Self {
        for &(ast, ty) in types {
            builder(self, ty, ast);
        }

        self
    }

    pub fn build_spec(
        &mut self,
        spec: FragRef<SpecBase>,
        SpecAst {
            generics,
            body,
            name,
            inherits,
            ..
        }: SpecAst,
    ) {
        let frame = self.scope.start_frame();

        self.insert_generics(generics, 0);
        self.scope
            .push(Interner::SELF, Ty::Param(generics.len() as u16), name.span);
        self.typec[spec].inherits = self.build_inherits(inherits);
        self.typec[spec].generics = self.generics(generics);
        self.typec[spec].methods = self.build_spec_methods(spec, body, generics.len() + 1);

        self.scope.end_frame(frame);
    }

    fn build_inherits(&mut self, inherits: ParamSpecsAst) -> FragSlice<Spec> {
        self.spec_sum(inherits.iter()).unwrap_or_default()
    }

    fn build_spec_methods(
        &mut self,
        parent: FragRef<SpecBase>,
        body: SpecBodyAst,
        offset: usize,
    ) -> FragSlice<SpecFunc> {
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
        self.typec.spec_funcs.extend(methods)
    }

    pub fn build_enum(&mut self, ty: FragRef<Enum>, EnumAst { generics, body, .. }: EnumAst) {
        let frame = self.scope.start_frame();

        self.insert_generics(generics, 0);
        self.typec[ty].variants = self.enum_variants(body);
        self.typec[ty].generics = self.generics(generics);

        self.scope.end_frame(frame);
    }

    pub fn enum_variants(&mut self, body: EnumBodyAst) -> FragSlice<Variant> {
        let variants = body
            .iter()
            .filter_map(|EnumVariantAst { name, ty }| {
                let ty = ty.map_or(Some(Ty::UNIT), |(.., ty)| self.ty(ty))?;

                Some(Variant {
                    name: name.ident,
                    ty,
                    span: Some(name.span),
                })
            })
            .collect::<BumpVec<_>>();
        self.typec.variants.extend(variants)
    }

    pub fn build_struct(
        &mut self,
        ty: FragRef<Struct>,
        StructAst { generics, body, .. }: StructAst,
    ) {
        let frame = self.scope.start_frame();

        self.insert_generics(generics, 0);
        self.typec[ty].fields = self.struct_fields(body);
        self.typec[ty].generics = self.generics(generics);

        self.scope.end_frame(frame);
    }

    fn struct_fields(&mut self, body: StructBodyAst) -> FragSlice<Field> {
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
        self.typec.fields.extend(fields)
    }
}
