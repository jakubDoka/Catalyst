use std::default::default;

use parsing_t::*;
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

        let generics_len = self.insert_generics(generics, 0);
        self.scope
            .push(Interner::SELF, Ty::Param(generics_len as u8), name.span);
        let mut spec_set = SpecSet::default();
        self.generics(generics, &mut spec_set, 0);
        let inherits = self.build_inherits(inherits, &mut spec_set);
        let methods = self.build_spec_methods(spec, body, &mut spec_set, generics_len + 1);
        let generics = self.take_generics(0, generics_len, &mut spec_set);
        let ent = Typec::get_mut(&mut self.typec.base_specs, spec);
        *ent = SpecBase {
            generics,
            methods,
            inherits,
            ..*ent
        };

        self.scope.end_frame(frame);
    }

    fn build_inherits(
        &mut self,
        inherits: Option<ParamSpecsAst>,
        spec_set: &mut SpecSet,
    ) -> FragSlice<Spec> {
        let Some(inherits) = inherits else {
            return default();
        };
        self.spec_sum(inherits.specs(), spec_set)
            .unwrap_or_default()
    }

    fn build_spec_methods(
        &mut self,
        parent: FragRef<SpecBase>,
        body: Option<ListAst<FuncSigAst>>,
        spec_set: &mut SpecSet,
        offset: usize,
    ) -> FragSlice<SpecFunc> {
        let Some(body) = body else {
            return default();
        };

        let mut methods = bumpvec![cap body.len()];
        for &func in body.iter() {
            let Some((signature, generics)) = self.collect_signature(func, spec_set, offset) else {
                continue;
            };

            let func = SpecFunc {
                generics,
                signature,
                name: func.name.ident,
                span: func.name.span,
                parent,
            };

            methods.push(func);
        }
        self.typec.spec_funcs.extend(methods)
    }

    pub fn build_enum(&mut self, r#enum: FragRef<Enum>, EnumAst { generics, body, .. }: EnumAst) {
        let frame = self.scope.start_frame();

        let generics_len = self.insert_generics(generics, 0);
        let mut spec_set = SpecSet::default();
        self.generics(generics, &mut spec_set, 0);
        let variants = self.enum_variants(body, &mut spec_set);
        let generics = self.take_generics(0, generics_len, &mut spec_set);
        let ent = Typec::get_mut(&mut self.typec.enums, r#enum);
        *ent = Enum {
            generics,
            variants,
            ..*ent
        };

        self.scope.end_frame(frame);
    }

    pub fn enum_variants(
        &mut self,
        body: Option<ListAst<EnumVariantAst>>,
        spec_set: &mut SpecSet,
    ) -> FragSlice<Variant> {
        let Some(body) = body else {return default()};
        let variants = body
            .iter()
            .filter_map(|EnumVariantAst { name, ty }| {
                let ty = ty.map_or(Some(Ty::UNIT), |(.., ty)| self.ty(ty))?;
                self.typec.register_ty_generics(ty, spec_set);
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

        let generics_len = self.insert_generics(generics, 0);
        let mut spec_set = SpecSet::default();
        self.generics(generics, &mut spec_set, 0);
        let fields = self.struct_fields(body, &mut spec_set);
        let generics = self.take_generics(0, generics_len, &mut spec_set);
        let ent = Typec::get_mut(&mut self.typec.structs, ty);
        *ent = Struct {
            generics,
            fields,
            ..*ent
        };

        self.scope.end_frame(frame);
    }

    fn struct_fields(
        &mut self,
        body: Option<ListAst<StructFieldAst>>,
        spec_set: &mut SpecSet,
    ) -> FragSlice<Field> {
        let Some(body) = body else {return default()};

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
                        vis: vis.map(|vis| vis.vis),
                        ty: {
                            let ty = self.ty(ty)?;
                            self.typec.register_ty_generics(ty, spec_set);
                            ty
                        },
                        flags: FieldFlags::MUTABLE & mutable.is_some()
                            | FieldFlags::USED & used.is_some(),
                        span: name.span,
                        name: name.ident,
                    })
                },
            )
            .nsc_collect::<Option<BumpVec<_>>>()
            .unwrap_or_default();
        self.typec.fields.extend(fields)
    }
}
