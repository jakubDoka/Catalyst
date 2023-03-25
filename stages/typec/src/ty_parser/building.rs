use std::{iter, mem};

use diags::AddCtlError;

use crate::{ctx::MissingSpecInherits, ty_parser::TypecParser};

use super::collecting::CollectGroup;

use {ast::*, std::default::default, storage::*, types::*};

impl<'arena, 'ctx> TypecParser<'arena, 'ctx> {
    pub(super) fn build_impl_funcs(&mut self) -> &mut Self {
        let impl_frames = mem::take(&mut self.ext.transfer.impl_frames);
        let impl_funcs = mem::take(&mut self.ext.transfer.impl_funcs);
        let iter = iter::once(0)
            .chain(impl_frames.iter().map(|&(.., i)| i))
            .zip(impl_frames.iter().copied());

        for (start, (impl_ast, impl_ref, end)) in iter {
            let funcs = &impl_funcs[start..end];

            let Some(&(.., first)) = funcs.first() else {
                 continue;
             };

            let frame = self.ctx.start_frame();

            let Func {
                generics, owner, ..
            } = self.ext.types[first];
            let offset = self.ctx.insert_generics(impl_ast.generics, 0);
            self.ctx
                .insert_spec_functions(generics, 0, self.ext.types, self.ext.interner);

            if let Some(impl_ref) = impl_ref {
                self.build_spec_impl(impl_ref);
            }
            if let Some(owner) = owner {
                self.ctx.push_self(owner, impl_ast.target.span());
            }
            self.build_funcs(funcs, offset);

            self.ctx.end_frame(frame);
        }

        self
    }

    fn build_spec_impl(&mut self, impl_ref: FragRef<Impl>) {
        let Impl {
            key: ImplKey { ty, spec },
            generics,
            loc,
            ..
        } = self.ext.types[impl_ref];

        let spec_base = spec.base(self.ext.types);
        // if SpecBase::is_macro(spec_base) && let Some(mut macro_impl) = self.ext.types.macros.get_mut(&ty) {
        //      macro_impl.r#impl = Some(impl_ref);
        //      ctx.macros.push(MacroCompileRequest {
        //          name: macro_impl.name,
        //          ty,
        //          r#impl: impl_ref,
        //          params: macro_impl.params,
        //      });
        //  }
        let spec_ent = self.ext.types[spec_base];

        // check that all inherits are implemented
        // this is necessary for the validation but also grants
        // nice optimizations since impl of this spec will imply
        // impl of all inherits
        {
            let params = match spec {
                Spec::Base(..) => default(),
                Spec::Instance(i) => self.ext.types[i].args,
            };

            let missing_impls = self.ext.types[spec_ent.inherits]
                .to_bumpvec()
                .into_iter()
                .filter_map(|inherit| {
                    self.ext
                        .creator()
                        .find_implementation(ty, inherit, generics, &mut None)
                        .is_none()
                        .then(|| self.ext.creator().instantiate_spec(inherit, params))
                        .map(|instance| self.ext.creator().display(instance))
                })
                .intersperse_with(|| " + ".into())
                .collect::<String>();

            if !missing_impls.is_empty() {
                MissingSpecInherits {
                    ty: self.ext.creator().display(ty),
                    missing: missing_impls,
                    loc: loc.source_loc(self.ext.types),
                }
                .add(self.ext.workspace);
            }
        }
    }
    pub(super) fn build<T: CollectGroup<'arena>>(
        &mut self,
        builder: fn(&mut Self, FragRef<T::Output>, T),
    ) -> &mut Self {
        let output = mem::take(T::output(self.ext.transfer));
        for &(ast, ty) in output.iter() {
            builder(self, ty, ast);
        }
        *T::output(self.ext.transfer) = output;
        self
    }

    pub(super) fn build_funcs(&mut self, funcs: &[(FuncDefAst, FragRef<Func>)], offset: usize) {
        for &(ast, func) in funcs.iter() {
            let Func { signature, .. } = self.ext.types[func];
            match self.builder(signature.ret).build_func(ast, func, offset) {
                Some(Some(body)) => {
                    self.ext.transfer.checked_funcs.push((func, body));
                }
                Some(None) => {
                    self.ext.transfer.extern_funcs.push(func);
                }
                None => (),
            };
        }
    }

    pub(super) fn build_spec(
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
        let frame = self.ctx.start_frame();

        let generics_len = self.ctx.insert_generics(generics, 0);
        self.ctx
            .push_self(TyParamIdx::new(0, generics_len).unwrap().to_ty(), name.span);
        let mut spec_set = SpecSet::default();
        self.generics(generics, &mut spec_set, 0);
        let inherits = self.build_inherits(inherits, &mut spec_set);
        let methods = self.build_spec_methods(spec, body, &mut spec_set, generics_len + 1);
        let generics = self.take_generics(0, generics_len, &mut spec_set);
        self.ext.types[spec] = SpecBase {
            generics,
            methods,
            inherits,
            ..self.ext.types[spec]
        };

        self.ctx.end_frame(frame);
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
        self.ext.types.cache.spec_funcs.extend(methods)
    }

    pub(super) fn build_enum(
        &mut self,
        r#enum: FragRef<Enum>,
        EnumAst { generics, body, .. }: EnumAst,
    ) {
        let frame = self.ctx.start_frame();

        let generics_len = self.ctx.insert_generics(generics, 0);
        let mut spec_set = SpecSet::default();
        self.generics(generics, &mut spec_set, 0);
        let variants = self.enum_variants(body, &mut spec_set);
        let generics = self.take_generics(0, generics_len, &mut spec_set);
        self.ext.types[r#enum] = Enum {
            generics,
            variants,
            ..self.ext.types[r#enum]
        };

        self.ctx.end_frame(frame);
    }

    fn enum_variants(
        &mut self,
        body: Option<ListAst<EnumVariantAst>>,
        spec_set: &mut SpecSet,
    ) -> FragSlice<Variant> {
        let Some(body) = body else {return default()};
        let variants = body
            .iter()
            .filter_map(|EnumVariantAst { name, ty }| {
                let ty = ty.map_or(Some(Ty::UNIT), |(.., ty)| self.ty(ty))?;
                self.ext.types.register_ty_generics(ty, spec_set);
                Some(Variant {
                    name: name.ident,
                    ty,
                    span: name.span,
                })
            })
            .collect::<BumpVec<_>>();
        self.ext.types.cache.variants.extend(variants)
    }

    pub(super) fn build_struct(
        &mut self,
        ty: FragRef<Struct>,
        StructAst { generics, body, .. }: StructAst,
    ) {
        let frame = self.ctx.start_frame();

        let generics_len = self.ctx.insert_generics(generics, 0);
        let mut spec_set = SpecSet::default();
        self.generics(generics, &mut spec_set, 0);
        let fields = self.struct_fields(body, &mut spec_set);
        let generics = self.take_generics(0, generics_len, &mut spec_set);
        self.ext.types[ty] = Struct {
            generics,
            fields,
            ..self.ext.types[ty]
        };

        self.ctx.end_frame(frame);
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
                            self.ext.types.register_ty_generics(ty, spec_set);
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
        self.ext.types.cache.fields.extend(fields)
    }
}
