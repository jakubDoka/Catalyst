use super::*;

impl<'arena, 'ctx> TirBuilder<'arena, 'ctx> {
    // pub fn build_impl_funcs(
    //     &mut self,
    //     arena: &'arena Arena,
    //     transfer: &AstTransfer,
    //     compiled_funcs: &mut BumpVec<(FragRef<Func>, TirFunc<'arena>)>,
    //     extern_funcs: &mut Vec<FragRef<Func>>,
    //     ctx: &mut TirBuilderCtx,
    // ) -> &mut Self {
    //     let iter = iter::once(0)
    //         .chain(transfer.impl_frames.iter().map(|&(.., i)| i))
    //         .zip(transfer.impl_frames.iter().copied());

    //     for (start, (impl_ast, impl_ref, end)) in iter {
    //         let funcs = &transfer.impl_funcs[start..end];

    //         let Some(&(.., first)) = funcs.first() else {
    //             continue;
    //         };

    //         let frame = self.ctx.start_frame();

    //         let Func {
    //             generics, owner, ..
    //         } = self.ext.typec[first];
    //         let offset = self.insert_generics(impl_ast.generics, 0);
    //         self.insert_spec_functions(generics, 0);

    //         if let Some(impl_ref) = impl_ref {
    //             self.build_spec_impl(impl_ref, ctx);
    //         }
    //         if let Some(owner) = owner {
    //             self.scope
    //                 .push(Interner::SELF, owner, impl_ast.target.span());
    //         }
    //         self.build_funcs(arena, funcs, compiled_funcs, extern_funcs, ctx, offset);

    //         self.ctx.end_frame(frame);
    //     }

    //     self
    // }

    // pub fn build_spec_impl(&mut self, impl_ref: FragRef<Impl>, ctx: &mut TirBuilderCtx) {
    //     let Impl {
    //         key: ImplKey { ty, spec },
    //         generics,
    //         loc,
    //         ..
    //     } = self.ext.typec[impl_ref];

    //     let spec_base = spec.base(self.ext.typec);
    //     if SpecBase::is_macro(spec_base) && let Some(mut macro_impl) = self.ext.typec.macros.get_mut(&ty) {
    //         macro_impl.r#impl = Some(impl_ref);
    //         ctx.macros.push(MacroCompileRequest {
    //             name: macro_impl.name,
    //             ty,
    //             r#impl: impl_ref,
    //             params: macro_impl.params,
    //         });
    //     }
    //     let spec_ent = self.ext.typec[spec_base];

    //     // check that all inherits are implemented
    //     // this is necessary for the validation but also grants
    //     // nice optimizations since impl of this spec will imply
    //     // impl of all inherits
    //     {
    //         let params = match spec {
    //             Spec::Base(..) => default(),
    //             Spec::Instance(i) => self.ext.typec[i].args,
    //         };

    //         let missing_impls = self.ext.typec[spec_ent.inherits]
    //             .to_bumpvec()
    //             .into_iter()
    //             .filter_map(|inherit| {
    //                 self.ext.typec
    //                     .find_implementation(ty, inherit, generics, &mut None, self.ext.interner)
    //                     .is_none()
    //                     .then(|| self.ext.creator().instantiate_spec(inherit, params))
    //                     .map(|instance| self.ext.creator().display(instance))
    //             })
    //             .intersperse_with(|| " + ".into())
    //             .collect::<String>();

    //         if !missing_impls.is_empty() {
    //             self.ext.workspace.push(MissingSpecInherits {
    //                 ty: self.ext.creator().display(ty),
    //                 missing: missing_impls,
    //                 loc: loc.source_loc(self.ext.typec, self.ext.resources),
    //             });
    //         }
    //     }
    // }
}
