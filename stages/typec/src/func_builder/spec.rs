use super::*;

impl TyChecker<'_> {
    pub fn build_impl_funcs<'a>(
        &mut self,
        arena: &'a Arena,
        transfer: &AstTransfer,
        compiled_funcs: &mut BumpVec<(FragRef<Func>, TirFunc<'a>)>,
        extern_funcs: &mut Vec<FragRef<Func>>,
        ctx: &mut TirBuilderCtx,
    ) -> &mut Self {
        let iter = iter::once(0)
            .chain(transfer.impl_frames.iter().map(|&(.., i)| i))
            .zip(transfer.impl_frames.iter().copied());

        for (start, (impl_ast, impl_ref, end)) in iter {
            let funcs = &transfer.impl_funcs[start..end];

            let Some(&(.., first)) = funcs.first() else {
                continue;
            };

            let frame = self.scope.start_frame();

            let Func {
                generics, owner, ..
            } = self.typec[first];
            let offset = self.insert_generics(impl_ast.generics, 0);
            self.insert_spec_functions(generics, 0);

            if let Some(impl_ref) = impl_ref {
                self.build_spec_impl(impl_ref, ctx);
            }
            if let Some(owner) = owner {
                self.scope
                    .push(Interner::SELF, owner, impl_ast.target.span());
            }
            self.build_funcs(arena, funcs, compiled_funcs, extern_funcs, ctx, offset);

            self.scope.end_frame(frame);
        }

        self
    }

    pub fn build_spec_impl(&mut self, impl_ref: FragRef<Impl>, ctx: &mut TirBuilderCtx) {
        let Impl {
            key: ImplKey { ty, spec },
            generics,
            loc,
            ..
        } = self.typec[impl_ref];

        let spec_base = spec.base(self.typec);
        if SpecBase::is_macro(spec_base) && let Some(mut macro_impl) = self.typec.macros.get_mut(&ty) {
            macro_impl.r#impl = Some(impl_ref);
            ctx.macros.push(MacroCompileRequest {
                name: macro_impl.name,
                ty,
                r#impl: impl_ref,
                params: macro_impl.params,
            });
        }
        let spec_ent = self.typec[spec_base];

        // check that all inherits are implemented
        // this is necessary for the validation but also grants
        // nice optimizations since impl of this spec will imply
        // impl of all inherits
        {
            let params = match spec {
                Spec::Base(..) => default(),
                Spec::Instance(i) => self.typec[i].args,
            };

            let missing_impls = self.typec[spec_ent.inherits]
                .to_bumpvec()
                .into_iter()
                .filter_map(|inherit| {
                    self.typec
                        .find_implementation(ty, inherit, generics, &mut None, self.interner)
                        .is_none()
                        .then(|| self.typec.instantiate_spec(inherit, params, self.interner))
                        .map(|instance| self.typec.display_spec(instance, self.interner))
                })
                .intersperse_with(|| " + ".into())
                .collect::<String>();

            if !missing_impls.is_empty() {
                self.workspace.push(MissingSpecInherits {
                    ty: self.typec.display_ty(ty, self.interner),
                    missing: missing_impls,
                    loc: loc.source_loc(self.typec, self.resources),
                });
            }
        }
    }

    pub fn check_impl_signature(
        &mut self,
        implementor: Ty,
        spec_func: SpecFunc,
        func_id: FragRef<Func>,
        collect: bool,
    ) -> Result<(), SignatureCheckError> {
        let func = self.typec[func_id];

        let spec_func_params = self
            .typec
            .pack_spec_func_param_specs(spec_func)
            .collect::<BumpVec<_>>();
        let generic_start = spec_func_params.len() - spec_func.generics.len();
        let mut spec_func_slots = vec![None; spec_func_params.len()];
        spec_func_slots[generic_start - 1] = Some(implementor);
        let func_params = self
            .typec
            .pack_func_param_specs(func_id)
            .collect::<BumpVec<_>>();

        match (spec_func.signature.args.len(), func.signature.args.len()) {
            (a, b) if a == b => (),
            (a, b) => return Err(SignatureCheckError::ArgCountMismatch(a, b)),
        }

        match (spec_func.signature.cc, func.signature.cc) {
            (a, b) if a == b => (),
            (a, b) => return Err(SignatureCheckError::CCMismatch(a, b)),
        }

        let mut mismatches = collect.then(|| bumpvec![cap spec_func.signature.args.len() + 1]);
        for (i, (spec_arg, func_arg)) in spec_func
            .signature
            .args
            .keys()
            .zip(func.signature.args.keys())
            .enumerate()
        {
            if let Err((a, b)) = self.typec.compatible(
                &mut spec_func_slots,
                self.typec[func_arg],
                self.typec[spec_arg],
            ) {
                if let Some(ref mut mismatches) = mismatches {
                    mismatches.push((Some(i), a, b));
                } else {
                    return Err(SignatureCheckError::ArgMismatch(default()));
                }
            }
        }

        if let Err((a, b)) = self.typec.compatible(
            &mut spec_func_slots,
            func.signature.ret,
            spec_func.signature.ret,
        ) {
            if let Some(ref mut mismatches) = mismatches {
                mismatches.push((None, a, b));
            } else {
                return Err(SignatureCheckError::ArgMismatch(default()));
            }
        }

        if let Some(mismatches) = mismatches && !mismatches.is_empty() {
            return Err(SignatureCheckError::ArgMismatch(mismatches));
        }

        let Some(spec_func_slots) = spec_func_slots.into_iter().collect::<Option<BumpVec<_>>>() else {
            unimplemented!("hmmm lets see when this happens");
        };

        let mut missing_specs = collect.then(|| bumpvec![]);
        for (specs, &ty) in spec_func_params.into_iter().zip(spec_func_slots.iter()) {
            let implements = self.typec.implements_sum(
                ty,
                specs,
                func_params.as_slice(),
                &spec_func_slots,
                &mut missing_specs.as_mut(),
                self.interner,
            );

            if !collect && !implements {
                return Err(SignatureCheckError::MissingSpecs(default()));
            }
        }

        if let Some(missing_keys) = missing_specs && !missing_keys.is_empty() {
            return Err(SignatureCheckError::MissingSpecs(missing_keys));
        }

        Ok(())
    }

    pub fn handle_signature_check_error(
        &mut self,
        err: SignatureCheckError,
        tested: Span,
        pattern: Span,
        spec_source_loc: Option<SourceLoc>,
    ) {
        let loc = SourceLoc {
            origin: self.source,
            span: tested,
        };
        match err {
            SignatureCheckError::ArgCountMismatch(expected, actual) => {
                self.workspace.push(ImplArgCountMismatch {
                    expected,
                    actual,
                    loc,
                    pattern,
                    spec_source_loc,
                });
            }
            SignatureCheckError::ArgMismatch(args) => {
                let mismatches = args
                    .into_iter()
                    .map(|(i, expected, actual)| {
                        format!(
                            "{}{} {} {}",
                            ["ret", "arg"][i.is_some() as usize],
                            i.map(|i| format!(" {i}")).unwrap_or_default(),
                            self.typec.type_diff(actual, expected, self.interner),
                            self.typec.type_diff(expected, actual, self.interner),
                        )
                    })
                    .intersperse_with(|| "\n".into())
                    .collect::<String>();
                self.workspace.push(DataflowMismatch {
                    mismatches,
                    loc,
                    pattern,
                    spec_source_loc,
                });
            }
            SignatureCheckError::MissingSpecs(mut specs) => {
                specs.sort_unstable();

                let missing = specs
                    .group_by(|a, b| a.ty == b.ty)
                    .map(|g| (g[0].ty, g.iter().map(|s| s.spec)))
                    .map(|(ty, bounds)| {
                        format!(
                            "{}: {}",
                            self.typec.display_ty(ty, self.interner),
                            bounds
                                .map(|b| self.typec.display_spec(b, self.interner))
                                .intersperse_with(|| " + ".into())
                                .collect::<String>(),
                        )
                    })
                    .intersperse_with(|| "\n".into())
                    .collect::<String>();

                self.workspace.push(MissingImplFuncSpecs {
                    missing,
                    loc,
                    pattern,
                    spec_source_loc,
                });
            }
            SignatureCheckError::CCMismatch(expected, actual) => {
                self.workspace.push(ImplCCMismatch {
                    expected: expected.map_or("", |cc| &self.interner[cc]).to_string(),
                    actual: actual.map_or("", |cc| &self.interner[cc]).to_string(),
                    loc,
                    pattern,
                    spec_source_loc,
                });
            }
        }
    }
}

ctl_errors! {
    #[err => "dataflow of impl method does not match spec"]
    #[info => "found mismatches:\n{mismatches}"]
    error DataflowMismatch: fatal {
        #[info spec_source_loc, "spec is defined here"]
        #[info spec_source_loc.map(|loc| loc.origin), pattern, "this function has different dataflow"]
        #[err loc]
        mismatches ref: String,
        loc: SourceLoc,
        pattern: Span,
        spec_source_loc: Option<SourceLoc>,
    }

    #[err => "amount of arguments of impl method does not match spec"]
    #[info => "expected {expected} but got {actual}"]
    error ImplArgCountMismatch: fatal {
        #[info spec_source_loc, "spec is defined here"]
        #[info spec_source_loc.map(|loc| loc.origin), pattern, "this function takes {expected} arguments"]
        #[err loc, "impl method takes {actual} arguments"]
        expected: usize,
        actual: usize,
        loc: SourceLoc,
        pattern: Span,
        spec_source_loc: Option<SourceLoc>,
    }

    #[err => "missing spec inherits"]
    #[info => "'{ty}: {missing}' is not implemented"]
    error MissingSpecInherits: fatal {
        #[err loc]
        ty ref: String,
        missing ref: String,
        loc: SourceLoc,
    }

    #[err => "missing impl function specs"]
    #[info => "missing:\n{missing}"]
    error MissingImplFuncSpecs: fatal {
        #[info spec_source_loc, "spec is defined here"]
        #[info spec_source_loc.map(|loc| loc.origin), pattern, "defined here"]
        #[err loc]
        missing ref: String,
        loc: SourceLoc,
        pattern: Span,
        spec_source_loc: Option<SourceLoc>,
    }

    #[err => "calling convention of impl method does not match spec"]
    #[info => "expected '{expected}' but got '{actual}'"]
    error ImplCCMismatch: fatal {
        #[info spec_source_loc, "spec is defined here"]
        #[info spec_source_loc.map(|loc| loc.origin), pattern, "this function has calling convention '{expected}'"]
        #[err loc, "impl method has calling convention '{actual}'"]
        expected ref: String,
        actual ref: String,
        loc: SourceLoc,
        pattern: Span,
        spec_source_loc: Option<SourceLoc>,
    }

}
