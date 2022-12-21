use super::*;

impl TyChecker<'_> {
    pub fn call<'a>(
        &mut self,
        call @ CallExprAst { callable, .. }: CallExprAst,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        match callable {
            UnitExprAst::Path(path) => {
                let (func, caller, params) = self.func_path(path, builder)?;
                match func {
                    FuncLookupResult::Func(func) => {
                        let caller = caller.map(Ok);
                        self.direct_call(func, params, caller, call, inference, builder)
                    }
                    FuncLookupResult::Var(..) => todo!(),
                    FuncLookupResult::SpecFunc(func, caller) => {
                        self.direct_spec_call(func, params, Ok(caller), call, inference, builder)
                    }
                }
            }
            UnitExprAst::DotExpr(&DotExprAst { lhs, rhs, .. }) => {
                let lhs = self.unit_expr(lhs, Inference::None, builder)?;
                let (func, _, params) = self.method_path(lhs.ty, rhs, builder)?;
                match func {
                    FuncLookupResult::Func(func) => {
                        self.direct_call(func, params, Some(Err(lhs)), call, inference, builder)
                    }
                    FuncLookupResult::SpecFunc(func, ..) => {
                        self.direct_spec_call(func, params, Err(lhs), call, inference, builder)
                    }
                    FuncLookupResult::Var(..) => todo!(),
                }
            }
            kind => {
                todo!("{kind:?}")
            }
        }
    }

    pub fn direct_call<'a>(
        &mut self,
        func: FragRef<Func>,
        params: Option<TyGenericsAst>,
        caller: Option<Result<Ty, TirNode<'a>>>,
        call @ CallExprAst { args, .. }: CallExprAst,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let Func {
            signature,
            upper_generics,
            owner,
            ..
        } = self.typec.funcs[func];

        let param_specs = self
            .typec
            .pack_func_param_specs(func)
            .collect::<BumpVec<_>>();
        let generic_start = upper_generics.len();

        let mut param_slots = bumpvec![None; param_specs.len()];
        param_slots
            .iter_mut()
            .skip(generic_start)
            .zip(params.iter().flat_map(|params| params.iter()))
            .for_each(|(param_slot, &param)| *param_slot = self.ty(param));

        let (args, params, ty) = self.call_internals(
            param_specs.as_slice(),
            &mut param_slots,
            caller,
            owner,
            args,
            signature,
            inference,
            builder,
        )?;

        if func == Func::CAST && let &[from, to] = params {
            builder.ctx.cast_checks.push(CastCheck { loc: call.span(), from, to });
        }

        let call_tir = CallTir {
            func: CallableTir::Func(func),
            args,
            params,
        };

        Some(TirNode::new(
            ty,
            TirKind::Call(builder.arena.alloc(call_tir)),
            call.span(),
        ))
    }

    pub fn direct_spec_call<'a>(
        &mut self,
        func: FragRef<SpecFunc>,
        params: Option<TyGenericsAst>,
        caller: Result<Ty, TirNode<'a>>,
        call @ CallExprAst { args, .. }: CallExprAst,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let func_ent @ SpecFunc {
            generics,
            signature,
            ..
        } = self.typec.spec_funcs[func];

        let param_specs = self
            .typec
            .pack_spec_func_param_specs(func_ent)
            .collect::<BumpVec<_>>();
        let mut param_slots = bumpvec![None; param_specs.len()];
        let generic_start = param_slots.len() - generics.len();
        param_slots
            .iter_mut()
            .skip(generic_start)
            .zip(params.iter().flat_map(|params| params.iter()))
            .for_each(|(param_slot, &param)| *param_slot = self.ty(param));
        param_slots[generic_start - 1] = Some(caller.map_or_else(|e| e.ty, |ty| ty));

        let (args, params, ty) = self.call_internals(
            param_specs.as_slice(),
            &mut param_slots,
            Some(caller),
            None,
            args,
            signature,
            inference,
            builder,
        )?;

        let call_tir = CallTir {
            func: CallableTir::SpecFunc(func),
            args,
            params,
        };

        Some(TirNode::new(
            ty,
            TirKind::Call(builder.arena.alloc(call_tir)),
            call.span(),
        ))
    }

    pub fn unpack_param_slots<'a>(
        &mut self,
        params: impl Iterator<Item = Option<Ty>> + Clone + ExactSizeIterator,
        span: Span,
        builder: &mut TirBuilder<'a, '_>,
        something: &'static str,
        syntax: &'static str,
    ) -> Option<&'a [Ty]> {
        let missing = params
            .clone()
            .enumerate()
            .filter_map(|(i, param)| param.is_none().then_some(i))
            .map(|i| format!("#{i}"))
            .intersperse_with(|| ", ".into())
            .collect::<String>();

        if missing.is_empty() {
            return Some(builder.arena.alloc_iter(
                params.map(|p| p.expect("since missing is empty, there are no None values")),
            ));
        }

        self.workspace.push(UnknownTypeParameters {
            loc: SourceLoc {
                span,
                origin: self.source,
            },
            missing,
            something,
            syntax,
        })?
    }

    #[allow(clippy::type_complexity)]
    #[allow(clippy::too_many_arguments)]
    pub fn call_internals<'a>(
        &mut self,
        generics: &[FragSlice<Spec>],
        param_slots: &mut [Option<Ty>],
        mut caller: Option<Result<Ty, TirNode<'a>>>,
        owner: Option<Ty>,
        args: CallArgsAst,
        signature: Signature,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> Option<(&'a [TirNode<'a>], &'a [Ty], Ty)> {
        let arg_types = self.typec.args[signature.args].to_bumpvec();

        if let Some(inference) = inference.ty() {
            // we don't want to report error twice so this one is ignored
            _ = self.typec.compatible(param_slots, inference, signature.ret);
        }

        if let Some(ref mut caller) = caller {
            if let Some(&first) = arg_types.first() && let Err(caller) = caller {
                self.balance_pointers(caller, first, builder);
            }

            if let Some(owner) = owner {
                let ty = caller.map_or_else(|expr| expr.ty, |ty| ty);
                let owner = self.typec.balance_pointers(owner, ty, self.interner);
                _ = self.typec.compatible(param_slots, ty, owner);
            }
        }

        let parsed_args = args
            .iter()
            .copied()
            .zip(
                arg_types
                    .into_iter()
                    .skip(caller.and_then(|c| c.err()).is_some() as usize),
            )
            .map(|(arg, ty)| {
                let instantiated = self.typec.try_instantiate(ty, param_slots, self.interner);
                let parsed_arg = self.expr(arg, instantiated.into(), builder);
                if let Some(parsed_arg) = parsed_arg && instantiated.is_none() {
                    self.infer_params(param_slots, parsed_arg.ty, ty, arg.span());
                }
                parsed_arg
            })
            .nsc_collect::<Option<BumpVec<_>>>()
            .map(|args| caller.and_then(|c| c.err()).into_iter().chain(args))
            .map(|args| builder.arena.alloc_iter(args.collect::<BumpVec<_>>()))?;

        let params = self.unpack_param_slots(
            param_slots.iter().copied(),
            args.span(),
            builder,
            "call",
            "(works on methods too) (<func_path>\\[<ty_param>, ...](<expr_arg>, ...))",
        )?;

        let mut missing_keys = bumpvec![];
        for (&param, &specs) in params.iter().zip(generics) {
            self.typec.implements_sum(
                param,
                specs,
                generics,
                params,
                &mut Some(&mut missing_keys),
                self.interner,
            );
        }

        if !missing_keys.is_empty() {
            let missing_impls = missing_keys
                .into_iter()
                .map(|ImplKey { ty, spec }| {
                    format!(
                        "{}: {}",
                        self.typec.display_ty(ty, self.interner),
                        self.typec.display_spec(spec, self.interner)
                    )
                })
                .intersperse_with(|| "\n".into())
                .collect::<String>();
            self.workspace.push(ParamImplMissing {
                loc: SourceLoc {
                    span: args.span(),
                    origin: self.source,
                },
                missing_impls,
            })?;
        }

        let ret = self.typec.instantiate(signature.ret, params, self.interner);

        Some((parsed_args, params, ret))
    }
}

ctl_errors! {
    #[err => "not all {something} parameters can be inferred"]
    #[info => "missing parameters: {missing}"]
    #[help => "provide the parameters {syntax}"]
    error UnknownTypeParameters: fatal {
        #[err loc]
        missing ref: String,
        loc: SourceLoc,
        something: &'static str,
        syntax: &'static str,
    }

    #[err => "not all parameter specs are satisfied"]
    #[info => "missing implementations:\n {missing_impls}"]
    error ParamImplMissing: fatal {
        #[err loc]
        missing_impls ref: String,
        loc: SourceLoc,
    }
}
