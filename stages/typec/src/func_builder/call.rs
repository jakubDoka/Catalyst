use crate::context::ArgCountMismatch;

use super::*;

impl<'arena, 'ctx> TirBuilder<'arena, 'ctx> {
    pub fn call(
        &mut self,
        call @ CallAst { callable, .. }: CallAst,
        inference: Inference,
    ) -> ExprRes<'arena> {
        match callable {
            UnitExprAst::Path(path) => {
                let (func, caller, params) = self.func_path(path)?;
                match func {
                    FuncLookupResult::Func(func) => {
                        let caller = caller.map(Ok);
                        self.direct_call(func, params, caller, call, inference)
                    }
                    FuncLookupResult::Var(..) => todo!(),
                    FuncLookupResult::SpecFunc(func, caller) => {
                        self.direct_spec_call(func, params, Ok(caller), call, inference)
                    }
                }
            }
            UnitExprAst::DotExpr(&DotExprAst { lhs, rhs, .. }) => {
                let lhs = self.unit_expr(lhs, Inference::None)?;
                let (func, _, params) = self.method_path(lhs.ty, rhs)?;
                match func {
                    FuncLookupResult::Func(func) => {
                        self.direct_call(func, params, Some(Err(lhs)), call, inference)
                    }
                    FuncLookupResult::SpecFunc(func, ..) => {
                        self.direct_spec_call(func, params, Err(lhs), call, inference)
                    }
                    FuncLookupResult::Var(..) => todo!(),
                }
            }
            kind => {
                todo!("{kind:?}")
            }
        }
    }

    pub fn direct_call(
        &mut self,
        func: FragRef<Func>,
        params: Option<ListAst<TyAst>>,
        caller: Option<Result<Ty, TirNode<'arena>>>,
        call @ CallAst { args, .. }: CallAst,
        inference: Inference,
    ) -> ExprRes<'arena> {
        let Func {
            signature,
            upper_generics,
            owner,
            loc,
            ..
        } = self.ext.typec[func];
        let param_specs = self
            .ext
            .typec
            .pack_func_param_specs(func)
            .collect::<BumpVec<_>>();
        let generic_start = upper_generics.len();

        let mut param_slots = bumpvec![None; param_specs.len()];
        param_slots
            .iter_mut()
            .skip(generic_start)
            .zip(params.iter().flat_map(|params| params.iter()))
            .for_each(|(param_slot, &param)| *param_slot = self.parser().ty(param));

        let (args, params, ty) = self.call_internals(
            param_specs.as_slice(),
            &mut param_slots,
            caller,
            owner,
            args,
            signature,
            inference,
            &|s| loc.map(|loc| loc.source_loc(s.ext.typec, s.ext.resources)),
        )?;

        if func == Func::CAST && let &[to, from] = params {
            self.ctx.cast_check(call.span(), from, to);
        }

        let call_tir = CallTir {
            func: CallableTir::Func(func),
            args,
            params,
        };

        Some(TirNode::new(
            ty,
            TirKind::Call(self.arena.alloc(call_tir)),
            call.span(),
        ))
    }

    pub fn direct_spec_call(
        &mut self,
        func: FragRef<SpecFunc>,
        params: Option<ListAst<TyAst>>,
        caller: Result<Ty, TirNode<'arena>>,
        call @ CallAst { args, .. }: CallAst,
        inference: Inference,
    ) -> ExprRes<'arena> {
        let func_ent @ SpecFunc {
            generics,
            signature,
            ..
        } = self.ext.typec[func];

        let param_specs = self
            .ext
            .typec
            .pack_spec_func_param_specs(func_ent)
            .collect::<BumpVec<_>>();
        let mut param_slots = bumpvec![None; param_specs.len()];
        let generic_start = param_slots.len() - generics.len();
        param_slots
            .iter_mut()
            .skip(generic_start)
            .zip(params.iter().flat_map(|params| params.iter()))
            .for_each(|(param_slot, &param)| *param_slot = self.parser().ty(param));
        param_slots[generic_start - 1] = Some(caller.map_or_else(|e| e.ty, |ty| ty));

        let (args, params, ty) = self.call_internals(
            param_specs.as_slice(),
            &mut param_slots,
            Some(caller),
            None,
            args,
            signature,
            inference,
            &|s| {
                s.ext.typec[func_ent.parent]
                    .loc
                    .map(|loc| loc.source_loc(s.ext.typec, s.ext.resources))
            },
        )?;

        let call_tir = CallTir {
            func: CallableTir::SpecFunc(func),
            args,
            params,
        };

        Some(TirNode::new(
            ty,
            TirKind::Call(self.arena.alloc(call_tir)),
            call.span(),
        ))
    }

    #[inline]
    pub fn unpack_param_slots(
        &mut self,
        params: impl Iterator<Item = Option<Ty>> + Clone + ExactSizeIterator,
        span: Span,

        something: &'static str,
        syntax: &'static str,
    ) -> Option<&'arena [Ty]> {
        if let Some(params) = params.clone().collect::<Option<BumpVec<_>>>() {
            return Some(self.arena.alloc_iter(params));
        }

        let missing = params
            .enumerate()
            .filter_map(|(i, param)| param.is_none().then_some(i))
            .map(|i| format!("#{i}"))
            .intersperse_with(|| ", ".into())
            .collect::<String>();

        UnknownTypeParameters {
            loc: self.meta.loc(span),
            missing,
            something,
            syntax,
        }
        .add(self.ext.workspace)?
    }

    #[allow(clippy::type_complexity)]
    #[allow(clippy::too_many_arguments)]
    pub fn call_internals(
        &mut self,
        generics: &[FragSlice<Spec>],
        param_slots: &mut [Option<Ty>],
        mut caller: Option<Result<Ty, TirNode<'arena>>>,
        owner: Option<Ty>,
        args: ListAst<ExprAst>,
        signature: Signature,
        inference: Inference,

        func: &dyn Fn(&mut Self) -> Option<SourceLoc>,
    ) -> Option<(&'arena [TirNode<'arena>], &'arena [Ty], Ty)> {
        let arg_types = self.ext.typec[signature.args].to_bumpvec();

        if let Some(inference) = inference.ty() {
            // we don't want to report error twice so this one is ignored
            _ = self
                .ext
                .typec
                .compatible(param_slots, inference, signature.ret);
        }

        if let Some(ref mut caller) = caller {
            if let Some(&first) = arg_types.first() && let Err(caller) = caller {
                self.balance_pointers(caller, first);
            }

            if let Some(owner) = owner {
                let ty = caller.map_or_else(|expr| expr.ty, |ty| ty);
                let owner = self.ext.creator().balance_pointers(owner, ty);
                _ = self.ext.typec.compatible(param_slots, ty, owner);
            }
        }

        let expected_arg_count = arg_types.len();
        let actual_arg_count = caller.and_then(|c| c.err()).is_some() as usize + args.len();
        if expected_arg_count != actual_arg_count {
            let func_loc = func(self);
            ArgCountMismatch {
                expected: expected_arg_count,
                actual: actual_arg_count,
                loc: self.meta.loc(args.span()),
                func_loc,
            }
            .add(self.ext.workspace)?;
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
                let instantiated = self.ext.creator().try_instantiate(ty, param_slots);
                let parsed_arg = self.expr(arg, instantiated.into());
                if let Some(parsed_arg) = parsed_arg && instantiated.is_none() {
                    self.infer_params(param_slots, parsed_arg.ty, ty, arg.span());
                }
                parsed_arg
            })
            .nsc_collect::<Option<BumpVec<_>>>()
            .map(|args| caller.and_then(|c| c.err()).into_iter().chain(args))
            .map(|args| self.arena.alloc_iter(args.collect::<BumpVec<_>>()))?;

        let params = self.unpack_param_slots(
            param_slots.iter().copied(),
            args.span(),
            "call",
            "(works on methods too) (<func_path>\\[<ty_param>, ...](<expr_arg>, ...))",
        )?;

        let mut missing_keys = bumpvec![];
        for (&param, &specs) in params.iter().zip(generics) {
            self.ext.creator().implements_sum(
                param,
                specs,
                generics,
                params,
                &mut Some(&mut missing_keys),
            );
        }

        if !missing_keys.is_empty() {
            let missing_impls = missing_keys
                .into_iter()
                .map(|ImplKey { ty, spec }| {
                    format!(
                        "{}: {}",
                        self.ext.creator().display(ty),
                        self.ext.creator().display(spec)
                    )
                })
                .intersperse_with(|| "\n".into())
                .collect::<String>();
            ParamImplMissing {
                loc: self.meta.loc(args.span()),
                missing_impls,
            }
            .add(self.ext.workspace)?;
        }

        let ret = self.ext.creator().instantiate(signature.ret, params);

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
