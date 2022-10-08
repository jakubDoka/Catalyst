use std::{cmp::Ordering, default::default, iter, vec};

use diags::*;
use lexing_t::*;
use packaging_t::*;
use parsing::*;
use parsing_t::*;

use storage::*;

use typec_t::*;

use crate::{ty_parser::TyPathResult, *};

pub type ExprRes<'a> = Option<TirNode<'a>>;

impl TyChecker<'_> {
    pub fn build_impl_funcs<'a>(
        &mut self,
        arena: &'a Arena,
        transfer: &AstTransfer,
        compiled_funcs: &mut Vec<(VRef<Func>, TirNode<'a>)>,
        extern_funcs: &mut Vec<VRef<Func>>,
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

            let offset = impl_ast.generics.len();
            let Func { generics, .. } = self.typec.funcs[first];
            self.insert_generics(impl_ast.generics, 0);
            self.insert_spec_functions(generics, 0);

            if let Some(impl_ref) = impl_ref {
                self.build_spec_impl(arena, impl_ref, funcs, compiled_funcs, offset);
            } else {
                self.build_funcs(arena, funcs, compiled_funcs, extern_funcs, offset);
            }

            self.scope.end_frame(frame);
        }

        self
    }

    fn build_spec_impl<'a>(
        &mut self,
        arena: &'a Arena,
        impl_ref: VRef<Impl>,
        input: &[(FuncDefAst, VRef<Func>)],
        compiled_funcs: &mut Vec<(VRef<Func>, TirNode<'a>)>,
        offset: usize,
    ) {
        let Impl {
            key: ImplKey { ty, spec },
            span,
            ..
        } = self.typec.impls[impl_ref];
        let spec_base = spec.base(self.typec);
        let spec_ent = self.typec[spec_base];
        let spec_methods = self.typec.spec_funcs[spec_ent.methods].to_bumpvec();

        let mut methods = bumpvec![None; spec_methods.len()];
        for &(ast, func) in input {
            let Some(func_res) = self.build_func(ast, func, arena, offset) else { continue; };
            let Some(body) = func_res else {
                self.extern_in_impl(ast.span);
                continue;
            };
            compiled_funcs.push((func, body));

            let Func { name, .. } = self.typec.funcs[func];
            let Some((i, spec_func)) = spec_methods
                .iter()
                .copied()
                .enumerate()
                .find(|&(_, SpecFunc { name: n, .. })| n == name)
            else {
                let left_to_implement = spec_methods
                    .iter()
                    .zip(methods.as_slice())
                    .filter_map(|(f, m)| m.is_none().then_some(f.name))
                    .collect::<BumpVec<_>>();
                self.unknown_spec_impl_func(ast.signature.name.span, left_to_implement.as_slice());
                continue;
            };

            self.check_impl_signature(ty, spec_func, func, ast.signature.span());

            methods[i] = Some(func);
        }

        let Some(methods) = methods.iter().copied().collect::<Option<BumpVec<_>>>() else {
            let missing = spec_methods
                .iter()
                .zip(methods.as_slice())
                .filter_map(|(f, m)| m.is_none().then_some(f.name))
                .collect::<BumpVec<_>>();
            self.missing_spec_impl_funcs(span, missing.as_slice());
            return;
        };

        self.typec.impls[impl_ref].methods = self.typec.func_slices.bump(methods);
    }

    pub fn check_impl_signature(
        &mut self,
        implementor: Ty,
        spec_func: SpecFunc,
        func_id: VRef<Func>,
        span: Span,
    ) {
        let func = self.typec.funcs[func_id];

        let spec_func_params = self
            .typec
            .pack_spec_func_param_specs(spec_func)
            .collect::<BumpVec<_>>();
        let generic_start = spec_func_params.len() - self.typec[spec_func.generics].len();
        let mut spec_func_slots = vec![None; spec_func_params.len()];
        spec_func_slots[generic_start - 1] = Some(implementor);
        let func_params = self
            .typec
            .pack_func_param_specs(func_id)
            .collect::<BumpVec<_>>();

        let spec_args = self.typec.args[spec_func.signature.args].to_bumpvec();
        let func_args = self.typec.args[func.signature.args].to_bumpvec();

        if spec_args.len() != func_args.len() {
            self.spec_arg_count_mismatch(span, spec_args.len(), func_args.len());
            return;
        }

        let mut faulty = false;
        for (i, (spec_arg, func_arg)) in spec_args.into_iter().zip(func_args).enumerate() {
            if let Err((a, b)) = self
                .typec
                .compatible(&mut spec_func_slots, func_arg, spec_arg)
            {
                self.spec_arg_mismatch(span, i, a, b);
                faulty = true;
            }
        }

        if let Err((a, b)) = self.typec.compatible(
            &mut spec_func_slots,
            func.signature.ret,
            spec_func.signature.ret,
        ) {
            self.spec_ret_mismatch(span, a, b);
            faulty = true;
        }

        if faulty {
            return;
        }

        let Some(spec_func_slots) = spec_func_slots.into_iter().collect::<Option<BumpVec<_>>>() else {
            unimplemented!("hmmm lets see when this happens");
        };

        let mut missing_keys = bumpvec![];
        for (specs, ty) in spec_func_params.into_iter().zip(spec_func_slots) {
            self.typec.implements_sum(
                ty,
                specs,
                func_params.as_slice(),
                &mut Some(&mut missing_keys),
            );
        }

        for ImplKey { ty, spec } in missing_keys {
            self.missing_spec(ty, spec, span);
        }
    }

    pub fn build_funcs<'a>(
        &mut self,
        arena: &'a Arena,
        input: &[(FuncDefAst, VRef<Func>)],
        compiled_funcs: &mut Vec<(VRef<Func>, TirNode<'a>)>,
        extern_funcs: &mut Vec<VRef<Func>>,
        offset: usize,
    ) -> &mut Self {
        let iter = input.iter().filter_map(|&(ast, func)| {
            let res = self.build_func(ast, func, arena, offset)?;

            let Some(body) = res else {
                extern_funcs.push(func);
                return None;
            };

            Some((func, body))
        });

        compiled_funcs.extend(iter);

        self
    }

    pub fn build_func<'a>(
        &mut self,
        FuncDefAst {
            signature:
                FuncSigAst {
                    generics,
                    args,
                    ret,
                    ..
                },
            body,
            ..
        }: FuncDefAst,
        func: VRef<Func>,
        arena: &'a Arena,
        offset: usize,
    ) -> Option<Option<TirNode<'a>>> {
        let frame = self.scope.start_frame();
        let Func {
            signature,
            generics: self_generics,
            ..
        } = self.typec.funcs[func];

        let mut builder = TirBuilder::new(
            arena,
            signature.ret,
            ret.map(|ret| ret.span()),
            self.typec.pack_func_param_specs(func).collect::<Vec<_>>(),
        );

        self.insert_generics(generics, offset);
        self.insert_spec_functions(self_generics, offset);
        self.args(signature.args, args, &mut builder);

        let tir_body = match body {
            FuncBodyAst::Arrow(.., expr) => self.expr(expr, Some(signature.ret), &mut builder),
            FuncBodyAst::Block(body) => self.block(body, Some(signature.ret), &mut builder),
            FuncBodyAst::Extern(..) => return Some(None),
        }?;

        self.scope.end_frame(frame);

        Some(if tir_body.ty == Ty::TERMINAL {
            Some(tir_body)
        } else {
            self.return_low(Some(tir_body), body.span(), &mut builder)
        })
    }

    fn block<'a>(
        &mut self,
        block: BlockAst,
        inference: Inference,
        builder: &mut TirBuilder<'a>,
    ) -> ExprRes<'a> {
        let frame = builder.start_frame();
        let scope_frame = self.scope.start_frame();

        let Some((last, other)) = block.elements.split_last() else {
            return Some(TirNode::new(Ty::UNIT, TirKind::Block(&[]), block.span()))
        };

        let mut store = bumpvec![cap block.len()];
        store.extend(
            other
                .iter()
                .filter_map(|expr| self.expr(expr.value, None, builder)),
        );
        let last = self.expr(last.value, inference, builder);
        builder.end_frame(frame);
        let last = last?;
        store.push(last);

        self.scope.end_frame(scope_frame);

        let nodes = builder.arena.alloc_slice(&store);
        Some(TirNode::new(last.ty, TirKind::Block(nodes), block.span()))
    }

    fn r#return<'a>(
        &mut self,
        expr: Option<ExprAst>,
        span: Span,
        builder: &mut TirBuilder<'a>,
    ) -> ExprRes<'a> {
        if let Some((span, ..)) = builder.runner {
            self.control_flow_in_const(span, span);
        }

        let value = if let Some(expr) = expr {
            Some(self.expr(expr, Some(builder.ret), builder).unwrap())
        } else {
            None
        };

        let span = expr.map_or(span, |expr| span.joined(expr.span()));

        self.return_low(value, span, builder)
    }

    fn return_low<'a>(
        &mut self,
        value: ExprRes<'a>,
        span: Span,
        builder: &mut TirBuilder<'a>,
    ) -> ExprRes<'a> {
        self.type_check(builder.ret, value.map_or(Ty::UNIT, |value| value.ty), span)?;

        Some(TirNode::new(
            Ty::TERMINAL,
            TirKind::Return(value.map(|v| builder.arena.alloc(v))),
            span,
        ))
    }

    fn expr<'a>(
        &mut self,
        expr: ExprAst,
        inference: Inference,
        builder: &mut TirBuilder<'a>,
    ) -> ExprRes<'a> {
        let value = match expr {
            ExprAst::Unit(&unit) => self.unit_expr(unit, inference, builder),
            ExprAst::Binary(&binary) => self.binary_expr(binary, builder),
        }?;

        if let Some(ty) = inference {
            self.type_check(ty, value.ty, expr.span())?;
        }

        Some(value)
    }

    fn unit_expr<'a>(
        &mut self,
        unit_ast: UnitExprAst,
        inference: Inference,
        builder: &mut TirBuilder<'a>,
    ) -> ExprRes<'a> {
        match unit_ast {
            UnitExprAst::Path(path) => self.value_path(path, inference, builder),
            UnitExprAst::Return(ReturnExprAst { return_span, expr }) => {
                self.r#return(expr, return_span, builder)
            }
            UnitExprAst::Int(span) => self.int(span, inference),
            UnitExprAst::Char(span) => self.char(span),
            UnitExprAst::Call(&call) => self.call(call, inference, builder),
            UnitExprAst::Const(run) => self.const_expr(run, inference, builder),
            UnitExprAst::StructConstructor(struct_constructor) => {
                self.struct_constructor(struct_constructor, inference, builder)
            }
            UnitExprAst::DotExpr(_) => todo!(),
            UnitExprAst::PathInstance(_) => todo!(),
            UnitExprAst::TypedPath(_) => todo!(),
        }
    }

    fn struct_constructor<'a>(
        &mut self,
        ctor @ StructConstructorAst { path, body, .. }: StructConstructorAst,
        inference: Inference,
        builder: &mut TirBuilder<'a>,
    ) -> ExprRes<'a> {
        let (ty, params) = if let Some(PathInstanceAst { path, params }) = path {
            let ty = self.ty_path(path)?;
            (
                match ty {
                    TyPathResult::Ty(ty) => ty.as_generic().or_else(|| todo!())?,
                    _ => todo!(),
                },
                params.and_then(|(_, params)| {
                    params
                        .iter()
                        .map(|&param| self.ty(param))
                        .nsc_collect::<Option<BumpVec<_>>>()
                }),
            )
        } else {
            let ty = inference.or_else(|| self.cannot_infer(ctor.span())?)?;
            match ty {
                Ty::Instance(instance) => (
                    self.typec[instance].base,
                    Some(self.typec[self.typec[instance].args].to_bumpvec()),
                ),
                _ => (ty.as_generic().or_else(|| todo!())?, None),
            }
        };

        let struct_meta = match ty {
            GenericTy::Struct(struct_meta) => self.typec[struct_meta],
        };

        let mut param_slots = bumpvec![None; self.typec[struct_meta.generics].len()];

        if let Some(params) = params {
            if let Some(PathInstanceAst { params: Some((.., params)), .. }) = path
                && params.len() > param_slots.len()
            {
                self.too_many_params(params, param_slots.len())?;
            }

            param_slots
                .iter_mut()
                .zip(params.iter().copied())
                .for_each(|(slot, param)| *slot = Some(param))
        }

        let mut fields = bumpvec![None; body.len()];

        for field_ast @ &StructConstructorFieldAst { name, expr } in body.iter() {
            let (index, field) = self.typec.fields[struct_meta.fields]
                .iter()
                .copied()
                .enumerate()
                .find(|(.., field)| field.name == name.ident)
                .or_else(|| self.unknown_field(ty.as_ty(), struct_meta.fields, ctor.span())?)?;

            let inference = self
                .typec
                .try_instantiate(field.ty, &param_slots, self.interner);
            let Some(value) = self.expr(expr.unwrap_or_else(|| todo!()), inference, builder) else {
                continue;
            };

            self.infer_params(&mut param_slots, value.ty, field.ty, field_ast.span());

            if fields[index].replace(value).is_some() {
                self.duplicate_field(name.span());
            }
        }

        let Some(params) = param_slots.into_iter().collect::<Option<BumpVec<_>>>() else {
            todo!()
        };

        let Some(fields) = fields.into_iter().collect::<Option<BumpVec<_>>>() else {
            todo!()
        };

        let final_ty = if params.is_empty() {
            ty.as_ty()
        } else {
            Ty::Instance(self.typec.instance(ty, &params, self.interner))
        };

        Some(TirNode::new(
            final_ty,
            TirKind::Constructor(builder.arena.alloc_iter(fields)),
            ctor.span(),
        ))
    }

    fn const_expr<'a>(
        &mut self,
        run: ConstAst,
        inference: Inference,
        builder: &mut TirBuilder<'a>,
    ) -> ExprRes<'a> {
        if let Some((runner, ..)) = builder.runner {
            self.nested_runner(runner, run.span())?
        }

        let frame = builder.start_frame();
        builder.runner = Some((run.r#const, frame));
        let expr = self.expr(run.value, inference, builder);
        let (.., frame) = builder
            .runner
            .take()
            .expect("runner should be present since nesting is impossible");
        builder.end_frame(frame);

        let expr = builder.arena.alloc(expr?);

        Some(TirNode::new(expr.ty, TirKind::Const(expr), expr.span))
    }

    fn call<'a>(
        &mut self,
        call @ CallExprAst { callable, .. }: CallExprAst,
        inference: Inference,
        builder: &mut TirBuilder<'a>,
    ) -> ExprRes<'a> {
        match callable {
            UnitExprAst::Path(path) => {
                let func = self.func_path(path, builder)?;
                match func {
                    FuncLookupResult::Func(func) => {
                        self.direct_call(func, iter::empty(), None, call, inference, builder)
                    }
                    FuncLookupResult::Var(..) => todo!(),
                    FuncLookupResult::SpecFunc(func, caller) => self.direct_spec_call(
                        func,
                        iter::empty(),
                        Ok(caller),
                        call,
                        inference,
                        builder,
                    ),
                }
            }
            UnitExprAst::PathInstance(path) => {
                let func = self.func_path(path.path, builder)?;
                let params = path.params();
                match func {
                    FuncLookupResult::Func(func) => {
                        self.direct_call(func, params, None, call, inference, builder)
                    }
                    FuncLookupResult::Var(..) => todo!(),
                    FuncLookupResult::SpecFunc(func, caller) => {
                        self.direct_spec_call(func, params, Ok(caller), call, inference, builder)
                    }
                }
            }
            UnitExprAst::TypedPath(TypedPathAst { ty, path, .. }) => {
                let ty = self.ty(ty)?;
                let func = self.method_path(ty, path.path, builder)?;
                let params = path.params();
                match func {
                    FuncLookupResult::Func(func) => {
                        self.direct_call(func, params, None, call, inference, builder)
                    }
                    FuncLookupResult::SpecFunc(func, ..) => {
                        self.direct_spec_call(func, params, Ok(ty), call, inference, builder)
                    }
                    FuncLookupResult::Var(..) => todo!(),
                }
            }
            UnitExprAst::DotExpr(&DotExprAst { lhs, rhs, .. }) => {
                let lhs = self.unit_expr(lhs, None, builder)?;
                let func = self.method_path(lhs.ty, rhs.path, builder)?;
                let params = rhs.params();
                match func {
                    FuncLookupResult::Func(func) => {
                        self.direct_call(func, params, Some(lhs), call, inference, builder)
                    }
                    FuncLookupResult::SpecFunc(func, ..) => {
                        self.direct_spec_call(func, params, Err(lhs), call, inference, builder)
                    }
                    FuncLookupResult::Var(..) => todo!(),
                }
            }
            kind @ (UnitExprAst::Return(..)
            | UnitExprAst::Call(..)
            | UnitExprAst::Int(..)
            | UnitExprAst::Char(..)
            | UnitExprAst::StructConstructor(..)
            | UnitExprAst::Const(..)) => {
                todo!("{kind:?}")
            }
        }
    }

    fn direct_call<'a>(
        &mut self,
        func: VRef<Func>,
        params: impl Iterator<Item = TyAst>,
        caller: Option<TirNode<'a>>,
        call @ CallExprAst { args, .. }: CallExprAst,
        inference: Inference,
        builder: &mut TirBuilder<'a>,
    ) -> ExprRes<'a> {
        let Func {
            signature,
            upper_generics,
            ..
        } = self.typec.funcs[func];

        let param_specs = self
            .typec
            .pack_func_param_specs(func)
            .collect::<BumpVec<_>>();
        let generic_start = self.typec[upper_generics].len();

        let mut param_slots = bumpvec![None; param_specs.len()];
        param_slots
            .iter_mut()
            .skip(generic_start)
            .zip(params)
            .for_each(|(param_slot, param)| *param_slot = self.ty(param));

        let (args, params, ty) = self.call_internals(
            param_specs.as_slice(),
            &mut param_slots,
            caller,
            args,
            signature,
            inference,
            builder,
        )?;

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

    fn direct_spec_call<'a>(
        &mut self,
        func: VRef<SpecFunc>,
        params: impl Iterator<Item = TyAst>,
        caller: Result<Ty, TirNode<'a>>,
        call @ CallExprAst { args, .. }: CallExprAst,
        inference: Inference,
        builder: &mut TirBuilder<'a>,
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
        let generic_start = param_slots.len() - self.typec[generics].len();
        param_slots
            .iter_mut()
            .skip(generic_start)
            .zip(params)
            .for_each(|(param_slot, param)| *param_slot = self.ty(param));
        param_slots[generic_start - 1] = Some(caller.map_or_else(|e| e.ty, |ty| ty));

        let (args, params, ty) = self.call_internals(
            param_specs.as_slice(),
            &mut param_slots,
            caller.err(),
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

    fn call_params<'a>(
        &mut self,
        params: impl Iterator<Item = Option<Ty>> + Clone,
        span: Span,
        builder: &mut TirBuilder<'a>,
    ) -> Option<&'a [Ty]> {
        let Some(params) = params.clone().collect::<Option<BumpVec<_>>>() else {
            let missing = params
                .enumerate()
                .filter_map(|(i, ty)| ty.is_none().then_some(i));

            for missing in missing {
                self.cannot_infer_param(span, missing);
            }

            return None;
        };

        Some(builder.arena.alloc_iter(params))
    }

    #[allow(clippy::type_complexity)]
    #[allow(clippy::too_many_arguments)]
    fn call_internals<'a>(
        &mut self,
        generics: &[VSlice<Spec>],
        param_slots: &mut [Option<Ty>],
        mut caller: Option<TirNode<'a>>,
        args: CallArgsAst,
        signature: Signature,
        inference: Inference,
        builder: &mut TirBuilder<'a>,
    ) -> Option<(&'a [TirNode<'a>], &'a [Ty], Ty)> {
        let arg_types = self.typec.args[signature.args].to_bumpvec();

        if let Some(inference) = inference {
            // we don't want to report error twice so this one is ignored
            let _ = self.typec.compatible(param_slots, inference, signature.ret);
        }

        if let Some(ref mut caller) = caller && let Some(&first) = arg_types.first() {
            // TODO: do pointer correction
            self.balance_pointers(caller, first, builder);
            self.infer_params(param_slots, caller.ty, first, caller.span);
        }

        let parsed_args = args
            .iter()
            .copied()
            .zip(arg_types.into_iter().skip(caller.is_some() as usize))
            .map(|(arg, ty)| {
                let instantiated = self.typec.try_instantiate(ty, param_slots, self.interner);
                let parsed_arg = self.expr(arg, instantiated, builder);
                if let Some(parsed_arg) = parsed_arg && instantiated.is_none() {
                    self.infer_params(param_slots, parsed_arg.ty, ty, arg.span());
                }
                parsed_arg
            })
            .nsc_collect::<Option<BumpVec<_>>>()
            .map(|args| caller.into_iter().chain(args))
            .map(|args| builder.arena.alloc_iter(args.collect::<BumpVec<_>>()))?;

        let params = self.call_params(param_slots.iter().copied(), args.span(), builder)?;

        let mut missing_keys = bumpvec![];
        for (&param, &specs) in params.iter().zip(generics) {
            self.typec
                .implements_sum(param, specs, generics, &mut Some(&mut missing_keys));
        }

        for ImplKey { spec, ty } in missing_keys {
            self.missing_spec(ty, spec, args.span());
        }

        let ret = self.typec.instantiate(signature.ret, params, self.interner);

        Some((parsed_args, params, ret))
    }

    fn balance_pointers<'a>(
        &mut self,
        node: &mut TirNode<'a>,
        ty: Ty,
        builder: &mut TirBuilder<'a>,
    ) -> Option<()> {
        let (desired_pointer_depth, mutability) = match ty {
            Ty::Pointer(ptr) => (self.typec[ptr].depth, self.typec[ptr].mutability),
            _ => (0, Mutability::Immutable),
        };
        loop {
            let current_pointed_depth = node.ty.ptr_depth(self.typec);
            match desired_pointer_depth.cmp(&current_pointed_depth) {
                Ordering::Less => {
                    let ty = self.typec.deref(node.ty);
                    *node = TirNode::new(ty, TirKind::Deref(builder.arena.alloc(*node)), node.span);
                }
                Ordering::Greater => {
                    let ty = self.typec.pointer_to(mutability, node.ty, self.interner);
                    *node = TirNode::new(
                        Ty::Pointer(ty),
                        TirKind::Ref(builder.arena.alloc(*node)),
                        node.span,
                    );
                }
                Ordering::Equal => break,
            }
        }

        Some(())
    }

    fn infer_params(
        &mut self,
        params: &mut [Option<Ty>],
        reference: Ty,
        template: Ty,
        span: Span,
    ) -> Option<()> {
        self.typec
            .compatible(params, reference, template)
            .map_err(|(r, t)| self.generic_ty_mismatch(r, t, span))
            .ok()
    }

    fn func_path<'a>(
        &mut self,
        path @ PathExprAst { start, segments }: PathExprAst,
        builder: &mut TirBuilder<'a>,
    ) -> Option<FuncLookupResult<'a>> {
        let module = match self.lookup(start.ident, start.span, FUNC_OR_MOD)? {
            ScopeItem::Func(func) => return Some(FuncLookupResult::Func(func)),
            ScopeItem::Module(module) => module,
            ScopeItem::Ty(ty) => {
                let (&start, segments) = segments
                    .split_first()
                    .or_else(|| self.invalid_expr_path(path.span())?)?;
                return self.method_path(ty, PathExprAst { start, segments }, builder);
            }
            item => self.invalid_symbol_type(item, start.span, FUNC_OR_MOD)?,
        };

        let (&func_or_type, segments) = segments
            .split_first()
            .or_else(|| self.invalid_expr_path(path.span())?)?;

        let id = self
            .interner
            .intern_scoped(module.as_u32(), func_or_type.ident);
        let ty = match self.lookup(id, func_or_type.span, FUNC)? {
            ScopeItem::Func(func) => return Some(FuncLookupResult::Func(func)),
            ScopeItem::Ty(ty) => ty,
            item => self.invalid_symbol_type(item, func_or_type.span, FUNC)?,
        };

        let (&start, segments) = segments
            .split_first()
            .or_else(|| self.invalid_expr_path(path.span())?)?;

        self.method_path(ty, PathExprAst { start, segments }, builder)
    }

    fn method_path<'a>(
        &mut self,
        ty: Ty,
        path @ PathExprAst { start, segments }: PathExprAst,
        _builder: &mut TirBuilder<'a>,
    ) -> Option<FuncLookupResult<'a>> {
        let ty = ty.caller(self.typec);
        match *segments {
            [] => {
                let id = self.interner.intern_scoped(ty, start.ident);
                match self.lookup(id, path.span(), FUNC)? {
                    ScopeItem::Func(func) => Some(FuncLookupResult::Func(func)),
                    ScopeItem::SpecFunc(func) => Some(FuncLookupResult::SpecFunc(func, ty)),
                    item => self.invalid_symbol_type(item, path.span(), FUNC)?,
                }
            }
            [name] => {
                let id = self.interner.intern_scoped(ty, name.ident);
                let module = lookup!(Module self, start.ident, start.span);
                let method_id = self.interner.intern_scoped(module.index(), id);
                match self.lookup(method_id, path.span(), FUNC)? {
                    ScopeItem::Func(func) => Some(FuncLookupResult::Func(func)),
                    item => self.invalid_symbol_type(item, path.span(), FUNC)?,
                }
            }
            _ => self.invalid_expr_path(path.span())?,
        }
    }

    fn value_path<'a>(
        &mut self,
        path @ PathExprAst { start, .. }: PathExprAst,
        _inference: Inference,
        builder: &mut TirBuilder<'a>,
    ) -> ExprRes<'a> {
        let var = lookup!(Var self, start.ident, start.span);
        Some(TirNode::new(
            builder.get_var(var).ty,
            TirKind::Access(var),
            path.span(),
        ))
    }

    fn int<'a>(&mut self, span: Span, inference: Inference) -> ExprRes<'a> {
        let span_str = span_str!(self, span);
        let (ty, postfix_len) = Ty::INTEGERS
            .iter()
            .map(|&ty| {
                (
                    ty,
                    match ty {
                        Ty::Builtin(b) => b.name(),
                        _ => unreachable!(),
                    },
                )
            })
            .find_map(|(ty, str)| span_str.ends_with(str).then_some((ty, str.len())))
            .or_else(|| {
                inference
                    .filter(|ty| Ty::INTEGERS.contains(ty))
                    .map(|ty| (ty, 0))
            })
            .unwrap_or((Ty::UINT, 0));
        Some(TirNode::new(
            ty,
            TirKind::Int,
            span.sliced(..span_str.len() - postfix_len),
        ))
    }

    fn char<'a>(&mut self, span: Span) -> ExprRes<'a> {
        Some(TirNode::new(Ty::CHAR, TirKind::Char, span))
    }

    fn binary_expr<'a>(
        &mut self,
        binary_ast @ BinaryExprAst { lhs, op, rhs }: BinaryExprAst,
        builder: &mut TirBuilder<'a>,
    ) -> ExprRes<'a> {
        let lhs = self.expr(lhs, None, builder);
        let rhs = self.expr(rhs, None, builder);
        let (lhs, rhs) = (lhs?, rhs?); // recovery

        let base_id = match *op.segments {
            [] => op.start.ident,
            [name] => {
                let module = lookup!(Module self, name.ident, name.span);
                self.interner.intern_scoped(module.index(), op.start.ident)
            }
            _ => self.invalid_op_expr_path(op.span())?,
        };

        let id = self
            .interner
            .intern_with(|s, t| self.typec.binary_op_id(base_id, lhs.ty, rhs.ty, t, s));
        let func = lookup!(Func self, id, op.span());

        let ty = self.typec.funcs[func].signature.ret;
        let call = CallTir {
            func: CallableTir::Func(func),
            params: default(),
            args: builder.arena.alloc_slice(&[lhs, rhs]),
        };
        Some(TirNode::new(
            ty,
            TirKind::Call(builder.arena.alloc(call)),
            binary_ast.span(),
        ))
    }

    fn args(&mut self, types: VSlice<Ty>, args: FuncArgsAst, builder: &mut TirBuilder) {
        for (&ty, &arg) in self.typec.args[types].iter().zip(args.iter()) {
            let var = builder.create_var(TirKind::Var(None), ty, arg.name.span);
            self.scope.push(arg.name.ident, var, arg.name.span);
        }
    }

    fn type_check(&mut self, expected: Ty, got: Ty, span: Span) -> Option<()> {
        self.type_check_detailed(expected, got, |s| {
            s.generic_ty_mismatch(expected, got, span)
        })
    }

    fn type_check_detailed<A>(
        &mut self,
        expected: Ty,
        got: Ty,
        display: impl Fn(&mut Self) -> A,
    ) -> Option<()> {
        if Ty::compatible(expected, got) {
            return Some(());
        }

        display(self);

        None
    }

    gen_error_fns! {
        push unreachable_expr(self, span: Span, because: Span) {
            warn: "unreachable expression";
            (span, self.source) {
                info[span]: "this is unreachable";
                info[because]: "because of this";
            }
        }

        push incomplete_tir(self, func: FuncDefAst) {
            err: "not all blocks were closed when typechecking function";
            info: "this is a bug in the compiler, please report it";
            (func.span(), self.source) {
                info[func.signature.name.span]: "happened in this function";
            }
        }

        push generic_ty_mismatch(self, expected: Ty, got: Ty, span: Span) {
            err: "type mismatch";
            info: (
                "expected '{}' but got '{}'",
                self.typec.type_diff(expected, got, self.interner),
                self.typec.type_diff(got, expected, self.interner),
            );
            (span, self.source) {
                err[span]: "mismatch occurred here";
            }
        }

        push invalid_expr_path(self, span: Span) {
            err: "invalid expression path";
            info: "expected format: <ident> |";
            (span, self.source) {
                err[span]: "found here";
            }
        }

        push invalid_op_expr_path(self, span: Span) {
            err: "invalid operator expression path";
            info: "expected format: <op> | <op>\\<module>";
            (span, self.source) {
                err[span]: "found here";
            }
        }

        push nested_runner(self, previous: Span, current: Span) {
            err: "'const' cannot be directly nested";
            help: "removing 'const' should result in equivalent code";
            (previous.joined(current), self.source) {
                err[current]: "nesting happens here";
                info[previous]: "operation is already performed at compile time because of this";
            }
        }

        push const_runtime_access(self, r#const: Span, value: Span) {
            err: "cannot access runtime value in 'const'";
            help: "try moving the access to a non-const function";
            help: "or declaring the variable as constant";
            (r#const.joined(value), self.source) {
                err[r#const]: "this is what makes access const";
                info[value]: "this is a runtime value, outsize of 'const' context";
            }
        }

        push control_flow_in_const(self, r#const: Span, control_flow: Span) {
            err: ("cannot '{}' in 'const' context", span_str!(self, control_flow));
            info: "jump produced by this call would cross const/runtime boundary";
            (r#const.joined(control_flow), self.source) {
                err[r#const]: "this is what defines const context";
                info[control_flow]: "this is the control flow keyword that is not allowed in const context";
            }
        }

        push too_many_params(self, params: TyGenericsAst, max: usize) {
            err: "too many type parameters";
            info: ("expected at most {} parameters", max);
            (params.span(), self.source) {
                err[params.span()]: "found here";
            }
        }

        push missing_spec(self, ty: Ty, spec: Spec, span: Span) {
            err: (
                "'{}' does not implement '{}'",
                self.typec.display_ty(ty, self.interner),
                self.typec.display_spec(spec, self.interner),
            );
            (span, self.source) {
                err[span]: "when calling this";
            }
        }

        push cannot_infer(self, span: Span) {
            err: "cannot infer type";
            (span, self.source) {
                err[span]: "when type checking this";
            }
        }

        push cannot_infer_param(self, span: Span, index: usize) {
            err: ("cannot infer type of parameters[{}]", index);
            (span, self.source) {
                err[span]: "while instantiating this call";
            }
        }

        push expected_struct(self, ty: Ty, span: Span) {
            err: "expected struct type";
            info: ("found '{}'", self.typec.display_ty(ty, self.interner));
            (span, self.source) {
                err[span]: "when type checking this";
            }
        }

        push unknown_field(self, ty: Ty, fields: VSlice<Field>, span: Span) {
            err: ("unknown field");
            info: (
                "available fields in '{}': {}",
                self.typec.display_ty(ty, self.interner),
                self.typec.fields[fields]
                    .iter()
                    .map(|f| &self.interner[f.name])
                    .intersperse(", ")
                    .collect::<String>(),
            );
            (span, self.source) {
                err[span]: "occurred here";
            }
        }

        push duplicate_field(self, span: Span) {
            err: "duplicate field";
            (span, self.source) {
                err[span]: "this was already initialized";
            }
        }

        push unknown_spec_impl_func(self, func_span: Span, left: &[VRef<str>]) {
            err: "unknown spec function";
            help: (
                "functions that can be implemented: {}",
                left.iter()
                    .map(|&f| &self.interner[f])
                    .intersperse(", ")
                    .collect::<String>(),
            );
            (func_span, self.source) {
                err[func_span]: "this function does not belong to spec";
            }
        }

        push missing_spec_impl_funcs(self, span: Option<Span>, missing: &[VRef<str>]) {
            err: "missing spec functions";
            help: (
                "functions that are missing: {}",
                missing.iter()
                    .map(|&f| &self.interner[f])
                    .intersperse(", ")
                    .collect::<String>(),
            );
            (span?, self.source) {
                err[span?]: "this impl block does not implement all required functions";
            }
        }

        push extern_in_impl(self, span: Span) {
            err: "extern functions cannot be direct part of spec implementation";
            (span, self.source) {
                err[span]: "this function is extern";
            }
        }

        push spec_arg_count_mismatch(self, span: Span, expected: usize, got: usize) {
            err: "spec function argument count mismatch";
            info: ("expected {} arguments but got {}", expected, got);
            (span, self.source) {
                err[span]: "this function takes different number of arguments";
            }
        }

        push spec_arg_mismatch(self, span: Span, index: usize, expected: Ty, got: Ty) {
            err: "spec function argument type mismatch";
            info: (
                "expected '{}' but found '{}'",
                self.typec.display_ty(expected, self.interner),
                self.typec.display_ty(got, self.interner),
            );
            (span, self.source) {
                err[span]: ("this function takes different type as argument[{}]", index);
            }
        }

        push spec_ret_mismatch(self, span: Span, expected: Ty, got: Ty) {
            err: "spec function return type mismatch";
            info: (
                "expected '{}' but found '{}'",
                self.typec.display_ty(expected, self.interner),
                self.typec.display_ty(got, self.interner),
            );
            (span, self.source) {
                err[span]: "this function returns different type";
            }
        }
    }
}

pub type Inference = Option<Ty>;

enum FuncLookupResult<'a> {
    Func(VRef<Func>),
    SpecFunc(VRef<SpecFunc>, Ty),
    #[allow(dead_code)]
    Var(TirNode<'a>),
}
