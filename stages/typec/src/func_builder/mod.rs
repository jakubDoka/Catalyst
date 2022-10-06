use std::{default::default, iter, vec};

use diags::*;
use lexing_t::*;
use packaging_t::*;
use parsing::*;
use parsing_t::*;

use storage::*;

use typec_t::*;

use crate::*;

pub type ExprRes<'a> = Option<TypedTirNode<'a>>;

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
        let spec = self.typec.impls[impl_ref].spec;
        let spec_base = self.typec.types.base(spec);
        let spec_ent = self.typec.types[spec_base].kind.cast::<TySpec>();
        let spec_methods = self.typec.spec_funcs[spec_ent.methods].to_bumpvec();
        let mut methods = bumpvec![None; spec_methods.len()];
        for &(ast, func) in input {
            let Some(func_res) = self.build_func(ast, func, arena, offset) else { continue; };
            let Some(body) = func_res else { todo!() };
            compiled_funcs.push((func, body));

            let Func { name, .. } = self.typec.funcs[func];
            let Some((i, spec_func)) = spec_methods
                .iter()
                .copied()
                .enumerate()
                .find(|&(_, SpecFunc { name: n, .. })| n == name)
            else {
                todo!()
            };

            self.check_impl_signature(spec_func, func, ast.signature.span());

            methods[i] = Some(func);
        }

        let Some(methods) = methods.into_iter().collect::<Option<BumpVec<_>>>() else {
            todo!()
        };

        self.typec.impls[impl_ref].methods = self.typec.func_slices.bump(methods);
    }

    pub fn check_impl_signature(&mut self, spec_func: SpecFunc, func_id: VRef<Func>, _span: Span) {
        let func = self.typec.funcs[func_id];

        let spec_func_params = self
            .pack_spec_func_param_specs(spec_func)
            .collect::<BumpVec<_>>();
        let mut spec_func_slots = vec![None; spec_func_params.len()];
        let func_params = self.pack_func_param_specs(func_id).collect::<BumpVec<_>>();

        let spec_args = self.typec.ty_slices[spec_func.signature.args].to_bumpvec();
        let func_args = self.typec.ty_slices[func.signature.args].to_bumpvec();

        if spec_args.len() != func_args.len() {
            todo!()
        }

        for (spec_arg, func_arg) in spec_args.into_iter().zip(func_args) {
            let func_arg = self
                .typec
                .instantiate(func_arg, &func_params, self.interner);
            if let Err((a, b)) = self
                .typec
                .compatible(&mut spec_func_slots, func_arg, spec_arg)
            {
                todo!("{a:?}, {b:?}")
            }
        }

        let func_ret = self
            .typec
            .instantiate(func.signature.ret, &func_params, self.interner);
        if let Err((a, b)) =
            self.typec
                .compatible(&mut spec_func_slots, func_ret, spec_func.signature.ret)
        {
            todo!("{a:?}, {b:?}")
        }

        let Some(spec_func_slots) = spec_func_slots.into_iter().collect::<Option<BumpVec<_>>>() else {
            todo!()
        };

        let implements = spec_func_params
            .into_iter()
            .zip(spec_func_slots)
            .all(|(spec, ty)| self.typec.implements(ty, spec).is_some());

        if !implements {
            todo!()
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
            self.pack_func_param_specs(func).collect::<Vec<_>>(),
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
            Some(tir_body.node)
        } else {
            self.return_low(Some(tir_body), body.span(), &mut builder)
                .map(|tir| tir.node)
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
            return self.node(Ty::UNIT, BlockTir { nodes: &[], ty: Ty::UINT, span: block.span() }, builder)
        };

        let mut store = bumpvec![cap block.len()];
        store.extend(
            other
                .iter()
                .filter_map(|expr| self.expr(expr.value, None, builder))
                .map(|expr| expr.node),
        );
        let last = self.expr(last.value, inference, builder);
        builder.end_frame(frame);
        let last = last?;
        store.push(last.node);

        self.scope.end_frame(scope_frame);

        let nodes = builder.arena.alloc_slice(&store);
        self.node(
            last.ty,
            BlockTir {
                nodes,
                ty: last.ty,
                span: block.span(),
            },
            builder,
        )
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

        let ret = ReturnTir {
            val: value.map(|val| val.node),
            span,
        };

        self.node(Ty::TERMINAL, ret, builder)
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
            UnitExprAst::Int(span) => self.int(span, inference, builder),
            UnitExprAst::Char(span) => self.char(span, builder),
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
                ty,
                params.and_then(|(_, params)| {
                    params
                        .iter()
                        .map(|&param| self.ty(param))
                        .nsc_collect::<Option<BumpVec<_>>>()
                }),
            )
        } else {
            let ty = inference.or_else(|| self.cannot_infer(ctor.span())?)?;
            self.typec.types[ty]
                .kind
                .try_cast::<TyInstance>()
                .map(|ty| (ty.base, Some(self.typec.ty_slices[ty.args].to_bumpvec())))
                .unwrap_or((ty, None))
        };

        let struct_meta = self.typec.types[ty]
            .kind
            .try_cast::<TyStruct>()
            .or_else(|| self.expected_struct(ty, ctor.span())?)?;

        let mut param_slots = bumpvec![None; self.typec.ty_slices[struct_meta.generics].len()];

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
                .or_else(|| self.unknown_field(ty, struct_meta.fields, ctor.span())?)?;

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
            ty
        } else {
            self.typec.instance(ty, &params, self.interner)
        };

        let node = ConstructorTir {
            ty: final_ty,
            fields: builder
                .arena
                .alloc_iter(fields.iter().map(|field| field.node)),
            span: ctor.span(),
        };

        self.node(final_ty, node, builder)
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

        let run = ConstTir {
            value: expr?.node,
            span: run.span(),
        };

        self.node(expr?.ty, run, builder)
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
        caller: Option<TypedTirNode<'a>>,
        call @ CallExprAst { args, .. }: CallExprAst,
        inference: Inference,
        builder: &mut TirBuilder<'a>,
    ) -> ExprRes<'a> {
        let Func {
            signature,
            upper_generics,
            ..
        } = self.typec.funcs[func];

        let param_specs = self.pack_func_param_specs(func).collect::<BumpVec<_>>();
        let generic_start = self.typec.ty_slices[upper_generics].len();

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

        let call = CallTir {
            func: CallableTir::Func(func),
            args,
            span: call.span(),
            params,
            ty,
        };

        self.node(ty, call, builder)
    }

    fn direct_spec_call<'a>(
        &mut self,
        func: VRef<SpecFunc>,
        params: impl Iterator<Item = TyAst>,
        caller: Result<VRef<Ty>, TypedTirNode<'a>>,
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
            .pack_spec_func_param_specs(func_ent)
            .collect::<BumpVec<_>>();
        let mut param_slots = bumpvec![None; param_specs.len()];
        let generic_start = param_slots.len() - self.typec.ty_slices[generics].len();
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

        let call = CallTir {
            func: CallableTir::SpecFunc(func),
            args,
            span: call.span(),
            params,
            ty,
        };

        self.node(ty, call, builder)
    }

    fn call_params<'a>(
        &mut self,
        params: impl Iterator<Item = Option<VRef<Ty>>> + Clone,
        span: Span,
        builder: &mut TirBuilder<'a>,
    ) -> Option<&'a [VRef<Ty>]> {
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
        generics: &[VRef<Ty>],
        param_slots: &mut [Option<VRef<Ty>>],
        caller: Option<TypedTirNode<'a>>,
        args: CallArgsAst,
        signature: Signature,
        inference: Inference,
        builder: &mut TirBuilder<'a>,
    ) -> Option<(&'a [TirNode<'a>], &'a [VRef<Ty>], VRef<Ty>)> {
        let arg_types =
            self.typec.ty_slices[signature.args][caller.is_some() as usize..].to_bumpvec();
        if let Some(inference) = inference {
            self.infer_params(param_slots, inference, signature.ret, args.span());
        }

        let parsed_args = args
            .iter()
            .copied()
            .zip(arg_types)
            .map(|(arg, ty)| {
                let instantiated = self.typec.try_instantiate(ty, param_slots, self.interner);
                let parsed_arg = self.expr(arg, instantiated, builder);
                if let Some(parsed_arg) = parsed_arg && instantiated.is_none() {
                    self.infer_params(param_slots, parsed_arg.ty, ty, arg.span());
                }
                parsed_arg
            })
            .nsc_collect::<Option<BumpVec<_>>>()
            .map(|args| caller.into_iter().chain(args).map(|a| a.node))
            .map(|args| builder.arena.alloc_iter(args.collect::<BumpVec<_>>()))?;

        let params = self.call_params(param_slots.iter().copied(), args.span(), builder)?;

        for (&param, &spec) in params.iter().zip(generics) {
            let param = self
                .typec
                .instantiate(param, &builder.generics, self.interner);
            if self.typec.implements(param, spec).is_none() {
                self.missing_spec(param, spec, args.span());
            }
        }

        let ret = self.typec.instantiate(signature.ret, params, self.interner);

        Some((parsed_args, params, ret))
    }

    fn infer_params(
        &mut self,
        params: &mut [Option<VRef<Ty>>],
        reference: VRef<Ty>,
        template: VRef<Ty>,
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
            .intern(scoped_ident!(module.as_u32(), func_or_type.ident));
        if let Some(func) = self.typec.funcs.index(id) {
            return Some(FuncLookupResult::Func(func));
        }

        let ty = self
            .typec
            .types
            .index(id)
            .or_else(|| self.scope_error(ScopeError::NotFound, id, path.span(), TY)?)?;

        let (&start, segments) = segments
            .split_first()
            .or_else(|| self.invalid_expr_path(path.span())?)?;

        self.method_path(ty, PathExprAst { start, segments }, builder)
    }

    fn method_path<'a>(
        &mut self,
        ty: VRef<Ty>,
        path @ PathExprAst { start, segments }: PathExprAst,
        _builder: &mut TirBuilder<'a>,
    ) -> Option<FuncLookupResult<'a>> {
        let ty = self.typec.types.base(ty);
        match *segments {
            [] => {
                let ty_id = self.typec.types.id(ty);
                let id = self.interner.intern(scoped_ident!(ty_id, start.ident));
                match self.lookup(id, path.span(), FUNC)? {
                    ScopeItem::Func(func) => Some(FuncLookupResult::Func(func)),
                    ScopeItem::SpecFunc(func) => Some(FuncLookupResult::SpecFunc(func, ty)),
                    item => self.invalid_symbol_type(item, path.span(), FUNC)?,
                }
            }
            [name] => {
                let module = lookup!(Module self, start.ident, start.span);
                let method_id = self
                    .interner
                    .intern(scoped_ident!(module.as_u32(), name.ident));
                let ty_id = self.typec.types.id(ty);
                let id = self.interner.intern(scoped_ident!(ty_id, method_id));
                match self.typec.funcs.index(id) {
                    Some(func) => Some(FuncLookupResult::Func(func)),
                    None => self.scope_error(ScopeError::NotFound, id, path.span(), FUNC)?,
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
        self.node(
            builder.get_var(var).ty,
            AccessTir {
                span: path.span(),
                ty: builder.get_var(var).ty,
                var,
            },
            builder,
        )
    }

    fn int<'a>(
        &mut self,
        span: Span,
        inference: Inference,
        builder: &mut TirBuilder<'a>,
    ) -> ExprRes<'a> {
        let span_str = span_str!(self, span);
        let (ty, postfix_len) = Ty::INTEGERS
            .iter()
            .map(|&ty| (ty, &self.interner[self.typec.types.id(ty)]))
            .find_map(|(ty, str)| span_str.ends_with(str).then_some((ty, str.len())))
            .or_else(|| {
                inference
                    .filter(|ty| Ty::INTEGERS.contains(ty))
                    .map(|ty| (ty, 0))
            })
            .unwrap_or((Ty::UINT, 0));
        self.node(
            ty,
            IntLit {
                span: span.sliced(..span.len() - postfix_len),
                ty,
            },
            builder,
        )
    }

    fn char<'a>(&mut self, span: Span, builder: &mut TirBuilder<'a>) -> ExprRes<'a> {
        self.node(Ty::CHAR, TirNode::Char(span.shrink(1)), builder)
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
                self.interner
                    .intern(scoped_ident!(module.as_u32(), op.start.ident))
            }
            _ => self.invalid_op_expr_path(op.span())?,
        };

        let id = self.typec.binary_op_id(base_id, lhs.ty, rhs.ty);
        let id = self.interner.intern(id);
        let func = lookup!(Func self, id, op.span());

        let ty = self.typec.funcs[func].signature.ret;
        let call = CallTir {
            func: CallableTir::Func(func),
            params: default(),
            args: builder.arena.alloc_slice(&[lhs.node, rhs.node]),
            ty,
            span: binary_ast.span(),
        };
        self.node(ty, call, builder)
    }

    fn node<'a>(
        &mut self,
        ty: VRef<Ty>,
        node: impl NodeInput<'a>,
        builder: &mut TirBuilder<'a>,
    ) -> Option<TypedTirNode<'a>> {
        let node = builder.node(node);
        Some(TypedTirNode { node, ty })
    }

    fn args(&mut self, types: VSlice<VRef<Ty>>, args: FuncArgsAst, builder: &mut TirBuilder) {
        for (&ty, &arg) in self.typec.ty_slices[types].iter().zip(args.iter()) {
            let param = Variable {
                value: None,
                ty,
                span: arg.span(),
            };
            let value = builder.node(param);

            let var = builder.create_var(value, ty, arg.name.span);
            self.scope.push(arg.name.ident, var, arg.name.span);
        }
    }

    fn type_check(&mut self, expected: VRef<Ty>, got: VRef<Ty>, span: Span) -> Option<()> {
        self.type_check_detailed(expected, got, |s| {
            s.generic_ty_mismatch(expected, got, span)
        })
    }

    fn type_check_detailed<A>(
        &mut self,
        expected: VRef<Ty>,
        got: VRef<Ty>,
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

        push generic_ty_mismatch(self, expected: VRef<Ty>, got: VRef<Ty>, span: Span) {
            err: "type mismatch";
            info: ("expected '{}' but got '{}'", self.type_diff(expected, got), self.type_diff(got, expected));
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

        push missing_spec(self, ty: VRef<Ty>, spec: VRef<Ty>, span: Span) {
            err: (
                "'{}' does not implement '{}'",
                &self.interner[self.typec.types.id(ty)],
                &self.interner[self.typec.types.id(spec)],
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

        push expected_struct(self, ty: VRef<Ty>, span: Span) {
            err: "expected struct type";
            info: ("found '{}'", &self.interner[self.typec.types.id(ty)]);
            (span, self.source) {
                err[span]: "when type checking this";
            }
        }

        push unknown_field(self, ty: VRef<Ty>, fields: VSlice<Field>, span: Span) {
            err: ("unknown field");
            info: (
                "available fields in '{}': {}",
                &self.interner[self.typec.types.id(ty)],
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
    }
}

pub type Inference = Option<VRef<Ty>>;

enum FuncLookupResult<'a> {
    Func(VRef<Func>),
    SpecFunc(VRef<SpecFunc>, VRef<Ty>),
    #[allow(dead_code)]
    Var(TypedTirNode<'a>),
}
