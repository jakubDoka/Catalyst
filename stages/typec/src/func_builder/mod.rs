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
        compiled_funcs: &mut BumpVec<(FragRef<Func>, TirNode<'a>)>,
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

            let offset = impl_ast.generics.len();
            let Func { generics, .. } = self.typec.funcs[first];
            self.insert_generics(impl_ast.generics, 0);
            self.insert_spec_functions(generics, 0);

            if let Some(impl_ref) = impl_ref {
                self.build_spec_impl(arena, impl_ref, funcs, compiled_funcs, ctx, offset);
            } else {
                self.build_funcs(arena, funcs, compiled_funcs, extern_funcs, ctx, offset);
            }

            self.scope.end_frame(frame);
        }

        self
    }

    fn build_spec_impl<'a>(
        &mut self,
        arena: &'a Arena,
        impl_ref: FragRef<Impl>,
        input: &[(FuncDefAst, FragRef<Func>)],
        compiled_funcs: &mut BumpVec<(FragRef<Func>, TirNode<'a>)>,
        ctx: &mut TirBuilderCtx,
        offset: usize,
    ) {
        let Impl {
            key: ImplKey { ty, spec },
            span: Some(span),
            ..
        } = self.typec.impls[impl_ref] else {
            todo!();
        };

        let spec_base = spec.base(self.typec);
        if SpecBase::is_macro(spec_base) && let Some(mut macro_impl) = self.typec.macros.get_mut(&ty) {
            macro_impl.r#impl = Some(impl_ref);
            ctx.macros.push(MacroCompileRequest {
                name: macro_impl.name,
                ty,
                r#impl: impl_ref,
            });
        }
        let spec_ent = self.typec[spec_base];
        let spec_methods = self.typec.spec_funcs[spec_ent.methods].to_bumpvec();

        self.scope.push(Interner::SELF, ty, span);

        let mut methods = bumpvec![None; spec_methods.len()];
        for &(ast, func) in input {
            let Some(func_res) = self.build_func(ast, func, arena, ctx, offset) else { continue; };
            let Some(body) = func_res else {
                self.extern_in_impl(ast.span);
                continue;
            };
            compiled_funcs.push((func, body));

            let Func {
                name,
                signature: Signature { ref mut cc, .. },
                ..
            } = self.typec.funcs[func];
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

            *cc = spec_func.signature.cc;

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

        self.typec.impls[impl_ref].methods = self.typec.func_slices.extend(methods);
    }

    pub fn check_impl_signature(
        &mut self,
        implementor: Ty,
        spec_func: SpecFunc,
        func_id: FragRef<Func>,
        span: Span,
    ) {
        let func = self.typec.funcs[func_id];

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
        for (specs, &ty) in spec_func_params.into_iter().zip(spec_func_slots.iter()) {
            self.typec.implements_sum(
                ty,
                specs,
                func_params.as_slice(),
                &spec_func_slots,
                &mut Some(&mut missing_keys),
                self.interner,
            );
        }

        for ImplKey { ty, spec } in missing_keys {
            self.missing_spec(ty, spec, span);
        }
    }

    pub fn build_funcs<'a>(
        &mut self,
        arena: &'a Arena,
        input: &[(FuncDefAst, FragRef<Func>)],
        compiled_funcs: &mut BumpVec<(FragRef<Func>, TirNode<'a>)>,
        extern_funcs: &mut Vec<FragRef<Func>>,
        ctx: &mut TirBuilderCtx,
        offset: usize,
    ) -> &mut Self {
        let iter = input.iter().filter_map(|&(ast, func)| {
            let res = self.build_func(ast, func, arena, ctx, offset)?;

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
        func: FragRef<Func>,
        arena: &'a Arena,
        ctx: &mut TirBuilderCtx,
        offset: usize,
    ) -> Option<Option<TirNode<'a>>> {
        let frame = self.scope.start_frame();
        let Func {
            signature,
            generics: self_generics,
            ..
        } = self.typec.funcs[func];

        ctx.vars.clear();
        ctx.generics.clear();
        ctx.generics.extend(self.typec.pack_func_param_specs(func));
        let mut builder = TirBuilder::new(arena, signature.ret, ret.map(|ret| ret.span()), ctx);

        self.insert_generics(generics, offset);
        self.insert_spec_functions(self_generics, offset);
        self.args(signature.args, args, &mut builder);

        let tir_body = match body {
            FuncBodyAst::Arrow(.., expr) => {
                self.expr(expr, Inference::Weak(signature.ret), &mut builder)
            }
            FuncBodyAst::Block(body) => {
                self.block(body, Inference::Weak(signature.ret), &mut builder)
            }
            FuncBodyAst::Extern(..) => return Some(None),
        }?;

        self.scope.end_frame(frame);

        Some(if tir_body.ty == Ty::TERMINAL {
            Some(tir_body)
        } else if tir_body.ty == signature.ret {
            self.return_low(Some(tir_body), body.span(), &mut builder)
        } else {
            let ret = self.return_low(None, body.span(), &mut builder)?;
            Some(TirNode::new(
                Ty::TERMINAL,
                TirKind::Block(builder.arena.alloc([tir_body, ret])),
                tir_body.span,
            ))
        })
    }

    fn block<'a>(
        &mut self,
        block: BlockAst,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let frame = VarHeaderTir::start_frame(builder.ctx);
        let scope_frame = self.scope.start_frame();

        let Some((last, other)) = block.elements.split_last() else {
            return Some(TirNode::new(Ty::UNIT, TirKind::Block(&[]), block.span()))
        };

        let mut store = bumpvec![cap block.len()];
        store.extend(
            other
                .iter()
                .filter_map(|expr| self.expr(expr.value, Inference::None, builder)),
        );
        let last = self.expr(last.value, inference, builder);
        frame.end(builder.ctx, ());
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
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        if let Some((span, ..)) = builder.runner {
            self.control_flow_in_const(span, span);
        }

        let value = expr
            .map(|expr| self.expr(expr, Inference::Strong(builder.ret), builder))
            .transpose()?;

        let span = expr.map_or(span, |expr| span.joined(expr.span()));

        self.return_low(value, span, builder)
    }

    fn return_low<'a>(
        &mut self,
        value: ExprRes<'a>,
        span: Span,
        builder: &mut TirBuilder<'a, '_>,
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
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let value = match expr {
            ExprAst::Unit(&unit) => self.unit_expr(unit, inference, builder),
            ExprAst::Binary(&binary) => self.binary_expr(binary, builder),
        }?;

        if let Inference::Strong(ty) = inference {
            self.type_check(ty, value.ty, expr.span())?;
        }

        Some(value)
    }

    fn unit_expr<'a>(
        &mut self,
        unit_ast: UnitExprAst,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        use UnitExprAst::*;
        match unit_ast {
            Path(path) => self.value_path(path, inference, builder),
            Return(ReturnExprAst { return_span, expr }) => {
                self.r#return(expr, return_span, builder)
            }
            Int(span) => self.int(span, inference),
            Char(span) => self.char(span),
            Bool(span) => self.bool(span),
            Call(&call) => self.call(call, inference, builder),
            StructCtor(ctor) => self.struct_ctor(ctor, inference, builder),
            EnumCtor(ctor) => self.enum_ctor(ctor, inference, builder),
            Match(match_expr) => self.r#match(match_expr, inference, builder),
            If(r#if) => self.r#if(r#if, inference, builder),
            Loop(loop_expr) => self.r#loop(loop_expr, inference, builder),
            DotExpr(&expr) => self.dot_expr(expr, inference, builder),
            Let(r#let) => self.r#let(r#let, inference, builder),
            Deref(.., &expr) => self.deref(expr, inference, builder),
            Ref(.., mutability, &expr) => self.r#ref(mutability, expr, inference, builder),
            PathInstance(_) => todo!(),
            TypedPath(_) => todo!(),
        }
    }

    fn r#loop<'a>(
        &mut self,
        loop_expr: LoopAst,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let loop_id = builder.ctx.loops.push(LoopHeaderTir {
            return_type: Ty::TERMINAL,
            inference,
            label: loop_expr.label.map(|label| label.ident),
        });

        let var_frame = VarHeaderTir::start_frame(builder.ctx);
        let frame = self.scope.start_frame();
        if let Some(label) = loop_expr.label {
            self.scope
                .push(label.ident, ScopeItem::LoopHeaderTir(loop_id), label.span);
        }

        let body = self.expr(loop_expr.body, Inference::None, builder)?;
        self.scope.end_frame(frame);
        var_frame.end(builder.ctx, ());

        let ty = builder
            .ctx
            .loops
            .pop()
            .expect("this functions is the only one that pops and pushes")
            .return_type;

        Some(TirNode::new(
            ty,
            TirKind::Loop(builder.arena.alloc(LoopTir { id: loop_id, body })),
            loop_expr.span(),
        ))
    }

    fn deref<'a>(
        &mut self,
        expr: UnitExprAst,
        _inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let expr = self.unit_expr(expr, Inference::None, builder)?;
        let Ty::Pointer(ptr) = expr.ty else {
            todo!();
        };

        let base = self.typec[ptr].base;

        Some(TirNode::new(
            base,
            TirKind::Deref(builder.arena.alloc(expr)),
            expr.span,
        ))
    }

    fn r#ref<'a>(
        &mut self,
        mutability: MutabilityAst,
        expr: UnitExprAst,
        _inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let expr = self.unit_expr(expr, Inference::None, builder)?;
        let mutability = self.mutability(mutability)?;
        let ptr = self.typec.pointer_to(mutability, expr.ty, self.interner);

        Some(TirNode::new(
            Ty::Pointer(ptr),
            TirKind::Ref(builder.arena.alloc(expr)),
            expr.span,
        ))
    }

    fn r#let<'a>(
        &mut self,
        r#let @ LetAst { pat, ty, value, .. }: LetAst,
        _inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let ty = ty.map(|(.., ty)| self.ty(ty)).transpose()?;
        let value = self.expr(value, ty.into(), builder)?;
        let pat = self.pattern(pat, value.ty, builder)?;

        Some(TirNode::new(
            Ty::UNIT,
            TirKind::Let(builder.arena.alloc(LetTir { pat, value })),
            r#let.span(),
        ))
    }

    fn r#if<'a>(
        &mut self,
        r#if @ IfAst {
            cond,
            body,
            elifs,
            r#else,
            ..
        }: IfAst,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let top = self.if_branch(cond, body, inference, builder)?;

        let elifs = elifs
            .iter()
            .filter_map(|&ElifAst { cond, body, .. }| {
                self.if_branch(cond, body, inference, builder)
            })
            .collect::<BumpVec<_>>();
        let elifs = builder.arena.alloc_iter(elifs);

        let r#else = r#else
            .map(|(.., r#else)| self.if_block(r#else, inference, builder))
            .transpose()?;

        let ty = Self::combine_branch_types(
            elifs
                .iter()
                .copied()
                .map(|branch| branch.body.ty)
                .chain(r#else.map(|r#else| r#else.ty)),
        );

        Some(TirNode::new(
            ty,
            TirKind::If(builder.arena.alloc(IfTir { top, elifs, r#else })),
            r#if.span(),
        ))
    }

    fn if_branch<'a>(
        &mut self,
        cond: ExprAst,
        body: IfBlockAst,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> Option<IfBranchTir<'a>> {
        let cond = self.expr(cond, Inference::Strong(Ty::BOOL), builder)?;
        let body = self.if_block(body, inference, builder)?;
        Some(IfBranchTir { cond, body })
    }

    fn if_block<'a>(
        &mut self,
        body: IfBlockAst,
        mut inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        inference = inference.weaken();
        match body {
            IfBlockAst::Block(body) => self.block(body, inference, builder),
            IfBlockAst::Arrow(.., body) => {
                let frame = self.scope.start_frame();
                let expr = self.expr(body, inference, builder);
                self.scope.end_frame(frame);
                let expr = expr?;
                Some(TirNode::new(
                    expr.ty,
                    TirKind::Block(builder.arena.alloc_iter([expr])),
                    expr.span,
                ))
            }
        }
    }

    fn enum_ctor<'a>(
        &mut self,
        ctor @ EnumCtorAst {
            path: PathInstanceAst { path, params },
            value,
        }: EnumCtorAst,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let (ty, variant_name) = self.enum_path(path, inference)?;

        let (variant, variant_ty) = self.typec[self.typec[ty].variants]
            .iter()
            .enumerate()
            .find_map(|(i, variant)| {
                (variant.name == variant_name.ident).then_some((i, variant.ty))
            })
            .or_else(|| todo!())?;

        let flag_ty = self.typec.enum_flag_ty(ty).unwrap();

        let variant_value = TirNode::new(
            Ty::Builtin(flag_ty),
            TirKind::Int(Some(variant as i64)),
            variant_name.span,
        );

        let mut param_slots = bumpvec![None; self.typec[ty].generics.len()];
        if let Some(inference) = inference.ty() && let Ty::Instance(inst) = inference {
            param_slots.iter_mut().zip(&self.typec[self.typec[inst].args])
                .for_each(|(slot, &arg)| *slot = Some(arg))
        }
        if let Some((.., params)) = params {
            for (slot, &param) in param_slots.iter_mut().zip(params.iter()) {
                *slot = self.ty(param);
            }
        }

        let value = if let Some((.., value)) = value {
            let inference = self
                .typec
                .try_instantiate(variant_ty, &param_slots, self.interner);
            let res = self.expr(value, inference.into(), builder)?;
            if inference.is_none() {
                self.infer_params(&mut param_slots, res.ty, variant_ty, value.span())?;
            }
            Some(res)
        } else if variant_ty != Ty::UNIT {
            todo!();
        } else {
            None
        };

        let ty = if param_slots.is_empty() {
            Ty::Enum(ty)
        } else {
            let Some(args) = param_slots.iter().copied().collect::<Option<BumpVec<_>>>() else {
                todo!();
            };
            Ty::Instance(
                self.typec
                    .instance(GenericTy::Enum(ty), &args, self.interner),
            )
        };

        let values = iter::once(variant_value)
            .chain(value.into_iter())
            .collect::<BumpVec<_>>();
        let values = builder.arena.alloc_iter(values);
        Some(TirNode::new(ty, TirKind::Ctor(values), ctor.span()))
    }

    fn enum_path(
        &mut self,
        path: PathExprAst,
        inference: Inference,
    ) -> Option<(FragRef<Enum>, NameAst)> {
        if path.slash.is_some() {
            let Some(expected) = inference.ty() else {
                self.cannot_infer(path.span())?;
            };

            let Ty::Enum(enum_ty) = expected.base(self.typec) else {
                todo!();
            };

            return Some((enum_ty, path.start));
        }

        let module = match self.lookup(path.start.ident, path.start.span, "module or enum")? {
            ScopeItem::Ty(Ty::Enum(enum_ty)) => {
                let &[variant] = path.segments else {
                    todo!();
                };

                return Some((enum_ty, variant));
            }
            ScopeItem::Module(module) => module,
            _ => todo!(),
        };

        let &[r#enum, variant] = path.segments else {
            todo!();
        };

        let id = self.interner.intern_scoped(module.index(), r#enum.ident);
        let Ty::Enum(enum_ty) = lookup!(Ty self, id, r#enum.span) else {
            todo!();
        };

        Some((enum_ty, variant))
    }

    fn dot_expr<'a>(
        &mut self,
        DotExprAst { lhs, rhs, .. }: DotExprAst,
        _inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let mut header = self.unit_expr(lhs, Inference::None, builder)?;

        let deref = header.ty.ptr_base(self.typec);
        let caller = deref.base(self.typec);
        let res = self.dot_path(caller, rhs, builder)?;

        self.balance_pointers(&mut header, deref, builder)?;

        Some(match res {
            DotPathResult::Field(field, ty) => TirNode::new(
                ty,
                TirKind::Field(builder.arena.alloc(FieldTir { header, field })),
                rhs.span(),
            ),
        })
    }

    fn dot_path<'a>(
        &mut self,
        ty: Ty,
        PathInstanceAst { path, params }: PathInstanceAst,
        _builder: &mut TirBuilder<'a, '_>,
    ) -> Option<DotPathResult> {
        if let Some((.., params)) = params {
            self.unexpected_params(params.span())?;
        }

        let Ty::Struct(struct_id) = ty.base(self.typec) else {
            self.non_struct_field_access(ty, path.span())?;
        };

        let Some((field, Field { ty: mut field_ty, .. })) = Struct::find_field(struct_id, path.start.ident, self.typec) else {
            self.field_not_found(struct_id, path.start)?;
        };

        if let Ty::Instance(instance) = ty {
            let params = self.typec[self.typec[instance].args].to_bumpvec();
            field_ty = self.typec.instantiate(ty, &params, self.interner);
        }

        Some(DotPathResult::Field(field as u32, field_ty))
    }

    fn find_struct_field(
        &self,
        struct_id: FragRef<Struct>,
        field_name: Ident,
    ) -> Option<(usize, FragRef<Field>, Ty)> {
        let Struct { fields, .. } = self.typec[struct_id];
        self.typec
            .fields
            .indexed(fields)
            .enumerate()
            .find_map(|(i, (id, field))| (field.name == field_name).then_some((i, id, field.ty)))
    }

    fn r#match<'a>(
        &mut self,
        MatchExprAst { expr, body, .. }: MatchExprAst,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let value = self.expr(expr, Inference::None, builder)?;

        let arms = body
            .iter()
            .map(|&MatchArmAst { pattern, body, .. }| {
                let frame = self.scope.start_frame();
                let var_frame = VarHeaderTir::start_frame(builder.ctx);
                let pat = self.pattern(pattern, value.ty, builder);
                let body = self.if_block(body, inference, builder);
                self.scope.end_frame(frame);
                var_frame.end(builder.ctx, ());
                Some(MatchArmTir {
                    pat: pat?,
                    body: body?,
                })
            })
            .collect::<Option<BumpVec<_>>>()?;
        let arms = builder.arena.alloc_iter(arms);

        let ty = Self::combine_branch_types(arms.iter().map(|arm| arm.body.ty));

        let tir = builder.arena.alloc(MatchTir { value, arms });
        Some(TirNode::new(ty, TirKind::Match(tir), body.span()))
    }

    fn combine_branch_types(tys: impl IntoIterator<Item = Ty>) -> Ty {
        tys.into_iter()
            .reduce(|a, b| match Ty::compatible(a, b) {
                true if a != Ty::TERMINAL => a,
                true => b,
                false => Ty::UNIT,
            })
            .unwrap_or(Ty::TERMINAL)
    }

    fn pattern<'a>(
        &mut self,
        pattern: PatAst,
        ty: Ty,
        builder: &mut TirBuilder<'a, '_>,
    ) -> Option<PatTir<'a>> {
        match pattern {
            PatAst::Binding(mutable, name) => {
                let var = builder.create_var(mutable.is_some(), ty, name.span);
                self.scope.push(name.ident, var, name.span);
                Some(PatTir {
                    kind: PatKindTir::Unit(UnitPatKindTir::Binding(mutable.is_some(), var)),
                    span: name.span,
                    has_binding: true,
                    is_refutable: false,
                    ty,
                })
            }
            PatAst::StructCtor(StructCtorPatAst { fields, .. }) => {
                let Ty::Struct(struct_ty) = ty.caller(self.typec) else {
                    self.expected_struct(ty, pattern.span())?;
                };
                let mut tir_fields = bumpvec![None; fields.len()];
                let mut double_dot = None;
                for &field in fields.iter() {
                    match field {
                        StructCtorPatFieldAst::Simple { name, mutable } => {
                            let Some((field_id, .., field_ty)) = self.find_struct_field(struct_ty, name.ident) else {
                                self.field_not_found(struct_ty, name)?;
                            };

                            let field =
                                self.pattern(PatAst::Binding(mutable, name), field_ty, builder)?;
                            tir_fields[field_id] = Some(field);
                        }
                        StructCtorPatFieldAst::Named { name, pat, .. } => {
                            let Some((field_id, .., field_ty)) = self.find_struct_field(struct_ty, name.ident) else {
                                self.field_not_found(struct_ty, name)?;
                            };

                            let field = self.pattern(pat, field_ty, builder)?;
                            tir_fields[field_id] = Some(field);
                        }
                        StructCtorPatFieldAst::DoubleDot(span) => {
                            if let Some(prev) = double_dot.replace(span) {
                                self.duplicate_double_dot(span, prev)?;
                            }
                        }
                    }
                }

                if let Some(double_dot) = double_dot {
                    tir_fields.iter_mut().filter(|f| f.is_none()).for_each(|f| {
                        *f = Some(PatTir {
                            kind: PatKindTir::Unit(UnitPatKindTir::Wildcard),
                            span: double_dot,
                            ty: Ty::UNIT,
                            has_binding: false,
                            is_refutable: false,
                        })
                    });
                }

                let Some(tir_fields) = tir_fields.iter().copied().collect::<Option<BumpVec<_>>>() else {
                    let missing_fields = tir_fields
                        .iter()
                        .zip(&self.typec[self.typec[struct_ty].fields])
                        .filter_map(|(opt, f)| opt.is_none().then_some(f.name))
                        .collect::<BumpVec<_>>();

                    self.missing_pat_ctor_fields(missing_fields, pattern.span())?;
                };

                Some(PatTir {
                    has_binding: tir_fields.iter().any(|f| f.has_binding),
                    is_refutable: tir_fields.iter().any(|f| f.is_refutable),
                    kind: PatKindTir::Unit(UnitPatKindTir::Struct {
                        fields: builder.arena.alloc_iter(tir_fields),
                    }),
                    span: fields.span(),
                    ty,
                })
            }
            PatAst::Int(span) => Some(PatTir {
                kind: PatKindTir::Unit(UnitPatKindTir::Int(Ok(span))),
                span,
                ty,
                has_binding: false,
                is_refutable: true,
            }),
            PatAst::EnumCtor(ctor) => {
                let ty_base = ty.ptr_base(self.typec);
                let Ty::Enum(enum_ty) = ty_base.base(self.typec) else {
                    todo!();
                };

                let (index, variant_ty) = ty_base
                    .find_component(ctor.name.ident, self.typec, self.interner)
                    .or_else(|| todo!())?;

                let value = ctor
                    .value
                    .map(|(.., body)| self.pattern(body, variant_ty, builder))
                    .transpose()?;

                Some(PatTir {
                    kind: PatKindTir::Unit(UnitPatKindTir::Enum {
                        id: index as u32,
                        ty: enum_ty,
                        value: value.map(|value| builder.arena.alloc(value)),
                    }),
                    span: ctor.span(),
                    ty,
                    has_binding: value.map_or(false, |v| v.has_binding),
                    is_refutable: self.typec[enum_ty].variants.len() > 1,
                })
            }
            PatAst::Wildcard(span) => Some(PatTir {
                kind: PatKindTir::Unit(UnitPatKindTir::Wildcard),
                has_binding: false,
                is_refutable: false,
                span,
                ty,
            }),
        }
    }

    fn struct_ctor<'a>(
        &mut self,
        ctor @ StructCtorAst { path, body, .. }: StructCtorAst,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let (ty, params) = if let Some(PathInstanceAst { path, params }) = path {
            let ty = self.ty_path(path)?;
            (
                match ty {
                    TyPathResult::Ty(ty) => ty.as_generic(),
                    _ => None,
                },
                params.and_then(|(_, params)| {
                    params
                        .iter()
                        .map(|&param| self.ty(param))
                        .nsc_collect::<Option<BumpVec<_>>>()
                }),
            )
        } else {
            let ty = inference.ty().or_else(|| self.cannot_infer(ctor.span())?)?;
            match ty {
                Ty::Instance(instance) => (
                    Some(self.typec[instance].base),
                    Some(self.typec[self.typec[instance].args].to_bumpvec()),
                ),
                _ => (ty.as_generic(), None),
            }
        };

        let Some(GenericTy::Struct(struct_id)) = ty else {
            self.expected_struct_path(path.map_or(ctor.span(), |p| p.span()))?;
        };

        let struct_meta = self.typec[struct_id];

        let mut param_slots = bumpvec![None; struct_meta.generics.len()];

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

        for field_ast @ &StructCtorFieldAst { name, expr } in body.iter() {
            let (index, field) = self.typec.fields[struct_meta.fields]
                .iter()
                .copied()
                .enumerate()
                .find(|(.., field)| field.name == name.ident)
                .or_else(|| {
                    self.unknown_field(Ty::Struct(struct_id), struct_meta.fields, ctor.span())?
                })?;

            let inference = self
                .typec
                .try_instantiate(field.ty, &param_slots, self.interner);
            let expr = if let Some(expr) = expr {
                self.expr(expr, inference.into(), builder)
            } else {
                self.value_path(
                    PathExprAst {
                        start: name,
                        segments: &[],
                        slash: None,
                    },
                    inference.into(),
                    builder,
                )
            };
            let Some(value) = expr else {
                continue;
            };

            self.infer_params(&mut param_slots, value.ty, field.ty, field_ast.span());

            if fields[index].replace(value).is_some() {
                self.duplicate_field(name.span());
            }
        }

        let Some(params) = param_slots.iter().copied().collect::<Option<BumpVec<_>>>() else {
            let missing_params = param_slots
                .iter()
                .copied()
                .enumerate()
                .filter_map(|(i, param)| param.is_none().then_some(i))
                .collect::<BumpVec<_>>();
            self.missing_constructor_params(ctor.span(), missing_params)?;
        };

        let Some(fields) = fields.iter().copied().collect::<Option<BumpVec<_>>>() else {
            let missing_fields = self.typec.fields[struct_meta.fields]
                .iter()
                .zip(fields.iter())
                .filter_map(|(field, value)| value.is_none().then_some(field.name))
                .collect::<BumpVec<_>>();

            self.missing_constructor_fields(ctor.span(), missing_fields.as_slice())?;
        };

        let final_ty = if params.is_empty() {
            Ty::Struct(struct_id)
        } else {
            Ty::Instance(
                self.typec
                    .instance(GenericTy::Struct(struct_id), &params, self.interner),
            )
        };

        Some(TirNode::new(
            final_ty,
            TirKind::Ctor(builder.arena.alloc_iter(fields)),
            ctor.span(),
        ))
    }

    fn call<'a>(
        &mut self,
        call @ CallExprAst { callable, .. }: CallExprAst,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
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
                        self.direct_call(func, params, Some(Ok(ty)), call, inference, builder)
                    }
                    FuncLookupResult::SpecFunc(func, ..) => {
                        self.direct_spec_call(func, params, Ok(ty), call, inference, builder)
                    }
                    FuncLookupResult::Var(..) => todo!(),
                }
            }
            UnitExprAst::DotExpr(&DotExprAst { lhs, rhs, .. }) => {
                let lhs = self.unit_expr(lhs, Inference::None, builder)?;
                let func = self.method_path(lhs.ty, rhs.path, builder)?;
                let params = rhs.params();
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

    fn direct_call<'a>(
        &mut self,
        func: FragRef<Func>,
        params: impl Iterator<Item = TyAst>,
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
            .zip(params)
            .for_each(|(param_slot, param)| *param_slot = self.ty(param));

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

    fn direct_spec_call<'a>(
        &mut self,
        func: FragRef<SpecFunc>,
        params: impl Iterator<Item = TyAst>,
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
            .zip(params)
            .for_each(|(param_slot, param)| *param_slot = self.ty(param));
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

    fn call_params<'a>(
        &mut self,
        params: impl Iterator<Item = Option<Ty>> + Clone,
        span: Span,
        builder: &mut TirBuilder<'a, '_>,
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
            let _ = self.typec.compatible(param_slots, inference, signature.ret);
        }

        if let Some(ref mut caller) = caller {
            if let Some(&first) = arg_types.first() && let Err(caller) = caller {
                self.balance_pointers(caller, first, builder);
            }

            if let Some(owner) = owner {
                let ty = caller.map_or_else(|expr| expr.ty, |ty| ty);
                let owner = self.typec.balance_pointers(owner, ty, self.interner);
                self.infer_params(param_slots, ty, owner, args.span());
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

        let params = self.call_params(param_slots.iter().copied(), args.span(), builder)?;

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
        builder: &mut TirBuilder<'a, '_>,
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
        path @ PathExprAst {
            start, segments, ..
        }: PathExprAst,
        builder: &mut TirBuilder<'a, '_>,
    ) -> Option<FuncLookupResult<'a>> {
        let module = match self.lookup(start.ident, start.span, FUNC_OR_MOD)? {
            ScopeItem::Func(func) => return Some(FuncLookupResult::Func(func)),
            ScopeItem::Module(module) => module,
            ScopeItem::Ty(ty) => {
                let (&start, segments) = segments
                    .split_first()
                    .or_else(|| self.invalid_expr_path(path.span())?)?;
                return self.method_path(
                    ty,
                    PathExprAst {
                        start,
                        segments,
                        slash: None,
                    },
                    builder,
                );
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

        self.method_path(
            ty,
            PathExprAst {
                start,
                segments,
                slash: None,
            },
            builder,
        )
    }

    fn method_path<'a>(
        &mut self,
        ty: Ty,
        path @ PathExprAst {
            start, segments, ..
        }: PathExprAst,
        _builder: &mut TirBuilder<'a, '_>,
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
        path @ PathExprAst { slash, start, .. }: PathExprAst,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        if slash.is_some() {
            let Some(inferred) = inference.ty() else {
                todo!();
            };

            match inferred.base(self.typec) {
                Ty::Enum(..) => {
                    return self.enum_ctor(
                        EnumCtorAst {
                            path: PathInstanceAst { path, params: None },
                            value: None,
                        },
                        inference,
                        builder,
                    )
                }
                Ty::Struct(_) => todo!(),
                Ty::Instance(_) => todo!(),
                Ty::Pointer(_) => todo!(),
                Ty::Param(_) => todo!(),
                Ty::Builtin(_) => todo!(),
            }
        }

        Some(
            match self.lookup(start.ident, start.span, "variable or enum")? {
                ScopeItem::Func(_) => todo!(),
                ScopeItem::SpecFunc(_) => todo!(),
                ScopeItem::Ty(ty) => match ty {
                    Ty::Struct(_) => todo!(),
                    Ty::Enum(..) => {
                        return self.enum_ctor(
                            EnumCtorAst {
                                path: PathInstanceAst { path, params: None },
                                value: None,
                            },
                            inference,
                            builder,
                        )
                    }
                    Ty::Instance(_) => todo!(),
                    Ty::Pointer(_) => todo!(),
                    Ty::Param(_) => todo!(),
                    Ty::Builtin(_) => todo!(),
                },
                ScopeItem::SpecBase(_) => todo!(),
                ScopeItem::VarHeaderTir(var) => {
                    TirNode::new(builder.get_var(var).ty, TirKind::Access(var), path.span())
                }
                item => self.invalid_symbol_type(item, start.span, "variable or enum")?,
            },
        )
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
                    .ty()
                    .filter(|ty| Ty::INTEGERS.contains(ty))
                    .map(|ty| (ty, 0))
            })
            .unwrap_or((Ty::UINT, 0));
        Some(TirNode::new(
            ty,
            TirKind::Int(None),
            span.sliced(..span_str.len() - postfix_len),
        ))
    }

    fn char<'a>(&mut self, span: Span) -> ExprRes<'a> {
        Some(TirNode::new(Ty::CHAR, TirKind::Char, span))
    }

    fn bool(&mut self, span: Span) -> ExprRes<'static> {
        Some(TirNode::new(
            Ty::BOOL,
            TirKind::Bool(span_str!(self, span).starts_with('t')),
            span,
        ))
    }

    fn binary_expr<'a>(
        &mut self,
        binary_ast @ BinaryExprAst { lhs, op, rhs }: BinaryExprAst,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let lhs = self.expr(lhs, Inference::None, builder);

        if op.ident == Interner::ASSIGN {
            let rhs = self.expr(rhs, lhs.map(|lhs| lhs.ty).into(), builder)?;
            return Some(TirNode::new(
                Ty::UNIT,
                TirKind::Assign(builder.arena.alloc(AssignTir { lhs: lhs?, rhs })),
                binary_ast.span(),
            ));
        }

        let rhs = self.expr(rhs, Inference::None, builder);
        let (lhs, rhs) = (lhs?, rhs?); // recovery

        let func = self.find_binary_func(op, lhs.ty, rhs.ty)?;

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

    fn find_binary_func(&mut self, op: NameAst, lhs_ty: Ty, rhs_ty: Ty) -> OptFragRef<Func> {
        let base_id = op.ident;
        let id = self
            .interner
            .intern_with(|s, t| self.typec.binary_op_id(base_id, lhs_ty, rhs_ty, t, s));
        Some(lookup!(Func self, id, op.span()))
    }

    fn args(&mut self, types: FragSlice<Ty>, args: FuncArgsAst, builder: &mut TirBuilder) {
        for (&ty, &arg) in self.typec.args[types].iter().zip(args.iter()) {
            let var = builder.create_var(false, ty, arg.name.span);
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

        push unknown_field(self, ty: Ty, fields: FragSlice<Field>, span: Span) {
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

        push unknown_spec_impl_func(self, func_span: Span, left: &[Ident]) {
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

        push missing_spec_impl_funcs(self, span: Span, missing: &[Ident]) {
            err: "missing spec functions";
            help: (
                "functions that are missing: {}",
                missing.iter()
                    .map(|&f| &self.interner[f])
                    .intersperse(", ")
                    .collect::<String>(),
            );
            (span, self.source) {
                err[span]: "this impl block does not implement all required functions";
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

        push missing_constructor_fields(self, span: Span, missing: &[Ident]) {
            err: "missing constructor fields";
            help: (
                "fields that are missing: {}",
                missing.iter()
                    .map(|&f| &self.interner[f])
                    .intersperse(", ")
                    .collect::<String>(),
            );
            info: "all fields must be initialized";
            (span, self.source) {
                err[span]: "this constructor does not initialize all required fields";
            }
        }

        push missing_constructor_params(self, span: Span, missing: BumpVec<usize>) {
            err: "cannot infer all type parameters of constructor";
            info: (
                "parameters that are missing: {}",
                missing
                    .iter()
                    .map(|&i| format!("parameters[{}]", i))
                    .intersperse(", ".into())
                    .collect::<String>(),
            );
            help: "syntax for specifying params: `T\\[T1, T2]\\{..}`";
            (span, self.source) {
                err[span]: "unable to infer all type parameters of this";
            }
        }

        push unexpected_params(self, span: Span) {
            err: "unexpected type parameters";
            help: "parameters are only allowed on constructors and function calls";
            (span, self.source) {
                err[span]: "this is not valid";
            }
        }

        push non_struct_field_access(self, ty: Ty, span: Span) {
            err: "cannot access field of non-struct type";
            info: ("found '{}'", self.typec.display_ty(ty, self.interner));
            (span, self.source) {
                err[span]: "when type checking this";
            }
        }

        push field_not_found(self, ty: FragRef<Struct>, name: NameAst) {
            err: (
                "field '{}' not found on '{}'",
                &self.interner[name.ident],
                self.typec.display_ty(Ty::Struct(ty), self.interner),
            );
            help: (
                "available fields: {}",
                self.typec[self.typec[ty].fields]
                    .iter()
                    .map(|f| &self.interner[f.name])
                    .intersperse(", ")
                    .collect::<String>(),
            );
            (name.span, self.source) {
                err[name.span]: "this field does not exist";
            }
        }

        push duplicate_double_dot(self, span: Span, prev: Span) {
            err: "duplicate '..'";
            (span, self.source) {
                err[span]: "this is a duplicate";
            }
            (prev, self.source) {
                err[prev]: "previous '..' already here";
            }
        }

        push missing_pat_ctor_fields(self, fields: BumpVec<Ident>, span: Span) {
            err: "missing fields in pattern";
            help: (
                "fields that are missing: {}",
                fields
                    .iter()
                    .map(|&f| &self.interner[f])
                    .intersperse(", ")
                    .collect::<String>(),
            );
            help: "if this is intentional, use '..' to ignore the missing fields";
            (span, self.source) {
                err[span]: "this pattern does not include all fields";
            }
        }

        push expected_struct_path(self, span: Span) {
            err: "expected struct path";
            (span, self.source) {
                err[span]: "this path does not lead to struct definition";
            }
        }
    }
}

enum DotPathResult {
    Field(u32, Ty),
}

enum FuncLookupResult<'a> {
    Func(FragRef<Func>),
    SpecFunc(FragRef<SpecFunc>, Ty),
    #[allow(dead_code)]
    Var(TirNode<'a>),
}
