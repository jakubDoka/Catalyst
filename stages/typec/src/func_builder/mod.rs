use std::default::default;

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
        items: GroupedItemSlice<ImplAst>,
        arena: &'a Arena,
        input: &[(usize, usize, VRef<Func>)],
        compiled_funcs: &mut Vec<(VRef<Func>, TirNode<'a>)>,
        extern_funcs: &mut Vec<VRef<Func>>,
    ) -> &mut Self {
        let iter = input
            .iter()
            .map(|&(i, j, func)| (items[i].0.body[j].value, func))
            .filter_map(|(ast, func)| {
                let ImplItemAst::Func(&ast) = ast;
                let res = self.build_func(ast, func, arena)?;

                let Some(body) = res else {
                    extern_funcs.push(func);
                    return None;
                };

                Some((func, body))
            });

        compiled_funcs.extend(iter);

        self
    }

    pub fn build_funcs<'a>(
        &mut self,
        items: GroupedItemSlice<FuncDefAst>,
        arena: &'a Arena,
        input: &TypecOutput<Func>,
        compiled_funcs: &mut Vec<(VRef<Func>, TirNode<'a>)>,
        extern_funcs: &mut Vec<VRef<Func>>,
    ) -> &mut Self {
        let iter =
            input
                .iter()
                .map(|&(i, func)| (items[i], func))
                .filter_map(|((ast, ..), func)| {
                    let res = self.build_func(ast, func, arena)?;

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
    ) -> Option<Option<TirNode<'a>>> {
        self.scope.start_frame();
        let Func { signature, .. } = self.typec.funcs[func];
        let mut builder = TirBuilder::new(arena, signature.ret, ret.map(|ret| ret.span()));

        self.insert_generics(generics, 0);
        self.args(signature.args, args, &mut builder);

        let tir_body = match body {
            FuncBodyAst::Arrow(.., expr) => self.expr(expr, Some(signature.ret), &mut builder),
            FuncBodyAst::Block(body) => self.block(body, Some(signature.ret), &mut builder),
            FuncBodyAst::Extern(..) => return Some(None),
        }?;

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
            UnitExprAst::StructConstructor(_) => todo!(),
            UnitExprAst::DotExpr(_) => todo!(),
        }
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
        call @ CallExprAst { callable, args }: CallExprAst,
        inference: Inference,
        builder: &mut TirBuilder<'a>,
    ) -> ExprRes<'a> {
        let args = args.iter().copied();
        let res = try {
            match callable {
                UnitExprAst::Path(path) => (self.func_path(path, builder)?, None),
                UnitExprAst::DotExpr(&DotExprAst { lhs, rhs, .. }) => {
                    let lhs = self.unit_expr(lhs, None, builder)?;
                    let func = self.method_path(lhs.ty, rhs, builder)?;
                    (func, Some(lhs))
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
        };

        let Some((func_res, caller)) = res else {
            // more diagnostics
            for arg in args {
                self.expr(arg, None, builder);
            }

            return None;
        };

        match func_res {
            FuncLookupResult::Func(func) if self.typec[func].is_generic() => {
                self.direct_generic_call(func, caller, args, call.span(), inference, builder)
            }
            FuncLookupResult::Func(func) => {
                self.direct_concrete_call(func, caller, args, call.span(), builder)
            }
            FuncLookupResult::Var(_) => todo!(),
        }
    }

    fn direct_generic_call<'a>(
        &mut self,
        func: VRef<Func>,
        caller: Option<TypedTirNode<'a>>,
        args: impl Iterator<Item = ExprAst>,
        span: Span,
        inference: Inference,
        builder: &mut TirBuilder<'a>,
    ) -> ExprRes<'a> {
        let Func {
            generics,
            upper_generics,
            signature,
            ..
        } = self.typec.funcs[func];

        let mut params = bumpvec![None; self.typec.spec_slices[generics].len()
            + self.typec.spec_slices[upper_generics].len()];

        if let Some(inference) = inference {
            self.infer_params(&mut params, inference, signature.ret, span)?;
        }

        let types = self.typec.ty_slices[signature.args].to_bumpvec();

        if let Some(caller) = caller && let Some(&first) = types.first() {
            self.infer_params(&mut params, caller.ty, first, span);
        }

        let args = args
            .zip(types.into_iter().skip(caller.is_some() as usize))
            .map(|(arg, ty)| {
                let inferred = self.typec.try_instantiate(ty, &params, self.interner);
                let expr = self.expr(arg, inferred, builder)?;

                if inferred.is_none() {
                    self.infer_params(&mut params, expr.ty, ty, arg.span())?;
                }

                Some(expr.node)
            });
        let args = caller
            .map(|caller| caller.node)
            .into_iter()
            .map(Some)
            .chain(args)
            .nsc_collect::<Option<BumpVec<_>>>()?;

        let Some(params) = params.iter().copied().nsc_collect::<Option<BumpVec<_>>>() else {
            todo!()
        };

        let args = builder.arena.alloc_iter(args);
        let params = builder.arena.alloc_iter(params);
        let ty = self.typec.instantiate(signature.ret, params, self.interner);

        let call = CallTir {
            func: CallableTir::Func(func),
            args,
            span,
            params,
            ty,
        };

        self.node(ty, call, builder)
    }

    fn infer_params(
        &mut self,
        params: &mut [Option<VRef<Ty>>],
        reference: VRef<Ty>,
        template: VRef<Ty>,
        span: Span,
    ) -> Option<()> {
        let mut stack = bumpvec![(reference, template)];

        while let Some((reference, template)) = stack.pop() {
            if reference == template {
                continue;
            }

            match (
                self.typec.types[reference].kind,
                self.typec.types[template].kind,
            ) {
                (TyKind::Pointer(reference), TyKind::Pointer(template)) => {
                    stack.push((reference.base, template.base));
                    stack.push((reference.mutability, template.mutability));
                }
                (TyKind::Instance(reference), TyKind::Instance(template)) => {
                    self.type_check(template.base, reference.base, span)?;
                    stack.extend(
                        self.typec.ty_slices[reference.args]
                            .iter()
                            .copied()
                            .zip(self.typec.ty_slices[template.args].iter().copied()),
                    );
                }
                (_, TyKind::Param(index)) if let Some(inferred) = params[index as usize] => {
                    self.type_check(inferred, reference, span)?;
                }
                (_, TyKind::Param(index)) => params[index as usize] = Some(reference),
                _ => self.generic_ty_mismatch(reference, template, span)?,
            }
        }

        Some(())
    }

    fn direct_concrete_call<'a>(
        &mut self,
        func: VRef<Func>,
        caller: Option<TypedTirNode<'a>>,
        ast_args: impl Iterator<Item = ExprAst>,
        span: Span,
        builder: &mut TirBuilder<'a>,
    ) -> ExprRes<'a> {
        let Func { signature, .. } = self.typec.funcs[func];

        let types = self.typec.ty_slices[signature.args].to_bumpvec();

        if let Some(caller) = caller && let Some(&first) = types.first() {
            // TODO: Auto deref or ref.
            self.type_check(first, caller.ty, span);
        }

        let args = ast_args
            .zip(types.into_iter().skip(caller.is_some() as usize))
            .map(|(arg, ty)| self.expr(arg, Some(ty), builder).map(|arg| arg.node));
        let args = caller
            .map(|caller| caller.node)
            .into_iter()
            .map(Some)
            .chain(args)
            .nsc_collect::<Option<BumpVec<_>>>()?;
        let args = builder.arena.alloc_iter(args);

        let call = CallTir {
            func: CallableTir::Func(func),
            args,
            span,
            params: &[],
            ty: signature.ret,
        };

        self.node(signature.ret, call, builder)
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

        let &func_or_type = segments
            .first()
            .or_else(|| self.invalid_expr_path(path.span())?)?;

        let id = self
            .interner
            .intern(scoped_ident!(module.as_u32(), func_or_type.ident));
        if let Some(index) = self.typec.funcs.index(id) {
            return Some(FuncLookupResult::Func(index));
        }

        let ty = self
            .typec
            .types
            .index(id)
            .or_else(|| self.scope_error(ScopeError::NotFound, id, path.span(), TY)?)?;

        let (&start, segments) = segments[1..]
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
        match *segments {
            [] => {
                let ty_id = self.typec.types.id(ty);
                let id = self.interner.intern(scoped_ident!(ty_id, start.ident));
                Some(FuncLookupResult::Func(lookup!(Func self, id, path.span())))
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
    }
}

pub type Inference = Option<VRef<Ty>>;

enum FuncLookupResult<'a> {
    Func(VRef<Func>),
    #[allow(dead_code)]
    Var(TypedTirNode<'a>),
}
