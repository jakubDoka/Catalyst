use std::default::default;

use diags::*;
use lexing_t::*;
use packaging_t::span_str;
use parsing::*;
use parsing_t::*;
use scope::*;
use storage::*;
use typec_t::*;

use crate::*;

macro_rules! dispatch_item {
    ($lookup:ident $self:expr, $id:expr, $span:expr =>
        $(
            $value:ident: $ty:ty => $expr:expr,
        )*
    ) => {
        {
            let item = $self.lookup::<$lookup>($id, $span)?;
            match_scope_ptr!(item =>
                $(
                    $value: $ty => $expr,
                )*
                _ => $self.handle_scope_error::<$lookup>(ScopeError::TypeMismatch(item.id), $id, $span)?,
            )
        }
    };
}

pub type ExprRes<'a> = Option<TypedTirNode<'a>>;

impl TyChecker<'_> {
    pub fn build_funcs<'a>(
        &mut self,
        items: GroupedItemSlice<FuncDefAst>,
        arena: &'a Arena,
        input: &TypecOutput<Func>,
        compiled_funcs: &mut &'a [(VRef<Func>, TirNode<'a>)],
        extern_funcs: &mut Vec<VRef<Func>>,
    ) -> &mut Self {
        let iter =
            input
                .iter()
                .map(|&(i, func)| (items[i], func))
                .filter_map(|((ast, ..), func)| {
                    let Some(res) = self.build_func(ast, func, arena) else {
                    extern_funcs.push(func);
                    return None;
                };

                    let Some(body) = res else {
                    self.incomplete_tir(ast);
                    return None;
                };

                    Some((func, body))
                });

        *compiled_funcs = arena.alloc_iter(iter.collect::<BumpVec<_>>());

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
        let mut builder = TirBuilder::new(arena, signature.ret, ret.map(|ret| ret.span()).into());

        self.insert_generics(generics, 0);
        self.args(signature.args, args, &mut builder);

        let tir_body = match body {
            FuncBodyAst::Arrow(.., expr) => self.expr(expr, Some(signature.ret), &mut builder),
            FuncBodyAst::Block(body) => self.block(body, Some(signature.ret), &mut builder),
            FuncBodyAst::Extern(..) => return None,
        };

        Some(
            tir_body
                .and_then(|tir_body| {
                    if tir_body.ty == Ty::TERMINAL {
                        Some(tir_body)
                    } else {
                        self.return_low(Some(tir_body), body.span(), &mut builder)
                    }
                })
                .map(|tir_body| tir_body.node),
        )
    }

    fn block<'a>(
        &mut self,
        block: BlockAst,
        inference: Inference,
        builder: &mut TirBuilder<'a>,
    ) -> ExprRes<'a> {
        let frame = builder.start_frame();
        self.scope.start_frame();

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

        self.scope.end_frame();

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
            UnitExprAst::Call(&call) => self.call(call, inference, builder),
        }
    }

    fn call<'a>(
        &mut self,
        call @ CallExprAst { callable, args }: CallExprAst,
        _inference: Inference,
        builder: &mut TirBuilder<'a>,
    ) -> ExprRes<'a> {
        match callable {
            UnitExprAst::Path(path) => {
                let func = self.func_path(path, builder)?;
                match func {
                    Ok(direct) => self.direct_concrete_call(direct, args, call.span(), builder),
                    Err(_pointer) => todo!(),
                }
            }
            UnitExprAst::Return(..) | UnitExprAst::Call(..) | UnitExprAst::Int(..) => {
                todo!()
            }
        }
    }

    fn direct_concrete_call<'a>(
        &mut self,
        func: VRef<Func>,
        ast_args: CallArgsAst,
        span: Span,
        builder: &mut TirBuilder<'a>,
    ) -> ExprRes<'a> {
        let Func { signature, .. } = self.typec.funcs[func];

        let types = self.typec.ty_slices[signature.args].to_bumpvec();
        let args = ast_args
            .iter()
            .zip(types)
            .map(|(&arg, ty)| self.expr(arg, Some(ty), builder).map(|arg| arg.node))
            .collect::<Option<BumpVec<_>>>()?;
        let args = builder.arena.alloc_slice(&args);

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
        _builder: &mut TirBuilder<'a>,
    ) -> Option<Result<VRef<Func>, TirNode<'a>>> {
        match *segments {
            [] => dispatch_item!(FuncLookup self, start.ident, start.span =>
                func: Func => Some(Ok(func)),
                _var: Var => todo!(),
            ),
            _ => self.invalid_expr_path(path.span())?,
        }
    }

    fn value_path<'a>(
        &mut self,
        path @ PathExprAst { start, segments }: PathExprAst,
        _inference: Inference,
        builder: &mut TirBuilder<'a>,
    ) -> ExprRes<'a> {
        match *segments {
            [] => dispatch_item!(ValueLookup self, start.ident, start.span =>
                var: Var => self.node(builder.get_var(var).ty, AccessTir {
                    span: path.span(),
                    ty: builder.get_var(var).ty,
                    var
                }, builder),
                _func: Func => todo!(),
            ),
            _ => self.invalid_expr_path(path.span())?,
        }
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

    fn binary_expr<'a>(
        &mut self,
        binary_ast @ BinaryExprAst { lhs, op, rhs }: BinaryExprAst,
        builder: &mut TirBuilder<'a>,
    ) -> ExprRes<'a> {
        let lhs = self.expr(lhs, None, builder);
        let rhs = self.expr(rhs, None, builder);
        let (lhs, rhs) = (lhs?, rhs?); // recovery

        let id = self.typec.binary_op_id(op.ident, lhs.ty, rhs.ty);
        let id = self.interner.intern(id);
        let func = self.lookup_typed::<OpLookup>(id, op.span)?;

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

            let var = builder.create_var(value, ty);
            let item = ScopeItem::new(
                arg.name.ident,
                var,
                arg.name.span,
                arg.name.span,
                self.current_file,
                Vis::Priv,
            );
            self.scope.push(item);
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
            (span, self.current_file) {
                info[span]: "this is unreachable";
                info[because]: "because of this";
            }
        }

        push incomplete_tir(self, func: FuncDefAst) {
            err: "not all blocks were closed when typechecking function";
            info: "this is a bug in the compiler, please report it";
            (func.span(), self.current_file) {
                info[func.signature.name.span]: "happened in this function";
            }
        }

        push generic_ty_mismatch(self, expected: VRef<Ty>, got: VRef<Ty>, span: Span) {
            err: "type mismatch";
            info: ("expected {} but got {}", self.type_diff(expected, got), self.type_diff(got, expected));
            (span, self.current_file) {
                err[span]: "mismatch occurred here";
            }
        }

        push invalid_expr_path(self, span: Span) {
            err: "invalid expression path";
            info: "expected format: <ident> |";
            (span, self.current_file) {
                err[span]: "found here";
            }
        }
    }
}

pub type Inference = Option<VRef<Ty>>;

gen_scope_lookup!(
    OpLookup<"operator", Func> {}
    ValueLookup<"variable or function"> {}
    FuncLookup<"function or variable"> {}
);
