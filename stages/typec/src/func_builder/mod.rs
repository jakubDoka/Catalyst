use std::{cell::Cell, default::default};

use diags::*;
use lexing_t::*;
use packaging_t::span_str;
use parsing::*;
use parsing_t::*;
use scope::*;
use storage::*;
use typec_t::*;

use crate::{item_collector::FuncDefs, *};

pub type TypeCheckedFuncs<'a> = BumpVec<(VRef<Func>, Option<BodyTir<'a>>)>;

impl TyChecker<'_> {
    pub fn build_funcs<'a>(
        &mut self,
        arena: &'a Arena,
        funcs: &mut FuncDefs,
        compiled_funcs: &mut TypeCheckedFuncs<'a>,
    ) {
        let mut builder = TirBuilderCtx::new();
        for (ast, func) in funcs.drain(..) {
            let Some(res) = self.build_func(ast, func, arena, &mut builder) else {
                compiled_funcs.push((func, None));
                continue;
            };
            let Some(body) = res else {
                self.incomplete_tir(ast);
                continue;
            };
            compiled_funcs.push((func, Some(body)));
        }
    }

    pub fn build_func<'a>(
        &mut self,
        FuncDefAst {
            signature: FuncSigAst { generics, args, .. },
            body,
            ..
        }: FuncDefAst,
        func: VRef<Func>,
        arena: &'a Arena,
        builder: &mut TirBuilderCtx<'a>,
    ) -> Option<Option<BodyTir<'a>>> {
        self.scope.start_frame();
        let Func { signature, .. } = self.typec.funcs[func];

        self.insert_generics(generics, 0, false);
        let args = self.args(signature.args, args, arena, builder);

        let root_block = builder.create_block(BlockInputTir::FuncArgs(args));

        let mut active_builder = TirBuilder::new(arena, builder, signature.ret, root_block);

        match body {
            FuncBodyAst::Arrow(span, expr) => {
                self.r#return(Some(expr), span, &mut active_builder)?
            }
            FuncBodyAst::Block(_block) => todo!(), //self.block(block, Inference::Known(signature.ret), builder),
            FuncBodyAst::Extern(..) => return None,
        };

        Some(active_builder.build())
    }

    fn r#return<'a>(
        &mut self,
        expr: Option<ExprAst>,
        span: Span,
        builder: &mut TirBuilder<'a, '_>,
    ) -> Option<ValueTir<'a>> {
        let value = if let Some(expr) = expr {
            Some(self.expr(expr, Some(builder.ret), builder)?)
        } else {
            None
        };

        let span = expr.map_or(span, |expr| span.joined(expr.span()));
        self.type_check(builder.ret, value.map_or(Ty::UNIT, |v| v.ty.get()), span)?;

        let ret = ControlFlowTir::Return(value);
        builder.close_block(ret);

        Some(builder.terminal())
    }

    fn expr<'a>(
        &mut self,
        expr: ExprAst,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> Option<ValueTir<'a>> {
        let value = match expr {
            ExprAst::Unit(&unit) => self.unit_expr(unit, inference, builder),
            ExprAst::Binary(&binary) => self.binary_expr(binary, builder),
        }?;

        if let Some(ty) = inference {
            self.type_check(ty, value.ty.get(), expr.span())?;
        }

        Some(value)
    }

    fn unit_expr<'a>(
        &mut self,
        unit_ast: UnitExprAst,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> Option<ValueTir<'a>> {
        match unit_ast {
            UnitExprAst::Path(path) => self.path(path, inference, builder),
            UnitExprAst::Return(ReturnExprAst { return_span, expr }) => {
                self.r#return(expr, return_span, builder)
            }
            UnitExprAst::Int(span) => self.int(span, inference, builder),
        }
    }

    fn path<'a>(
        &mut self,
        path: PathAst,
        _inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> Option<ValueTir<'a>> {
        use PathSegmentAst::*;
        match *path.segments {
            [Name(name)] => self
                .lookup_typed::<VarLookup>(name.ident, name.span)
                .map(|var| builder[var]),
            [] => unreachable!("handled in parsing stage"),
            _ => self.invalid_expr_path(path.span())?,
        }
    }

    fn int<'a>(
        &mut self,
        span: Span,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> Option<ValueTir<'a>> {
        let span_str = span_str!(self, span);
        let ty = Ty::INTEGERS
            .iter()
            .find(|&&ty| span_str.ends_with(&self.interner[self.typec.types.id(ty)]))
            .copied()
            .or(inference)
            .unwrap_or(Ty::UINT);
        Some(builder.inst(ty, InstTir::Int(span)))
    }

    fn binary_expr<'a>(
        &mut self,
        BinaryExprAst { lhs, op, rhs }: BinaryExprAst,
        builder: &mut TirBuilder<'a, '_>,
    ) -> Option<ValueTir<'a>> {
        let lhs = self.expr(lhs, None, builder)?;
        let rhs = self.expr(rhs, None, builder)?;

        let id = self
            .typec
            .binary_op_id(op.ident, lhs.ty.get(), rhs.ty.get());
        let id = self.interner.intern(id);
        let func = self.lookup_typed::<OpLookup>(id, op.span)?;

        let call = CallTir {
            func: FuncTir::Concrete(func),
            params: default(),
            args: builder.arena.alloc_slice(&[lhs, rhs]),
        };
        let ty = self.typec.funcs[func].signature.ret;
        Some(builder.inst(ty, call))
    }

    fn args<'a>(
        &mut self,
        types: VSlice<VRef<Ty>>,
        args: FuncArgsAst,
        arena: &'a Arena,
        builder: &mut TirBuilderCtx<'a>,
    ) -> &'a [ValueTir<'a>] {
        let mut values = bumpvec![cap args.len()];
        for (&ty, &arg) in self.typec.ty_slices[types].iter().zip(args.iter()) {
            let ty = arena.alloc(Cell::new(ty));
            let value = ValueTir { inst: None, ty };
            values.push(value);

            let var = builder.create_var(value);
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
        arena.alloc_slice(&values)
    }

    fn type_check(&mut self, expected: VRef<Ty>, got: VRef<Ty>, span: Span) -> Option<()> {
        if Ty::compatible(expected, got) {
            return Some(());
        }

        self.ty_mismatch(expected, got, span);

        None
    }

    gen_error_fns! {
        push incomplete_tir(self, func: FuncDefAst) {
            err: "not all blocks were closed when typechecking function";
            info: "this is a bug in the compiler, please report it";
            (func.span(), self.current_file) {
                info[func.signature.name.span]: "happened in this function";
            }
        }

        push ty_mismatch(self, expected: VRef<Ty>, got: VRef<Ty>, span: Span) {
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
    VarLookup<"variable", Var> {}
);
