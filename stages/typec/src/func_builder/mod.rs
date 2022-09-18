use diags::{gen_error_fn, gen_error_fns};
use lexing_t::Span;
use parsing::{
    BinaryExprAst, ExprAst, FuncArgsAst, FuncBodyAst, FuncDefAst, FuncSigAst, GenericsAst,
    UnitExprAst,
};
use parsing_t::Ast;
use scope::{ScopeItem, Vis};
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
        let mut builder = BodyTirBuilder::new(arena);
        for (ast, func) in funcs.drain(..) {
            if self.build_func(ast, func, &mut builder) {
                compiled_funcs.push((func, None));
                continue;
            }

            let Some(body) = builder.build() else {
                self.incomplete_tir(ast);
                continue;
            };
            compiled_funcs.push((func, Some(body)));
        }
    }

    pub fn build_func(
        &mut self,
        FuncDefAst {
            signature:
                FuncSigAst {
                    generics,
                    name,
                    args,
                    ret,
                    ..
                },
            body,
            ..
        }: FuncDefAst,
        func: VRef<Func>,
        builder: &mut BodyTirBuilder,
    ) -> bool {
        self.scope.start_frame();
        let Func { signature, .. } = self.typec.funcs[func];
        builder.ret = signature.ret;

        self.insert_generics(generics, 0, false);
        self.args(signature.args, args, builder);

        match body {
            FuncBodyAst::Arrow(.., expr) => self.expr(expr, Some(signature.ret), builder),
            FuncBodyAst::Block(block) => todo!(), //self.block(block, Inference::Known(signature.ret), builder),
            FuncBodyAst::Extern(..) => return true,
        };

        false
    }

    fn expr<'a>(
        &mut self,
        expr_ast: ExprAst,
        inference: Inference,
        builder: &mut BodyTirBuilder<'a>,
    ) -> Option<ValueTir<'a>> {
        let expr = match expr_ast {
            ExprAst::Unit(&unit) => self.unit_expr(unit, inference, builder),
            ExprAst::Binary(&binary) => self.binary_expr(binary, builder),
        }?;

        if let Some(ty) = inference && expr.ty.get() != ty {
            self.ty_mismatch(ty, expr.ty.get(), expr_ast.span());
            return None;
        }

        Some(expr)
    }

    fn unit_expr<'a>(
        &mut self,
        unit_ast: UnitExprAst,
        inference: Inference,
        builder: &mut BodyTirBuilder<'a>,
    ) -> Option<ValueTir<'a>> {
        todo!()
    }

    fn binary_expr<'a>(
        &mut self,
        BinaryExprAst { lhs, op, rhs }: BinaryExprAst,
        builder: &mut BodyTirBuilder<'a>,
    ) -> Option<ValueTir<'a>> {
        let lhs = self.expr(lhs, None, builder)?;
        let rhs = self.expr(rhs, None, builder)?;

        let id = self
            .typec
            .binary_op_id(op.ident, lhs.ty.get(), rhs.ty.get());
        let id = self.interner.intern(id);

        todo!();
    }

    fn args(&mut self, types: VSlice<VRef<Ty>>, args: FuncArgsAst, builder: &mut BodyTirBuilder) {
        for (&ty, &arg) in self.typec.ty_slices[types].iter().zip(args.iter()) {
            let ty = builder.ty(ty);
            let value = ValueTir { inst: None, ty };
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
    }
}

pub type Inference = Option<VRef<Ty>>;
