use std::{default::default, iter};

use diags::*;
use lexing_t::*;
use packaging_t::*;
use parsing_t::*;
use scope::*;
use storage::*;
use type_checking_t::*;

use crate::*;

mod bounds;

type Expected = Option<VRef<Ty>>;

impl FuncParser<'_> {
    pub fn funcs(&mut self, funcs: impl IntoIterator<Item = (Maybe<Ident>, Ast, VRef<Def>)>) {
        for (self_alias, ast, def) in funcs {
            self.scope.self_alias = self_alias;
            let [_cc, generics, _name, ref args @ .., _ret, body] = self.ast_data[ast.children] else {
                unreachable!();
            };

            self.tir_data.clear();
            self.func_parser_ctx.current_fn = def.into();
            let Ok(body) = self.r#fn(body, generics, args, self.typec.defs[def].sig.ret.expand()) else {
                continue;
            };
            self.typec.defs[def].tir_data = self.tir_data.clone();
            self.typec.defs[def].body = body.into();
            self.scope.self_alias.take();
        }
    }

    fn r#fn(
        &mut self,
        body: Ast,
        generics: Ast,
        args: &[Ast],
        ret: Expected,
    ) -> errors::Result<VSlice<Tir>> {
        self.scope.start_frame();

        let parsed_generics = self
            .typec
            .params_of_def(self.func_parser_ctx.current_fn.unwrap());

        ty_parser!(self, self.current_file).push_generics(generics, parsed_generics);
        self.push_args(args);

        let TirKind::Block { stmts } = self.block(body, ret).kind else {
            unreachable!();
        };

        self.scope.end_frame();
        Ok(stmts)
    }

    fn block(&mut self, body: Ast, expected: Expected) -> Tir {
        let stmts = &self.ast_data[body.children];

        let mut last = Tir::default();

        let mut reserved = self.tir_data.reserve(stmts.len());
        for (i, &item) in stmts.iter().rev().enumerate().rev() {
            let expected = (i == 0).then_some(expected).flatten();
            last = self.expr(item, expected).unwrap_or_default();
            self.tir_data.push_to_reserved(&mut reserved, last);
            if last.terminating() {
                break;
            }
        }
        let stmts = self
            .tir_data
            .fill_reserved(reserved, Tir::new(TirKind::Unreachable));

        Tir {
            kind: TirKind::Block { stmts },
            ..last
        }
    }

    fn expr(&mut self, ast: Ast, expected: Expected) -> errors::Result<Tir> {
        let expr = match ast.kind {
            AstKind::Binary => self.binary(ast)?,
            AstKind::Return => self.r#return(ast)?,

            AstKind::String => Tir::new(TirKind::String).span(ast.span).ty(Ty::STR),
            AstKind::Int => self.int(ast, expected),
            AstKind::Ident | AstKind::IdentChain => self.ident(ast)?,
            AstKind::Call => self.call(ast, expected)?,

            kind => unimplemented!("{kind:?}"),
        };

        self.expect_type(expr, expected);

        Ok(expr)
    }

    fn call(&mut self, ast: Ast, expected: Expected) -> errors::Result<Tir> {
        let [caller, ref args @ ..] = self.ast_data[ast.children] else {
            unreachable!();
        };

        match caller.kind {
            AstKind::Ident | AstKind::IdentChain => self.proc_call(caller, args, expected),
            AstKind::InstanceExpr => {
                todo!()
            }
            AstKind::DotExpr => {
                todo!()
            }
            kind => unimplemented!("{kind:?}"),
        }
    }

    fn proc_call(&mut self, caller: Ast, args: &[Ast], expected: Expected) -> errors::Result<Tir> {
        let id = ty_parser!(self, self.current_file).ident_chain_id(caller);
        let def = self.get_from_scope(id, caller.span, "function", Reports::base)?;

        let (kind, ret) = match_scope_ptr!((self, def, caller.span) => {
            def: Def => {
                let Def { sig, generics, upper_generics, .. } = self.typec.defs[def];
                let (args, params, ret) = if self.typec.has_flag(def, DefFlags::GENERIC) {
                    self.parse_generic_args(args, sig, generics, upper_generics, expected)?
                } else {
                    (self.parse_args(args, sig.args)?, default(), sig.ret)
                };

                (TirKind::Call {
                    def,
                    params,
                    args,
                }, ret)
            },
            bound_func: BoundFunc => {
                let BoundFunc { sig, params, parent, .. } = self.typec.bound_funcs[bound_func];
                let (args, params, ret) = if !params.is_empty() || self.typec.has_flag(parent, BoundFlags::GENERIC) {
                    self.parse_generic_bound_args(args, sig, params, parent, expected)?
                } else {
                    (self.parse_args(args, sig.args)?, default(), sig.ret)
                };
                (TirKind::BoundCall {
                    bound_func,
                    params,
                    args,
                }, ret)
            },
        });

        Ok(Tir::new(kind).span(caller.span).ty(ret))
    }

    fn parse_generic_bound_args(
        &mut self,
        args: &[Ast],
        sig: Sig,
        params: VSlice<VRef<Bound>>,
        parent: VRef<Bound>,
        expected: Expected,
    ) -> errors::Result<(VSlice<Tir>, VSlice<VRef<Ty>>, Maybe<VRef<Ty>>)> {
        let bound_base: BoundBase = self.typec.bounds[parent].kind.downcast().unwrap();

        let parent_params = &self.typec.bound_lists[bound_base.generics];
        let parent_assoc_types = &self.typec.bound_lists[bound_base.assoc_types];
        let func_params = &self.typec.bound_lists[params];

        let bounds = iter::empty()
            .chain(parent_params)
            .chain(parent_assoc_types)
            .chain(func_params)
            .copied()
            .collect::<BumpVec<_>>();

        let mut infer_slots = bumpvec![Ty::INFERRED; bounds.len()];

        let types = self.typec.ty_lists[sig.args].to_bumpvec();

        if let (Some(expected), Some(ret)) = (expected, sig.ret.expand()) {
            bound_checker!(self)
                .infer_type(expected, ret, &mut infer_slots, &bounds)
                .unwrap();
        }

        let mut arg_parser = |(&arg, ty)| {
            let instance = ty_factory!(self).try_instantiate(ty, &infer_slots);
            let expr = self
                .expr(arg, instance)
                .map_err(|()| TyInferenceError::InvalidExpr)?;
            if let Some(expr_ty) = expr.ty.expand()
                && self.typec.has_flag(ty, TyFlags::GENERIC)
                && instance.map_or(true, |instance| self.typec.has_flag(instance, TyFlags::GENERIC))
            {
                bound_checker!(self).infer_type(expr_ty, ty, &mut infer_slots, &bounds).map(|()| expr)
            } else if instance != expr.ty.expand() {
                Err(TyInferenceError::Mismatch(instance.into(), expr.ty))
            } else {
                Ok(expr)
            }
        };

        let arg_results = args
            .iter()
            .zip(types)
            .map(|(arg, ty)| arg_parser((arg, ty)).map_err(|err| (*arg, err)))
            .collect::<BumpVec<_>>();

        let mut errors = arg_results.iter().filter_map(|expr| expr.err()).peekable();

        if errors.peek().is_some() {
            for (ast, err) in errors {
                let diag = match err {
                    TyInferenceError::InvalidExpr => continue,
                    TyInferenceError::Mismatch(a, b) => {
                        diag!(
                            (ast.span, self.current_file) => "expected '{}' but got '{}'" {
                                a.expand().map_or("nothing", |ty| &self.interner[self.typec.types.id(ty)]),
                                b.expand().map_or("nothing", |ty| &self.interner[self.typec.types.id(ty)]),
                            },
                        )
                    }
                    TyInferenceError::NotCompatible(a, b) => {
                        diag!(
                            (ast.span, self.current_file) => "type '{}' does not implement '{}'" {
                                &self.interner[self.typec.types.id(a)],
                                &self.interner[self.typec.types.id(b)],
                            },
                        )
                    }
                };

                self.workspace.push(diag);
            }

            return Err(());
        }

        let args = self
            .tir_data
            .bump(arg_results.into_iter().map(Result::unwrap));

        let mut missing_params = infer_slots
            .iter()
            .enumerate()
            .filter_map(|(i, &ty)| (ty == Ty::INFERRED).then_some(i))
            .peekable();

        if missing_params.peek().is_some() {
            return Err(());
        }

        let ret = sig.ret.expand().map(|ret| {
            ty_factory!(self)
                .try_instantiate(ret, &infer_slots)
                .unwrap()
        });

        Ok((args, self.typec.ty_lists.bump(infer_slots), ret.into()))
    }

    fn parse_generic_args(
        &mut self,
        _args: &[Ast],
        _sig: Sig,
        _params: VSlice<VRef<Bound>>,
        _upper_params: VSlice<VRef<Bound>>,
        _expected: Expected,
    ) -> errors::Result<(VSlice<Tir>, VSlice<VRef<Ty>>, Maybe<VRef<Ty>>)> {
        todo!()
    }

    fn parse_args(&mut self, args: &[Ast], types: VSlice<VRef<Ty>>) -> errors::Result<VSlice<Tir>> {
        let arg_types = self.typec.ty_lists[types].to_bumpvec();
        let args = args
            .iter()
            .zip(arg_types)
            .map(|(&arg, ty)| self.expr(arg, ty.into()))
            .collect::<BumpVec<_>>()
            .into_iter()
            .collect::<Result<BumpVec<_>, _>>()?;
        Ok(self.tir_data.bump(args))
    }

    fn binary(&mut self, ast: Ast) -> errors::Result<Tir> {
        let [lhs, op, rhs] = self.ast_data[ast.children] else {
            unreachable!();
        };

        let op_id = self.op_id(op, true);
        let def: VRef<Def> =
            self.get_from_scope_concrete(op_id, op.span, "binary operator", Reports::base)?;

        let sig = self.typec.defs[def].sig;
        let [left_ty, right_ty] = self.typec.ty_lists[sig.args] else {
            unreachable!();
        };

        let lhs = self.expr(lhs, Some(left_ty));
        let rhs = self.expr(rhs, Some(right_ty));
        let (lhs, rhs) = (lhs?, rhs?);
        let args = self.tir_data.bump([lhs, rhs]);

        Ok(Tir::new(TirKind::Call {
            def,
            params: default(),
            args,
        })
        .ty(sig.ret)
        .span(ast.span))
    }

    fn int(&mut self, ast: Ast, expected: Expected) -> Tir {
        let ty = {
            let mapping = [
                (Ty::I8, "i8"),
                (Ty::I16, "i16"),
                (Ty::I32, "i32"),
                (Ty::I64, "i64"),
                (Ty::U8, "u8"),
                (Ty::U16, "u16"),
                (Ty::U32, "u32"),
                (Ty::U64, "u64"),
                (Ty::UINT, "u"),
            ];

            mapping
                .into_iter()
                .find_map(|(ty, str)| span_str!(self, ast.span).ends_with(str).then_some(ty))
                .or(expected)
                .unwrap_or(Ty::INT)
        };

        Tir::new(TirKind::Int).ty(ty).span(ast.span)
    }

    fn ident(&mut self, ast: Ast) -> errors::Result<Tir> {
        let id = ty_parser!(self, self.current_file).ident_chain_id(ast);

        let something = self.get_from_scope(id, ast.span, "variable", Reports::base)?;

        let result = if let Some(var) = something.try_read::<Tir>() {
            Tir::new(TirKind::Access { var })
                .span(ast.span)
                .ty(self.tir_data[var].ty)
        } else {
            unimplemented!();
        };

        Ok(result)
    }

    fn r#return(&mut self, ast: Ast) -> errors::Result<Tir> {
        let [r#return] = self.ast_data[ast.children] else {
            unreachable!();
        };

        let expr = if r#return.kind.is_none() {
            Tir::new(TirKind::Unreachable)
        } else {
            let expected = self.typec.defs[self.func_parser_ctx.current_fn.unwrap()]
                .sig
                .ret;
            self.expr(r#return, expected.expand())?
        };

        let (value, _) = self.tir_data.bump_push(expr);

        Ok(Tir::new(TirKind::Return {
            value: value.into(),
        })
        .span(ast.span))
    }

    fn op_id(&mut self, op: Ast, binary: bool) -> Ident {
        let kind = if binary { "binary" } else { "unary" };
        match op.kind {
            AstKind::Operator => {
                self.interner
                    .intern(ident!(kind, "(", span_str!(self, op.span), ")"))
            }
            AstKind::OperatorWithModule => {
                let [op, module] = self.ast_data[op.children] else {
                    unreachable!();
                };

                self.interner.intern(ident!(
                    span_str!(self, module.span),
                    "`",
                    kind,
                    "(",
                    span_str!(self, op.span),
                    ")"
                ))
            }
            kind => unimplemented!("{:?}", kind),
        }
    }

    fn expect_type(&mut self, tir: Tir, expected: Expected) {
        if let (Some(got), Some(expected)) = (tir.ty.expand(), expected)
            && got != expected
        {
            self.workspace.push(diag! {
                (tir.span, self.current_file) error => "type mismatch",
                (none) => "expected {} but got {}" {
                    &self.interner[self.typec.types.id(expected)],
                    &self.interner[self.typec.types.id(got)],
                },
            })
        }
    }

    fn push_args(&mut self, args: &[Ast]) {
        let types = self.typec.args_of(self.func_parser_ctx.current_fn.unwrap());
        let mut reserved = self.tir_data.reserve(args.len());
        for (i, (arg, &ty)) in args
            .iter()
            .zip(self.typec.ty_lists[types].iter())
            .enumerate()
        {
            let [name, ..] = self.ast_data[arg.children] else {
                unreachable!();
            };

            let id = self.interner.intern_str(span_str!(self, name.span));
            let argument = Tir::new(TirKind::Argument(i as u8)).ty(ty).span(name.span);
            let arg = self.tir_data.push_to_reserved(&mut reserved, argument);
            self.scope.push(ScopeItem::new(
                id,
                arg,
                name.span,
                self.current_file,
                Vis::Priv,
            ));
        }
        self.tir_data.finish_reserved(reserved);
    }

    fn unwrap_impl(&self, id: VRef<Impl>) -> UnwrappedImpl {
        let ent = self.typec.impls[id];
        let BoundKind::Instance(instance) = self.typec.bounds[ent.bound].kind else {
            unreachable!();
        };
        let BoundKind::Base(base) = self.typec.bounds[instance.base].kind else {
            unreachable!();
        };

        UnwrappedImpl {
            instance,
            base,
            id,
            ent,
        }
    }

    scope_error_handler!();
}

#[derive(Clone, Copy)]
struct UnwrappedImpl {
    instance: BoundInstance,
    base: BoundBase,
    id: VRef<Impl>,
    ent: Impl,
}
