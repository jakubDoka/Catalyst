use std::default::default;

use diags::*;
use lexing_t::*;
use packaging_t::*;
use parsing_t::*;
use scope::*;
use storage::*;
use type_checking_t::*;

use crate::*;

type Expected = Option<Ty>;

impl FuncParser<'_> {
    pub fn bound_impls(&mut self, impls: impl IntoIterator<Item = (AstEnt, Impl)>) {
        for (ast, r#impl) in impls {
            let [generics, .., body] = self.ast_data[ast.children] else {
                unreachable!();
            };

            let ImplEnt {
                id,
                params,
                implementor,
                ..
            } = self.typec.impls[r#impl];

            self.scope.start_frame();
            self.scope.self_alias = self.typec.types[implementor].loc.name.into();
            ty_parser!(self, self.current_file).push_generics(generics, params);

            for &item in &self.ast_data[body.children] {
                match item.kind {
                    AstKind::ImplType => drop(self.impl_type(item, r#impl)),
                    AstKind::ImplUse | AstKind::Func { .. } => (),
                    kind => unimplemented!("{:?}", kind),
                }
            }

            let mut funcs = vec![Maybe::none(); self.typec.func_count_of_impl(r#impl)];

            for &item in &self.ast_data[body.children] {
                match item.kind {
                    AstKind::Func { .. } => {
                        drop(self.bound_impl_func(item, r#impl, params, &mut funcs))
                    }
                    AstKind::ImplUse => drop(self.bound_impl_use(item, r#impl, &mut funcs)),
                    AstKind::ImplType => (),
                    kind => unimplemented!("{:?}", kind),
                }
            }

            self.scope.end_frame();

            let mut missing = funcs
                .iter_mut()
                .enumerate()
                .filter(|(.., maybe)| maybe.is_none())
                .filter_map(|(i, maybe)| {
                    let name = self.typec.func_of_impl(r#impl, i).loc.name;

                    let implementor_name = self.typec.types[implementor].loc.name;
                    let local_id = self.interner.intern(scoped_ident!(implementor_name, name));
                    if let Ok((def, ..)) = self.scope.get_concrete::<Def>(local_id) {
                        let sig = self.typec.defs[def].sig;
                        if bound_checker!(self)
                            .compare_bound_signatures(i, default(), sig, r#impl)
                            .is_ok()
                        {
                            let id = self.interner.intern(scoped_ident!(id, name));
                            let func = self.typec.wrap_def(id, def);
                            *maybe = func.into();
                            return None;
                        }
                    }

                    Some(name)
                })
                .peekable();

            if missing.peek().is_some() {
                let suggestions = missing
                    .collect::<Vec<_>>()
                    .into_iter()
                    .map(|name| &self.interner[name])
                    .collect::<Vec<_>>()
                    .join(", ");

                let loc = self
                    .typec
                    .loc_of(self.typec.impls[r#impl].bound, self.interner);
                self.workspace.push(diag! {
                    (ast.span, self.current_file) => "some of the methods are not implemented",
                    (none) => "missing methods: {}" { suggestions },
                    (exp loc) => "related bound type",
                });
                continue;
            }

            self.typec.impls[r#impl].funcs = self
                .typec
                .func_lists
                .bump(funcs.into_iter().map(Maybe::unwrap));
        }
    }

    fn bound_impl_use(
        &mut self,
        ast: AstEnt,
        r#impl: Impl,
        funcs: &mut Vec<Maybe<Func>>,
    ) -> errors::Result {
        let &[value, ast_name] = &self.ast_data[ast.children] else {
            unreachable!();
        };

        let name = span_str!(self, ast_name.span);
        let name_ident = self.interner.intern_str(name);
        let id = self
            .interner
            .intern(scoped_ident!(self.typec.impls[r#impl].id, name_ident));

        let Some(index) = self.typec.func_index_of_impl(r#impl, name_ident) else {
            let loc = self.typec.loc_of(self.typec.impls[r#impl].bound, self.interner);
            self.workspace.push(diag! {
                (ast_name.span, self.current_file) => "function with this name does not exist for the bound",
                (exp loc) => "related bound defined here"
            });
            return Err(());
        };

        if let Some(already) = funcs[index].expand() {
            let loc = self.typec.loc_of_func(already, self.interner);
            self.workspace.push(diag! {
                (ast_name.span, self.current_file) => "function with this name is already implemented",
                (exp loc) => "the function",
            });
            return Err(());
        }

        let func = match value.kind {
            AstKind::TyInstance => {
                let [header, ref args @ ..] = self.ast_data[value.children] else {
                    unreachable!();
                };

                let local_id = ty_parser!(self, self.current_file).ident_chain_id(header);
                let def: Def =
                    self.get_from_scope_concrete(local_id, value.span, "function", Reports::base)?;
                let params = args
                    .iter()
                    .map(|&arg| ty_parser!(self, self.current_file).parse(arg))
                    .collect::<Result<Vec<_>, _>>()?;
                self.typec.wrap_def_with_params(id, def, params)
            }
            AstKind::Ident | AstKind::IdentChain => {
                let local_id = ty_parser!(self, self.current_file).ident_chain_id(value);
                let def: Def =
                    self.get_from_scope_concrete(local_id, value.span, "function", Reports::base)?;
                self.typec.wrap_def(id, def)
            }
            kind => unimplemented!("{:?}", kind),
        };

        let sig = self.typec.sig_of_func(func);
        let params = self.typec.funcs[func].params;
        if let Err(err) = bound_checker!(self).compare_bound_signatures(index, params, sig, r#impl)
        {
            self.report_signature_mismatch(err, r#impl, ast_name.span);
            return Err(());
        }

        funcs[index] = Maybe::some(func);

        Ok(())
    }

    fn impl_type(&mut self, ast: AstEnt, r#impl: Impl) {
        let ImplEnt {
            implementor, bound, ..
        } = self.typec.impls[r#impl];
        let base_bound = self.typec.instance_base_of(bound);
        let [_generics, ast_name, ..] = self.ast_data[ast.children] else {
            unreachable!();
        };

        let name = span_str!(self, ast_name.span);
        let name_ident = self.interner.intern_str(name);
        let implementor_name = self.typec.types[implementor].loc.name;
        let local_id = self
            .interner
            .intern(scoped_ident!(implementor_name, name_ident));

        let index = self
            .typec
            .bound_assoc_ty_index(base_bound, name_ident)
            .expect("this code should not be reachable if the bound is malformed");

        let ty = self.typec.bound_assoc_ty_at(base_bound, index);

        self.scope.push(ScopeItem::new(
            local_id,
            ty,
            ast_name.span,
            self.current_file,
            Vis::Pub,
        ))
    }

    fn bound_impl_func(
        &mut self,
        item: AstEnt,
        r#impl: Impl,
        upper_params: Maybe<TyList>,
        funcs: &mut Vec<Maybe<Func>>,
    ) -> errors::Result {
        let [cc, generics, ast_name, ref args @ .., ret, body] = self.ast_data[item.children] else {
            unreachable!();
        };

        self.scope.start_frame();
        let params = ty_parser!(self, self.current_file).bounded_generics(generics)?;
        let sig = ty_parser!(self, self.current_file).sig(cc, args, ret)?;

        let name = span_str!(self, ast_name.span);
        let name_id = self.interner.intern_str(name);
        let impl_id = self.typec.impls[r#impl].id;
        let id = self.interner.intern(scoped_ident!(impl_id, name_id));

        let Some(index) = self.typec.func_index_of_impl(r#impl, name_id) else {
            let loc = self.typec.loc_of(self.typec.impls[r#impl].bound, self.interner);
            self.workspace.push(diag! {
                (ast_name.span, self.current_file) => "function with this name does not exist for the bound",
                (exp loc) => "related bound defined here"
            });
            return Err(());
        };

        if let Err(err) =
            bound_checker!(self).compare_bound_signatures(index, default(), sig, r#impl)
        {
            self.report_signature_mismatch(err, r#impl, ast_name.span);
            return Err(());
        }

        if let Some(already) = funcs[index].expand() {
            let loc = self.typec.loc_of_func(already, self.interner);
            self.workspace.push(diag! {
                (ast_name.span, self.current_file) => "function with this name is already implemented",
                (exp loc) => "previously defined here",
            });
            return Ok(());
        }

        let def_ent = DefEnt {
            params,
            flags: FuncFlags::GENERIC & params.is_some(),
            loc: Loc::new(
                ast_name.span,
                self.current_file,
                self.interner.intern_str(name),
            ),
            sig,
            upper_params,
            ..default()
        };
        let def = self.typec.defs.push(def_ent);

        let func = self.typec.wrap_def(id, def);
        funcs[index] = Maybe::some(func);

        self.tir_data.clear();
        self.func_parser_ctx.current_fn = def.into();
        let body = self.r#fn(body, generics, args, self.typec.defs[def].sig.ret.expand())?;
        self.typec.defs[def].tir_data = self.tir_data.clone();
        self.typec.defs[def].body = body.into();

        Ok(())
    }

    pub fn report_signature_mismatch(&mut self, err: SignatureError, r#impl: Impl, span: Span) {
        let loc = self
            .typec
            .loc_of(self.typec.impls[r#impl].bound, self.interner);
        let diag = match err {
            SignatureError::CallConv(expected, got) => diag!(
                (span, self.current_file) => "call convention mismatch, expected {} but got {}" {
                    expected.expand().map_or("\"default\"", |cc| &self.interner[cc]),
                    got.expand().map_or("\"default\"", |cc| &self.interner[cc]),
                },
                (exp loc) => "related bound defined here",
            ),
            SignatureError::ArgCount(expected, got) => diag!(
                (span, self.current_file) => "argument count mismatch, expected {} but got {}" {
                    expected,
                    got,
                },
                (exp loc) => "related bound defined here",
            ),
            SignatureError::Arg(pos, expected, got) => diag!(
                (span, self.current_file) => "argument {} mismatch, expected {} but got {}" {
                    pos + 1,
                    &self.interner[self.typec.types.id(expected)],
                    &self.interner[self.typec.types.id(got)],
                },
                (exp loc) => "related bound defined here",
            ),
            SignatureError::Ret(expected, got) => diag!(
                (span, self.current_file) => "return type mismatch, expected {} but got {}" {
                    expected.expand().map_or("nothing", |expected| &self.interner[self.typec.types.id(expected)]),
                    got.expand().map_or("nothing", |got| &self.interner[self.typec.types.id(got)]),
                },
                (exp loc) => "related bound defined here",
            ),
        };
        self.workspace.push(diag);
    }

    pub fn funcs(&mut self, funcs: impl IntoIterator<Item = (Maybe<Ident>, AstEnt, Def)>) {
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
        body: AstEnt,
        generics: AstEnt,
        args: &[AstEnt],
        ret: Expected,
    ) -> errors::Result<Maybe<TirList>> {
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

    fn block(&mut self, body: AstEnt, expected: Expected) -> TirEnt {
        let stmts = &self.ast_data[body.children];

        let mut last = TirEnt::default();

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
            .fill_reserved(reserved, TirEnt::new(TirKind::Unreachable));

        TirEnt {
            kind: TirKind::Block { stmts },
            ..last
        }
    }

    fn expr(&mut self, ast: AstEnt, expected: Expected) -> errors::Result<TirEnt> {
        let expr = match ast.kind {
            AstKind::Binary => self.binary(ast)?,
            AstKind::Return => self.r#return(ast)?,

            AstKind::String => TirEnt::new(TirKind::String)
                .span(ast.span)
                .ty(BuiltinTypes::STR),
            AstKind::Int => self.int(ast, expected),
            AstKind::Ident | AstKind::IdentChain => self.ident(ast)?,
            AstKind::Call => self.call(ast, expected)?,

            kind => unimplemented!("{kind:?}"),
        };

        self.expect_type(expr, expected);

        Ok(expr)
    }

    fn call(&mut self, ast: AstEnt, expected: Expected) -> errors::Result<TirEnt> {
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

    fn proc_call(
        &mut self,
        caller: AstEnt,
        args: &[AstEnt],
        expected: Expected,
    ) -> errors::Result<TirEnt> {
        let id = ty_parser!(self, self.current_file).ident_chain_id(caller);
        let def = self.get_from_scope(id, caller.span, "function", Reports::base)?;

        let (kind, ret) = match_scope_ptr!((self, def, caller.span) => {
            def: Def => {
                let DefEnt { sig, params, upper_params, .. } = self.typec.defs[def];
                let (args, ret) = if self.typec.defs[def].flags.contains(FuncFlags::GENERIC) {
                    self.parse_generic_args(args, sig, params, upper_params, expected)?
                } else {
                    (self.parse_args(args, sig.args)?, sig.ret)
                };

                (TirKind::Call {
                    def,
                    params: default(),
                    args,
                }, ret)
            },
            bound_func: BoundFunc => {
                let BoundFuncEnt { sig, params, parent, .. } = self.typec.bound_funcs[bound_func];
                let (args, ret) = if params.is_some() || self.typec.param_count(parent) > 0 {
                    self.parse_generic_bound_args(args, sig, params, parent, expected)?
                } else {
                    (self.parse_args(args, sig.args)?, sig.ret)
                };
                (TirKind::BoundCall {
                    bound_func,
                    params: default(),
                    args,
                }, ret)
            },
        });

        Ok(TirEnt::new(kind).span(caller.span).ty(ret))
    }

    fn parse_generic_bound_args(
        &mut self,
        args: &[AstEnt],
        sig: Sig,
        params: Maybe<TyList>,
        parent: Ty,
        expected: Expected,
    ) -> errors::Result<(Maybe<TirList>, Maybe<Ty>)> {
        let parent_param_count = self.typec.param_count(parent);
        self.typec.re_index_params(params, parent_param_count);

        let total_param_count = parent_param_count + self.typec.ty_lists[params].len();
        let mut infer_slots = vec![BuiltinTypes::INFERRED; total_param_count];

        let types = self.typec.ty_lists[params].to_vec();

        let arg_parser = |(&arg, ty)| {
            let instance = ty_factory!(self).try_instantiate(ty, &infer_slots);
            let expr = self
                .expr(arg, instance)
                .map_err(|()| TyInferenceError::InvalidExpr)?;
            if let Some(expr_ty) = expr.ty.expand() && self.typec.types[ty].flags.contains(TyFlags::GENERIC) {
                bound_checker!(self).infer_type(expr_ty, ty, &mut infer_slots).map(|()| expr)
            } else if instance != expr.ty.expand() {
                Err(TyInferenceError::TypeMismatch(instance.into(), expr.ty))
            } else {
                Ok(expr)
            }
        };

        let args = args
            .iter()
            .zip(types)
            .map(arg_parser)
            .collect::<Result<Vec<_>, _>>();

        todo!()
    }

    fn parse_generic_args(
        &mut self,
        args: &[AstEnt],
        sig: Sig,
        params: Maybe<TyList>,
        upper_params: Maybe<TyList>,
        expected: Expected,
    ) -> errors::Result<(Maybe<TirList>, Maybe<Ty>)> {
        todo!()
    }

    fn parse_args(
        &mut self,
        args: &[AstEnt],
        types: Maybe<TyList>,
    ) -> errors::Result<Maybe<TirList>> {
        let arg_types = self.typec.ty_lists[types].to_vec();
        let args = args
            .iter()
            .zip(arg_types)
            .map(|(&arg, ty)| self.expr(arg, ty.into()))
            .collect::<Vec<_>>()
            .into_iter()
            .collect::<Result<Vec<_>, _>>()?;
        Ok(self.tir_data.bump(args))
    }

    fn binary(&mut self, ast: AstEnt) -> errors::Result<TirEnt> {
        let [lhs, op, rhs] = self.ast_data[ast.children] else {
            unreachable!();
        };

        let op_id = self.op_id(op, true);
        let def: Def =
            self.get_from_scope_concrete(op_id, op.span, "binary operator", Reports::base)?;

        let sig = self.typec.defs[def].sig;
        let [left_ty, right_ty] = self.typec.ty_lists[sig.args] else {
            unreachable!();
        };

        let lhs = self.expr(lhs, Some(left_ty));
        let rhs = self.expr(rhs, Some(right_ty));
        let (lhs, rhs) = (lhs?, rhs?);
        let args = self.tir_data.bump([lhs, rhs]);

        Ok(TirEnt::new(TirKind::Call {
            def,
            params: Maybe::none(),
            args,
        })
        .ty(sig.ret)
        .span(ast.span))
    }

    fn int(&mut self, ast: AstEnt, expected: Expected) -> TirEnt {
        let ty = {
            let mapping = [
                (BuiltinTypes::I8, "i8"),
                (BuiltinTypes::I16, "i16"),
                (BuiltinTypes::I32, "i32"),
                (BuiltinTypes::I64, "i64"),
                (BuiltinTypes::U8, "u8"),
                (BuiltinTypes::U16, "u16"),
                (BuiltinTypes::U32, "u32"),
                (BuiltinTypes::U64, "u64"),
                (BuiltinTypes::UINT, "u"),
            ];

            mapping
                .into_iter()
                .find_map(|(ty, str)| span_str!(self, ast.span).ends_with(str).then_some(ty))
                .or(expected)
                .unwrap_or(BuiltinTypes::INT)
        };

        TirEnt::new(TirKind::Int).ty(ty).span(ast.span)
    }

    fn ident(&mut self, ast: AstEnt) -> errors::Result<TirEnt> {
        let id = ty_parser!(self, self.current_file).ident_chain_id(ast);

        let something = self.get_from_scope(id, ast.span, "variable", Reports::base)?;

        let result = if let Some(var) = something.try_read::<Tir>() {
            TirEnt::new(TirKind::Access { var })
                .span(ast.span)
                .ty(self.tir_data[var].ty)
        } else {
            unimplemented!();
        };

        Ok(result)
    }

    fn r#return(&mut self, ast: AstEnt) -> errors::Result<TirEnt> {
        let [r#return] = self.ast_data[ast.children] else {
            unreachable!();
        };

        let expr = if r#return.kind.is_none() {
            TirEnt::new(TirKind::Unreachable)
        } else {
            let expected = self.typec.defs[self.func_parser_ctx.current_fn.unwrap()]
                .sig
                .ret;
            self.expr(r#return, expected.expand())?
        };

        let (value, _) = self.tir_data.bump_push(expr);

        Ok(TirEnt::new(TirKind::Return {
            value: value.into(),
        })
        .span(ast.span))
    }

    fn op_id(&mut self, op: AstEnt, binary: bool) -> Ident {
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

    fn expect_type(&mut self, tir: TirEnt, expected: Expected) {
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

    fn push_args(&mut self, args: &[AstEnt]) {
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
            let argument = TirEnt::new(TirKind::Argument(i as u8))
                .ty(ty)
                .span(name.span);
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

    scope_error_handler!();
}
