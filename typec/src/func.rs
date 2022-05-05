use std::fmt::Write;

use cranelift_entity::{packed_option::ReservedValue, PrimaryMap};

use crate::{Error, *};
use lexer::*;
use parser::*;

pub type Funcs = PrimaryMap<Func, Ent>;

pub struct Builder<'a> {
    pub func: Func,
    pub funcs: &'a mut Funcs,
    pub ctx: &'a mut Context,
    pub body: &'a mut tir::Data,
    pub scope: &'a mut Scope,
    pub types: &'a mut Types,
    pub temp: &'a mut FramedStack<Tir>,
    pub sources: &'a Sources,
    pub ast: &'a ast::Data,
    pub modules: &'a mut Modules,
    pub diagnostics: &'a mut errors::Diagnostics,
}

impl<'a> Builder<'a> {
    pub fn verify_bound_impls(&mut self) -> errors::Result {
        // we have to collect all functions first and the check if all bound
        // functions are implemented, its done here
        {
            while let Some((implementor, bound, impl_block)) = self.ctx.bounds_to_verify.pop() {
                let &[.., implementor_ast, body] = self.ast.children(impl_block) else {
                    unreachable!();
                };
                let implementor_span = self.ast.nodes[implementor_ast].span;

                // handle use expressions
                if !body.is_reserved_value() {
                    for &ast in self.ast.children(body) {
                        if self.ast.nodes[ast].kind != ast::Kind::UseBoundFunc {
                            continue;
                        }

                        let &[func, ident] = self.ast.children(ast) else {
                            unreachable!();
                        };

                        let Ok(id) = self.ident_id(func, Some((implementor, implementor_span))) else {
                            continue;
                        };

                        let span = self.ast.nodes[func].span;
                        let Ok(func) = self.scope.get::<Func>(self.diagnostics, id, span) else {
                            continue;
                        };

                        let id = {
                            let func = self.id_of(ident);
                            let bound = self.types.ents[bound].id;
                            let implementor = self.types.ents[implementor].id;
                            Collector::bound_impl_owned_func_id(bound, implementor, func)
                        };

                        {
                            let item = modules::Item::new(id, func, span);
                            drop(self.scope.insert(
                                self.diagnostics,
                                span.source(),
                                id,
                                item.to_scope_item(),
                            ));
                            self.modules[span.source()].items.push(item);
                        }
                    }
                }

                let impl_span = self.ast.nodes[impl_block].span;

                // check if all functions exist and match the signature
                let funcs = {
                    let ty::Kind::Bound(funcs) = self.types.ents[bound].kind else {
                        unreachable!();
                    };

                    // TODO: don't allocate if it impacts performance
                    let mut vec = self.types.funcs.get(funcs).to_vec();
                    for func in &mut vec {
                        let ent = &self.funcs[*func];

                        let (sugar_id, certain_id) = {
                            let func = self.sources.id(ent.name);
                            let ty = self.types.ents[implementor].id;
                            let sugar_id = Self::owned_func_id(ty, func);
                            let bound = self.types.ents[bound].id;
                            let certain_id = Collector::bound_impl_owned_func_id(bound, ty, func);
                            (sugar_id, certain_id)
                        };

                        let maybe_other = None // looks better
                            .or_else(|| self.scope.weak_get::<Func>(certain_id))
                            .or_else(|| self.scope.weak_get::<Func>(sugar_id));

                        let Some(other) = maybe_other else {
                            self.diagnostics.push(Error::MissingBoundImplFunc {
                                func: ent.name,
                                loc: impl_span,
                            });
                            continue;
                        };

                        drop(self.compare_signatures(*func, other));

                        *func = other;
                    }

                    self.types.funcs.push(&vec)
                };

                let id = {
                    let implementor = self.types.ents[implementor].id;
                    let bound = self.types.ents[bound].id;
                    Collector::bound_impl_id(bound, implementor)
                };

                assert!(self
                    .types
                    .bound_cons
                    .insert(
                        id,
                        ty::BoundImpl {
                            span: self.ast.nodes[impl_block].span,
                            funcs,
                        }
                    )
                    .is_some());
            }
        }

        Ok(())
    }

    pub fn compare_signatures(&mut self, bound_func: Func, impl_func: Func) -> errors::Result {
        let a = self.funcs[impl_func].sig;
        let b = self.funcs[bound_func].sig;

        let a_param_len = self.types.args.len(a.params);
        let b_param_len = self.types.args.len(b.params);

        if a_param_len != b_param_len - 1 {
            self.diagnostics.push(Error::BoundImplFuncParamCount {
                impl_func: self.funcs[impl_func].name,
                bound_func: self.funcs[bound_func].name,
                expected: b_param_len,
                found: a_param_len,
            });
            return Err(());
        }

        // TODO: don't allocate if this becomes issue, bounds might get implemented a lot
        let mut params = vec![Ty::reserved_value(); b_param_len];

        let iter = {
            // TODO: same here
            let a_args = self.types.args.get(a.args).to_vec();
            let b_args = self.types.args.get(b.args).to_vec();
            let ast_params = {
                let children = self.ast.children(self.ctx.func_ast[impl_func]);
                let has_ret = (a.ret != self.types.builtin.nothing) as usize;
                &children
                    [Parser::FUNCTION_ARG_START..children.len() - Parser::FUNCTION_RET + has_ret]
            };

            let a = a_args.into_iter().chain(std::iter::once(a.ret));
            let b = b_args.into_iter().chain(std::iter::once(b.ret));
            a.zip(b).zip(ast_params)
        };

        for ((referenced, parametrized), &ast) in iter {
            let span = self.ast.nodes[ast].span;
            drop(infer_parameters(
                referenced,
                parametrized,
                &mut params,
                a.params,
                b.params,
                span,
                &mut self.types,
                &mut self.diagnostics,
            ));
        }

        Ok(())
    }

    pub fn build(&mut self) -> errors::Result {
        let Ent {
            flags,
            kind,
            sig: Sig {
                ret, params, args, ..
            },
            ..
        } = self.funcs[self.func];

        let ast = self.ctx.func_ast[self.func];
        let header = self.ast.children(ast);
        let &[generics, .., ret_ast, _] = header else {
            unreachable!();
        };
        let &body_ast = header.last().unwrap();

        if body_ast.is_reserved_value() {
            if flags.contains(func::Flags::EXTERNAL) {
                self.funcs[self.func].kind = func::Kind::External;
            }
            return Ok(());
        }

        self.scope.mark_frame();
        if let func::Kind::Owned(ty) = kind {
            let span = self.ast.nodes[ast].span;
            self.scope.push_item("Self", scope::Item::new(ty, span));
        }

        if !generics.is_reserved_value() {
            let ast = self.ast.children(generics);
            let bounds = self.types.args.get(params);
            let params = self.types.fn_params();
            for ((&item, &param), &bound_combo) in ast.iter().zip(params).zip(bounds) {
                let name = self.ast.children(item)[0];
                let span = self.ast.nodes[name].span;
                let str = self.sources.display(span);
                self.scope.push_item(str, scope::Item::new(param, span));

                let ty::Kind::BoundCombo(bounds) = self.types.ents[bound_combo].kind else {
                    unreachable!();
                };

                for &bound in self.types.args.get(bounds) {
                    let ty::Kind::Bound(funcs) = self.types.ents[bound].kind else {
                        unreachable!();
                    };

                    // TODO: can caching ids make less cache misses?
                    for &func in self.types.funcs.get(funcs) {
                        let name = self.funcs[func].name;
                        let id = {
                            let id = self.sources.id(name);
                            let bound = self.types.ents[bound_combo].id;
                            Self::owned_func_id(bound, id)
                        };
                        self.scope.push_item(id, scope::Item::new(func, name));
                    }
                }
            }
        }

        let args = {
            let ast_args =
                &header[Parser::FUNCTION_ARG_START..header.len() - Parser::FUNCTION_ARG_END];
            let args = self.types.args.get(args);
            let mut tir_args = Vec::with_capacity(args.len());
            for (i, (&ast, &ty)) in ast_args.iter().zip(args).enumerate() {
                self.ctx.use_type(ty, self.types);

                let &[name, ..] = self.ast.children(ast) else {
                    unreachable!();
                };
                let span = self.ast.nodes[name].span;

                let arg = {
                    let kind = tir::Kind::Argument(i as u32);
                    let ent = tir::Ent::new(kind, ty, span);
                    self.body.ents.push(ent)
                };

                {
                    let id = {
                        let str = self.sources.display(span);
                        ID::new(str)
                    };
                    let item = scope::Item::new(arg, span);
                    self.scope.push_item(id, item);
                }

                tir_args.push(arg);
            }
            self.body.cons.push(&tir_args)
        };

        let root = self.build_block(body_ast)?;
        self.funcs[self.func].body = root;
        self.funcs[self.func].args = args;

        self.body.used_types = self.types.args.push(&self.ctx.used_types);
        self.ctx.used_types.clear();
        self.ctx.used_types_set.clear();

        if !self.body.ents[root].flags.contains(tir::Flags::TERMINATING)
            && ret != self.types.builtin.nothing
        {
            let because = Some(self.ast.nodes[ret_ast].span);
            self.expect_tir_ty(root, ret, |_, got, loc| Error::ReturnTypeMismatch {
                because,
                expected: ret,
                got,
                loc,
            })?;
        }

        self.scope.pop_frame();

        Ok(())
    }

    fn build_block(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;

        self.temp.mark_frame();
        self.scope.mark_frame();

        let mut final_expr = None;
        let mut terminating = false;
        for &stmt in self.ast.children(ast) {
            let Ok(expr) = self.build_expr(stmt) else {
                continue; // Recover here
            };
            self.temp.push(expr);
            final_expr = Some(expr);
            terminating |= self.body.ents[expr].flags.contains(tir::Flags::TERMINATING);
            if terminating {
                break; // TODO: emit warning
            }
        }

        let result = {
            let ty = if let Some(expr) = final_expr {
                self.body.ents[expr].ty
            } else {
                self.types.builtin.nothing
            };

            let slice = self.temp.top_frame();
            let items = self.body.cons.push(slice);

            let kind = tir::Kind::Block(items);
            let flags = tir::Flags::TERMINATING & terminating;
            let ent = tir::Ent::with_flags(kind, ty, flags, span);
            self.body.ents.push(ent)
        };

        self.temp.pop_frame();
        self.scope.pop_frame();

        Ok(result)
    }

    fn build_expr(&mut self, ast: Ast) -> errors::Result<Tir> {
        let ast::Ent { kind, span, .. } = self.ast.nodes[ast];
        let expr = match kind {
            ast::Kind::Unary => self.build_unary(ast)?,
            ast::Kind::Binary => self.build_binary(ast)?,
            ast::Kind::Ident => self.build_ident(ast)?,
            ast::Kind::If => self.build_if(ast)?,
            ast::Kind::Return => self.build_return(ast)?,
            ast::Kind::Int(width) => self.build_int(ast, width)?,
            ast::Kind::Bool(value) => self.build_bool(ast, value)?,
            ast::Kind::Char => self.build_char(ast)?,
            ast::Kind::Variable(mutable) => self.build_variable(mutable, ast)?,
            ast::Kind::Constructor => self.build_constructor(ast)?,
            ast::Kind::DotExpr => self.build_dot_expr(ast)?,
            ast::Kind::Call => self.build_call(ast)?,
            ast::Kind::Block => self.build_block(ast)?,
            ast::Kind::Loop => self.build_loop(ast)?,
            ast::Kind::Break => self.build_break(ast)?,
            ast::Kind::Deref => self.build_deref(ast)?,
            _ => todo!(
                "Unhandled expression ast {:?}: {}",
                kind,
                self.sources.display(span)
            ),
        };
        Ok(expr)
    }

    fn build_deref(&mut self, ast: Ast) -> errors::Result<Tir> {
        let expr = self.ast.children(ast)[0];
        let target = self.build_expr(expr)?;
        let ty = self.body.ents[target].ty;

        if !matches!(self.types.ents[ty].kind, ty::Kind::Ptr(..)) {
            self.diagnostics.push(Error::NonPointerDereference {
                loc: self.ast.nodes[ast].span,
                ty,
            });
            return Err(());
        }

        Ok(self.deref_ptr(target))
    }

    fn build_char(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let ent = tir::Ent::new(tir::Kind::CharLit, self.types.builtin.char, span);
        Ok(self.body.ents.push(ent))
    }

    fn build_unary(&mut self, ast: Ast) -> errors::Result<Tir> {
        let &[op, expr] = self.ast.children(ast) else {
            unreachable!()
        };

        let expr = self.build_expr(expr)?;
        let operand_ty = self.body.ents[expr].ty;

        let func = {
            let span = self.ast.nodes[op].span;
            let id = {
                let ty = self.base_id_of(operand_ty);

                let op = self.sources.id(span);

                Self::unary_id(ty, op)
            };

            self.scope.get::<Func>(self.diagnostics, id, span)?
        };

        let result = {
            let ty = self.funcs[func].sig.ret;
            let span = self.ast.nodes[ast].span;
            let args = self.body.cons.push(&[expr]);
            let kind = tir::Kind::Call(operand_ty.into(), func, args);
            let ent = tir::Ent::new(kind, ty, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    pub fn unary_id(ty: ID, op: ID) -> ID {
        ID::new("<unary>") + ty + op
    }

    fn build_break(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let &[value] = self.ast.children(ast) else {
            unreachable!();
        };

        let value = if value.is_reserved_value() {
            None
        } else {
            Some(self.build_expr(value)?)
        };

        let loop_expr = self.scope.get::<Tir>(self.diagnostics, "<loop>", span)?;

        {
            let tir::Kind::LoopInProgress(ret, infinite) = &mut self.body.ents[loop_expr].kind else {
                unreachable!();
            };

            *infinite = false;

            if let Some(ret) = ret.expand() {
                let tir::Ent {
                    span: ret_span, ty, ..
                } = self.body.ents[ret];
                if let Some(value) = value {
                    self.expect_tir_ty(value, ty, |_, got, loc| Error::BreakValueTypeMismatch {
                        because: ret_span,
                        expected: ty,
                        got,
                        loc,
                    })?;
                } else {
                    self.diagnostics.push(Error::MissingBreakValue {
                        because: ret_span,
                        expected: ty,
                        loc: span,
                    });
                    return Err(());
                }
            } else {
                *ret = value.into();
            }
        }

        let result = {
            let kind = tir::Kind::Break(loop_expr, value.into());
            let ent = tir::Ent::with_flags(
                kind,
                self.types.builtin.nothing,
                tir::Flags::TERMINATING,
                span,
            );
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn build_loop(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let &[body_ast] = self.ast.children(ast) else {
            unreachable!();
        };

        let loop_slot = {
            let kind = tir::Kind::LoopInProgress(None.into(), true);
            let ent = tir::Ent::new(kind, self.types.builtin.nothing, span);
            self.body.ents.push(ent)
        };

        // break and continue will propagate side effects so we save the lookup

        self.scope
            .push_item("<loop>", scope::Item::new(loop_slot, span));

        let block = self.build_block(body_ast)?;

        self.scope.pop_item();

        {
            let tir::Kind::LoopInProgress(ret, infinite) = self.body.ents[loop_slot].kind else {
                unreachable!();
            };
            let ty = ret
                .map(|ret| self.body.ents[ret].ty)
                .unwrap_or(self.types.builtin.nothing);
            let flags = tir::Flags::TERMINATING & infinite;
            let kind = tir::Kind::Loop(block);
            let span = span;
            self.body.ents[loop_slot] = tir::Ent::with_flags(kind, ty, flags, span);
        }

        Ok(loop_slot)
    }

    fn build_call(&mut self, ast: Ast) -> errors::Result<Tir> {
        let &[caller, ..] = self.ast.children(ast) else {
            unreachable!();
        };

        let (id, fn_span, mut obj, caller, instantiation) =
            if self.ast.nodes[caller].kind == ast::Kind::DotExpr {
                let &[expr, name] = self.ast.children(caller) else {
                    unreachable!();
                };

                let expr = self.build_expr(expr)?;
                let ty = {
                    let ty = self.body.ents[expr].ty;
                    self.types.ptr_base_of(ty)
                };

                let (ident, instantiation) = {
                    if ast::Kind::Instantiation == self.ast.nodes[name].kind {
                        (self.ast.children(name)[0], Some(name))
                    } else {
                        (name, None)
                    }
                };

                let span = self.body.ents[expr].span;
                let name_id = self.ident_id(ident, Some((ty, span)))?;

                let span = self.ast.nodes[name].span;
                (name_id, span, Some(expr), Some(ty), instantiation)
            } else {
                let (ident, instantiation) = {
                    if ast::Kind::Instantiation == self.ast.nodes[caller].kind {
                        (self.ast.children(caller)[0], Some(caller))
                    } else {
                        (caller, None)
                    }
                };

                let (name_id, owner) = self.ident_id_low(ident, None)?;

                let span = self.ast.nodes[caller].span;
                (name_id, span, None, owner.map(|(ty, _)| ty), instantiation)
            };

        // TODO: Handle function pointer as field
        let func = self.scope.get::<Func>(self.diagnostics, id, fn_span)?;

        let Ent { sig, flags, .. } = self.funcs[func];

        let arg_tys = self.types.args.get(sig.args).to_vec(); // TODO: avoid allocation

        // handle auto ref or deref
        if let (Some(obj), Some(&expected)) = (obj.as_mut(), arg_tys.first()) {
            if let Ok(corrected) = self.ptr_correct(*obj, expected) {
                *obj = corrected;
            }
        }

        // we first collect results, its in a way of recovery
        // TODO: This can be optimized when needed
        let args = {
            let mut vec = Vec::with_capacity(arg_tys.len() + obj.is_some() as usize);
            if let Some(obj) = obj {
                vec.push(obj);
            }
            self.ast.children(ast)[1..]
                .iter()
                .fold(Ok(()), |acc, &arg| {
                    self.build_expr(arg).map(|arg| vec.push(arg)).and(acc)
                })?;
            vec
        };

        let (ret, func) = {
            let because = self.funcs[func].name;
            let args_len = args.len();
            if arg_tys.len() != args_len {
                let loc = self.ast.nodes[ast].span;
                self.diagnostics.push(Error::FunctionParamMismatch {
                    because,
                    expected: arg_tys.len(),
                    got: args_len,
                    loc,
                });
                return Err(());
            }

            if !flags.contains(Flags::GENERIC) {
                // here we type check all arguments and then check for errors
                args.iter()
                    .zip(arg_tys)
                    .map(|(&arg, ty)| {
                        self.expect_tir_ty(arg, ty, |_, got, loc| Error::CallArgTypeMismatch {
                            because,
                            expected: ty,
                            got,
                            loc,
                        })
                    })
                    .fold(Ok(()), |acc, err| acc.and(err))?;

                (sig.ret, func)
            } else {
                let params = self.types.args.get(sig.params);
                let mut param_slots = vec![Ty::default(); params.len()];

                if let Some(instantiation) = instantiation {
                    if params.len() > self.ast.children(instantiation).len() - 1 {
                        todo!("this is an error");
                    }

                    for (i, &param) in self.ast.children(instantiation)[1..].iter().enumerate() {
                        let Ok(ty) = parse_type!(self, param) else {
                            continue;
                        };
                        param_slots[i] = ty;
                    }
                }

                let foreign_params = sig.params;
                let home_params = self.funcs[self.func].sig.params;

                args.iter()
                    .zip(arg_tys)
                    .map(|(&arg, arg_ty)| {
                        let tir::Ent { ty, span, .. } = self.body.ents[arg];
                        infer_parameters(
                            ty,
                            arg_ty,
                            &mut param_slots,
                            home_params,
                            foreign_params,
                            span,
                            self.types,
                            self.diagnostics,
                        )
                    })
                    // fold prevents allocation
                    .fold(Ok(()), |acc, err| acc.and(err))?;

                if let (None, Some(ty), Some(param)) = (obj, caller, param_slots.last_mut()) && param.is_reserved_value() {
                    *param = ty;
                }

                param_slots
                    .iter()
                    .enumerate()
                    .filter_map(|(i, ty)| ty.is_reserved_value().then_some(i))
                    // OK if no cycles performed
                    .fold(Ok(()), |_, param| {
                        let span = self.ast.nodes[ast].span;
                        self.diagnostics.push(Error::UnknownGenericParam {
                            func: self.funcs[func].name,
                            param,
                            loc: span,
                        });
                        Err(())
                    })?;

                let generic = param_slots
                    .iter()
                    .any(|&params| self.types.ents[params].flags.contains(ty::Flags::GENERIC));

                let ret = instantiate(sig.ret, &param_slots, self.types)?;

                self.ctx.use_type(ret, self.types);

                if generic {
                    (ret, func)
                } else {
                    let func = {
                        let i_params = self.types.args.push(&param_slots);

                        // println!("====== {} {:?}", ty::Display::new(self.types, self.sources, ret), self.types.ents[ret]);

                        let sig = Sig {
                            params: i_params,
                            ret,
                            ..sig
                        };
                        let ent = Ent {
                            sig,
                            kind: Kind::Instance(func),
                            flags: self.funcs[func].flags & !func::Flags::GENERIC,
                            ..self.funcs[func]
                        };
                        self.funcs.push(ent)
                    };

                    (ret, func)
                }
            }
        };

        let result = {
            let span = self.ast.nodes[ast].span;
            let args = self.body.cons.push(&args);
            let kind = tir::Kind::Call(caller.into(), func, args);
            let ent = tir::Ent::new(kind, ret, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn ptr_correct(&mut self, mut target: Tir, expected: Ty) -> errors::Result<Tir> {
        let ty = self.body.ents[target].ty;

        let target_depth = self.types.ptr_depth_of(ty);
        let expected_depth = self.types.ptr_depth_of(expected);

        for _ in expected_depth..target_depth {
            target = self.deref_ptr(target);
        }

        for _ in target_depth..expected_depth {
            target = self.take_ptr(target);
        }

        let ty = self.body.ents[target].ty;
        if ty != expected {
            return Err(());
        }

        Ok(target)
    }

    fn take_ptr(&mut self, target: Tir) -> Tir {
        let tir::Ent {
            ty,
            span,
            flags,
            kind,
            ..
        } = &mut self.body.ents[target];
        let (span, ty) = (*span, *ty);
        if let &mut tir::Kind::Access(target) = kind {
            self.body.ents[target].flags.insert(tir::Flags::SPILLED);
        } else {
            flags.insert(tir::Flags::SPILLED);
        }

        let kind = tir::Kind::TakePtr(target);
        let ptr_ty = pointer_of(ty, self.types);
        let deref = tir::Ent::new(kind, ptr_ty, span);
        self.body.ents.push(deref)
    }

    fn deref_ptr(&mut self, target: Tir) -> Tir {
        let span = self.body.ents[target].span;
        let kind = tir::Kind::DerefPointer(target);
        let deref_ty = self.types.deref_ptr(self.body.ents[target].ty);
        self.ctx.use_type(deref_ty, self.types);
        let deref = tir::Ent::new(kind, deref_ty, span);
        self.body.ents.push(deref)
    }

    fn ident_id(&mut self, ast: Ast, owner: Option<(Ty, Span)>) -> errors::Result<ID> {
        self.ident_id_low(ast, owner).map(|(id, _)| id)
    }

    fn ident_id_low(
        &mut self,
        ast: Ast,
        owner: Option<(Ty, Span)>,
    ) -> errors::Result<(ID, Option<(Ty, Span)>)> {
        let children = self.ast.children(ast);
        match (children, owner) {
            (&[module, _, item], None) | (&[module, item], Some(_)) => {
                let module_id = {
                    let span = self.ast.nodes[module].span;
                    let id = self.sources.id(span);

                    let source = self.scope.get::<Source>(self.diagnostics, id, span)?;
                    ID::from(source)
                };

                let (ty, span) = if let Some(owner) = owner {
                    owner
                } else {
                    let span = self.ast.nodes[item].span;
                    let id = self.sources.id(span);
                    (self.scope.get::<Ty>(self.diagnostics, id, span)?, span)
                };

                let id = {
                    let name = self.id_of(item);
                    let ty = self.base_id_of(ty);
                    Self::owned_func_id(ty, name)
                };

                Ok((id + module_id, Some((ty, span))))
            }
            (&[module_or_type, item], None) => {
                let item_id = self.id_of(item);

                let span = self.ast.nodes[module_or_type].span;
                let id = self.sources.id(span);

                Ok(
                    if let Some(source) =
                        self.scope.may_get::<Source>(self.diagnostics, id, span)?
                    {
                        (item_id + ID::from(source), None)
                    } else {
                        let ty = self.scope.get::<Ty>(self.diagnostics, id, span)?;
                        (
                            Self::owned_func_id(self.base_id_of(ty), item_id),
                            Some((ty, span)),
                        )
                    },
                )
            }
            (&[], None) => return Ok((self.id_of(ast), None)),
            (&[], Some((ty, span))) => {
                let name = self.id_of(ast);
                let ty_id = self.base_id_of(ty);
                Ok((Self::owned_func_id(ty_id, name), Some((ty, span))))
            }
            _ => {
                self.diagnostics.push(Error::InvalidPath {
                    loc: self.ast.nodes[ast].span,
                });
                Err(())
            }
        }
    }

    fn build_dot_expr(&mut self, ast: Ast) -> errors::Result<Tir> {
        let &[header, field] = self.ast.children(ast) else {
            unreachable!();
        };

        let mut header = self.build_expr(header)?;

        let ty = self.body.ents[header].ty;
        let base_ty = self.types.ptr_base_of(ty);

        let span = self.ast.nodes[field].span;
        let (field_id, field_ty) = {
            let id = {
                let ty_id = self.types.ents[base_ty].id;
                let str = self.sources.display(span);
                ty::Builder::field_id(ty_id, ID::new(str))
            };
            self.find_field(base_ty, id, span)?
        };

        while base_ty != self.body.ents[header].ty {
            header = self.deref_ptr(header);
        }

        let result = {
            self.ctx.use_type(field_ty, self.types);
            let kind = tir::Kind::FieldAccess(header, field_id);
            let ent = tir::Ent::new(kind, field_ty, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn build_bool(&mut self, ast: Ast, value: bool) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let ty = self.types.builtin.bool;
        let kind = tir::Kind::BoolLit(value);
        let ent = tir::Ent::new(kind, ty, span);
        let result = self.body.ents.push(ent);
        Ok(result)
    }

    fn build_constructor(&mut self, ast: Ast) -> errors::Result<Tir> {
        let &[name, body] = self.ast.children(ast) else {
            unreachable!("{:?}", self.ast.children(ast));
        };

        let span = self.ast.nodes[name].span;
        let ty = {
            let id = self.ident_id(name, None)?;
            self.scope.get::<Ty>(self.diagnostics, id, span)?
        };

        self.build_constructor_low(ty, body)
    }

    fn build_constructor_low(&mut self, ty: Ty, body: Ast) -> errors::Result<Tir> {
        self.ctx.use_type(ty, self.types);

        let span = self.ast.nodes[body].span;
        let ty_id = self.types.ents[ty].id;

        let ty::Kind::Struct(fields) = self.types.ents[ty].kind else {
            self.diagnostics.push(Error::ExpectedStruct {
                got: ty,
                loc: span,
            });
            return Err(());
        };

        let mut initial_values = vec![Tir::reserved_value(); self.types.sfields.get(fields).len()]; // TODO: don't allocate
        for &field in self.ast.children(body) {
            let &[name, expr] = self.ast.children(field) else {
                unreachable!();
            };

            let span = self.ast.nodes[name].span;
            let id = {
                let id = self.sources.id(span);
                ty::Builder::field_id(ty_id, id)
            };

            let Ok((field, field_ty)) = self.find_field(ty, id, span) else {
                continue;
            };

            let ty::SFieldEnt {
                span: hint, index, ..
            } = self.types.sfields[field];

            let Ok(value) = (match self.ast.nodes[expr].kind {
                ast::Kind::InlineConstructor => self.build_constructor_low(field_ty, expr),
                _ => self.build_expr(expr),
            }) else {
                continue;
            };

            // we ignore this to report more errors
            drop(self.expect_tir_ty(value, field_ty, |_, got, loc| {
                Error::ConstructorFieldTypeMismatch {
                    because: hint,
                    expected: field_ty,
                    got,
                    loc,
                }
            }));

            initial_values[index as usize] = value;
        }

        if initial_values.iter().any(Tir::is_reserved_value) {
            let missing = initial_values
                .iter()
                .enumerate()
                .filter_map(|(index, &value)| (value == Tir::reserved_value()).then_some(index))
                .map(|i| self.types.sfields.get(fields)[i].span)
                .collect::<Vec<_>>();

            self.diagnostics.push(Error::ConstructorMissingFields {
                on: ty,
                missing,
                loc: span,
            });

            return Err(());
        }

        let result = {
            let span = self.ast.nodes[body].span;
            let fields = self.body.cons.push(&initial_values);
            let kind = tir::Kind::Constructor(fields);
            let ent = tir::Ent::new(kind, ty, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn build_variable(&mut self, mutable: bool, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let &[name, value] = self.ast.children(ast) else {
            unreachable!();
        };

        let id = {
            let span = self.ast.nodes[name].span;
            let str = self.sources.display(span);
            ID::new(str)
        };

        let value = self.build_expr(value)?;
        self.body.ents[value]
            .flags
            .insert(tir::Flags::ASSIGNABLE & mutable);

        {
            let item = scope::Item::new(value, span);
            self.scope.push_item(id, item);
        }

        let result = {
            let kind = tir::Kind::Variable(value);
            let ent = tir::Ent::new(kind, self.types.builtin.nothing, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn build_int(&mut self, ast: Ast, width: i16) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let kind = tir::Kind::IntLit(width);
        let ty = {
            let name = match width {
                8 => "i8",
                16 => "i16",
                32 => "i32",
                64 => "i64",
                _ => "int",
            };

            self.scope.get::<Ty>(self.diagnostics, name, span).unwrap()
        };

        let ent = tir::Ent::new(kind, ty, span);
        Ok(self.body.ents.push(ent))
    }

    fn build_return(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let &[value] = self.ast.children(ast) else {
            unreachable!()
        };

        let ret = self.funcs[self.func].sig.ret;

        let value = if value.is_reserved_value() {
            self.expect_ty(self.types.builtin.nothing, ret, |s| {
                Error::UnexpectedReturnValue {
                    because: s.funcs[s.func].name,
                    loc: span,
                }
            })?;
            None
        } else {
            let value = self.build_expr(value)?;
            self.expect_tir_ty(value, ret, |s, got, loc| {
                let &[.., ret_ast, _] = s.ast.children(s.ctx.func_ast[s.func]) else {
                    unreachable!();
                };
                let ret_span = (!ret_ast.is_reserved_value()).then(|| s.ast.nodes[ret_ast].span);
                Error::ReturnTypeMismatch {
                    because: ret_span,
                    expected: ret,
                    got,
                    loc,
                }
            })?;
            Some(value)
        };

        let result = {
            let kind = tir::Kind::Return(value.into());
            let ent = tir::Ent::with_flags(
                kind,
                self.types.builtin.nothing,
                tir::Flags::TERMINATING,
                span,
            );
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn build_if(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let &[cond, then, otherwise] = self.ast.children(ast) else {
            unreachable!()
        };

        let cond = self.build_expr(cond)?;
        drop(
            self.expect_tir_ty(cond, self.types.builtin.bool, |_, got, loc| {
                Error::IfConditionTypeMismatch { got, loc }
            }),
        );

        let then = self.build_block(then);
        let otherwise = if otherwise.is_reserved_value() {
            None
        } else {
            Some(self.build_block(otherwise)?)
        };
        let then = then?;

        let ty = if let Some(otherwise) = otherwise {
            let then = self.body.ents[then].ty;
            let otherwise = self.body.ents[otherwise].ty;
            if then == otherwise {
                then
            } else {
                self.types.builtin.nothing
            }
        } else {
            self.types.builtin.nothing
        };

        let flags = {
            let ents = &self.body.ents;

            let then_flags = ents[then].flags;
            let otherwise_flags = otherwise
                .map(|otherwise| ents[otherwise].flags)
                .unwrap_or(tir::Flags::empty());

            (then_flags & otherwise_flags) & tir::Flags::TERMINATING
        };

        let result = {
            let kind = tir::Kind::If(cond, then, otherwise.into());
            let ent = tir::Ent::with_flags(kind, ty, flags, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn build_ident(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let id = self.sources.id(span);

        let value = self.scope.get::<Tir>(self.diagnostics, id, span)?;

        // this allows better error messages
        let result = {
            let mut copy = self.body.ents[value];
            copy.kind = tir::Kind::Access(value);
            copy.span = span;
            self.body.ents.push(copy)
        };

        Ok(result)
    }

    fn build_binary(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let &[left, op, right] = self.ast.children(ast) else {
            unreachable!()
        };

        // we walk the ast even if something fails for better error diagnostics
        let (left, right) = {
            let left = self.build_expr(left);
            let right = self.build_expr(right);
            (left?, right?)
        };

        let left_ty = self.body.ents[left].ty;

        let op_span = self.ast.nodes[op].span;
        let id = {
            let op_id = {
                let str = self.sources.display(op_span);
                if str == "=" {
                    return self.build_assign(left, right, span);
                }

                ID::new(str)
            };

            let left_id = self.base_id_of(left_ty);

            Self::binary_id(left_id, op_id)
        };

        let Ok(func) = self.scope.get::<Func>(self.diagnostics, id, op_span) else {
            self.diagnostics.push(Error::BinaryOperatorNotFound {
                left_ty: self.body.ents[left].ty,
                right_ty: self.body.ents[right].ty,
                loc: op_span,
            });
            return Err(());
        };

        /* sanity check */
        {
            let func_ent = &self.funcs[func];
            let args = self.types.args.get(func_ent.sig.args);

            let arg_count = args.len();
            if arg_count != 2 {
                self.diagnostics.push(Error::OperatorArgCountMismatch {
                    because: func_ent.name,
                    expected: 2,
                    got: arg_count,
                    loc: op_span,
                });
                return Err(());
            }

            let expected = args[1];
            self.expect_tir_ty(right, expected, |_, got, loc| Error::BinaryTypeMismatch {
                expected,
                got,
                loc,
            })?;
        }

        let tir = {
            let ty = self.funcs[func].sig.ret;
            let args = self.body.cons.push(&[left, right]);
            let kind = tir::Kind::Call(left_ty.into(), func, args);
            let ent = tir::Ent::new(kind, ty, span);
            self.body.ents.push(ent)
        };

        Ok(tir)
    }

    fn build_assign(&mut self, left: Tir, right: Tir, span: Span) -> errors::Result<Tir> {
        let tir::Ent {
            ty,
            flags,
            span: left_span,
            kind,
        } = self.body.ents[left];

        if !flags.contains(tir::Flags::ASSIGNABLE) {
            let because = if let tir::Kind::Access(here) = kind {
                Some(self.body.ents[here].span)
            } else {
                None
            };
            self.diagnostics.push(Error::AssignToNonAssignable {
                because,
                loc: left_span,
            });
        }

        self.expect_tir_ty(right, ty, |_, got, loc| Error::AssignTypeMismatch {
            because: left_span,
            expected: ty,
            got,
            loc,
        })?;

        let result = {
            let kind = tir::Kind::Assign(left, right);
            let ent = tir::Ent::new(kind, ty, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn find_field(&mut self, on: Ty, id: ID, loc: Span) -> errors::Result<(SField, Ty)> {
        self.types
            .sfield_lookup
            .get(id)
            .map(|f| {
                let ty = self.types.sfields[f.field].ty;
                if let ty::Kind::Instance(_, params) = self.types.ents[on].kind {
                    (f.field, self.types.ensure_no_param(ty, params))
                } else {
                    (f.field, ty)
                }
            })
            .ok_or_else(|| {
                let candidates = if let ty::Kind::Struct(fields) = self.types.ents[on].kind {
                    self.types
                        .sfields
                        .get(fields)
                        .iter()
                        .map(|sfref| sfref.span)
                        .collect()
                } else {
                    Vec::new()
                };

                self.diagnostics.push(Error::UnknownField {
                    candidates,
                    on,
                    loc,
                });
            })
    }

    fn expect_tir_ty(
        &mut self,
        right: Tir,
        ty: Ty,
        err_fact: impl Fn(&mut Self, Ty, Span) -> Error,
    ) -> errors::Result {
        let tir::Ent { ty: got, span, .. } = self.body.ents[right];
        self.expect_ty(ty, got, |s| err_fact(s, got, span))
    }

    fn expect_ty(
        &mut self,
        ty: Ty,
        got: Ty,
        err_fact: impl Fn(&mut Self) -> Error,
    ) -> errors::Result {
        if ty != got {
            let error = err_fact(self);
            self.diagnostics.push(error);
            Err(())
        } else {
            Ok(())
        }
    }

    pub fn binary_id(left: ID, op: ID) -> ID {
        ID::new("<binary>") + left + op
    }

    pub fn owned_func_id(ty: ID, name: ID) -> ID {
        ty + name
    }

    fn base_id_of(&self, ty: Ty) -> ID {
        if self.func.is_reserved_value() {
            return self.types.base_id_of(ty, TyList::reserved_value());
        }
        let params = self.funcs[self.func].sig.params;
        self.types.base_id_of(ty, params)
    }
}

impl AstIDExt for Builder<'_> {
    fn state(&self) -> (&Data, &Sources) {
        (self.ast, self.sources)
    }
}

#[derive(Debug, Copy, Clone, Default)]
pub struct Ent {
    pub sig: Sig,
    pub name: Span,
    pub kind: Kind,
    pub body: Tir,
    pub args: TirList,
    pub flags: Flags,
}

impl Ent {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_link_name(&self, types: &Types, sources: &Sources, buffer: &mut String) {
        buffer.write_str(sources.display(self.name)).unwrap();
        if !self.sig.params.is_reserved_value() {
            buffer.write_char('[').unwrap();
            for &ty in types.args.get(self.sig.params) {
                ty.display(types, sources, buffer).unwrap();
                buffer.write_char(',').unwrap();
            }
            buffer.pop().unwrap();
            buffer.write_char(']').unwrap();
        }
    }
}

bitflags::bitflags! {
    #[derive(Default)]
    pub struct Flags: u32 {
        const ENTRY = 1 << 0;
        const GENERIC = 1 << 1;
        const INLINE = 1 << 2;
        const EXTERNAL = 1 << 3;
    }
}

crate::impl_bool_bit_and!(Flags);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Kind {
    Local,
    External,
    // (bound, relative index)
    Bound(Ty, u32),
    Owned(Ty),
    Instance(Func),
    Builtin,
}

impl Default for Kind {
    fn default() -> Self {
        Kind::Local
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Sig {
    pub params: TyList,
    pub call_conv: Span,
    pub args: TyList,
    pub ret: Ty,
}

pub struct SignatureDisplay<'a> {
    pub sig: &'a Sig,
    pub sources: &'a Sources,
    pub types: &'a Types,
}

impl<'a> SignatureDisplay<'a> {
    pub fn new(sig: &'a Sig, sources: &'a Sources, types: &'a Types) -> Self {
        SignatureDisplay {
            sig,
            types,
            sources,
        }
    }
}

impl std::fmt::Display for SignatureDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        for (i, &ty) in self.types.args.get(self.sig.args).iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", ty::Display::new(self.types, self.sources, ty))?;
        }
        write!(
            f,
            ") -> {}",
            ty::Display::new(self.types, self.sources, self.sig.ret)
        )?;
        Ok(())
    }
}

lexer::gen_entity!(Func);
lexer::gen_entity!(FuncList);
