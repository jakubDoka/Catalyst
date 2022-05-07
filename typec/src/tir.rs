use module_types::module::Modules;

use crate::{TyError, *, scope::ScopeContext};

pub struct BoundVerifier<'a> {
    pub ctx: &'a mut ScopeContext,
    pub ast: &'a AstData,
    pub types: &'a Types,
    pub ty_lists: &'a mut TyLists,
    pub builtin_types: &'a BuiltinTable,
    pub modules: &'a mut Modules,
    pub scope: &'a mut Scope,
    pub diagnostics: &'a mut errors::Diagnostics,
    pub funcs: &'a mut Funcs,
    pub func_lists: &'a mut TFuncLists,
    pub bound_impls: &'a mut BoundImpls,
    pub sources: &'a Sources,
}

impl<'a> BoundVerifier<'a> {
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
                        if self.ast.nodes[ast].kind != ast::AstKind::UseBoundFunc {
                            continue;
                        }

                        let &[func, ident] = self.ast.children(ast) else {
                            unreachable!();
                        };

                        let Ok(id) = ident_hasher!(self).ident_id(func, Some((implementor, implementor_span))) else {
                            continue;
                        };

                        let span = self.ast.nodes[func].span;
                        let Ok(func) = self.scope.get::<Func>(self.diagnostics, id, span) else {
                            continue;
                        };

                        let id = {
                            let func = ast::id_of(ident, self.ast, self.sources);
                            let bound = self.types[bound].id;
                            let implementor = self.types[implementor].id;
                            ID::bound_impl_owned_func(bound, implementor, func)
                        };

                        {
                            let item = module::ModuleItem::new(id, func, span);
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
                    let TyKind::Bound(funcs) = self.types[bound].kind else {
                        unreachable!();
                    };

                    // TODO: don't allocate if it impacts performance
                    let mut vec = self.func_lists.get(funcs).to_vec();
                    for func in &mut vec {
                        let ent = &self.funcs[*func];

                        let (sugar_id, certain_id) = {
                            let func = self.sources.id(ent.name);
                            let ty = self.types[implementor].id;
                            let sugar_id = ID::owned_func(ty, func);
                            let bound = self.types[bound].id;
                            let certain_id = ID::bound_impl_owned_func(bound, ty, func);
                            (sugar_id, certain_id)
                        };

                        let maybe_other = None // looks better
                            .or_else(|| self.scope.weak_get::<Func>(certain_id))
                            .or_else(|| self.scope.weak_get::<Func>(sugar_id));

                        let Some(other) = maybe_other else {
                            self.diagnostics.push(TyError::MissingBoundImplFunc {
                                func: ent.name,
                                loc: impl_span,
                            });
                            continue;
                        };

                        drop(self.compare_signatures(*func, other));

                        *func = other;
                    }

                    self.func_lists.push(&vec)
                };

                let id = {
                    let implementor = self.types[implementor].id;
                    let bound = self.types[bound].id;
                    ID::bound_impl(bound, implementor)
                };
                
                let bound = BoundImpl {
                    span: self.ast.nodes[impl_block].span,
                    funcs,
                };

                assert!(self.bound_impls.insert(id, bound).is_some());
            }
        }

        Ok(())
    }

    pub fn compare_signatures(&mut self, bound_func: Func, impl_func: Func) -> errors::Result {
        let a = self.funcs[impl_func].sig;
        let b = self.funcs[bound_func].sig;

        let a_param_len = self.ty_lists.len(a.params);
        let b_param_len = self.ty_lists.len(b.params);

        // println!("{}", self.funcs[bound_func].name.log(self.sources));

        if a_param_len != b_param_len - 1 {
            self.diagnostics.push(TyError::BoundImplFuncParamCount {
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
            let a_args = self.ty_lists.get(a.args);
            let b_args = self.ty_lists.get(b.args);
            let ast_params = {
                let children = self.ast.children(self.ctx.func_ast[impl_func]);
                let has_ret = (a.ret != self.builtin_types.nothing) as usize;
                &children
                    [ast::FUNCTION_ARG_START..children.len() - ast::FUNCTION_RET + has_ret]
            };

            let a = a_args.iter().chain(std::iter::once(&a.ret));
            let b = b_args.iter().chain(std::iter::once(&b.ret));
            a.zip(b).zip(ast_params)
        };

        for ((&referenced, &parametrized), &ast) in iter {
            let span = self.ast.nodes[ast].span;
            drop(infer_parameters(
                referenced,
                parametrized,
                &mut params,
                span,
                self.types,
                self.ty_lists,
                self.bound_impls,
                self.diagnostics,
            ));
        }

        Ok(())
    }
}

pub struct TirBuilder<'a> {
    pub func: Func,
    pub funcs: &'a mut Funcs,
    pub func_lists: &'a TFuncLists,
    pub ty_lists: &'a mut TyLists,
    pub instances: &'a mut Instances,
    pub sfields: &'a mut SFields,
    pub sfield_lookup: &'a mut SFieldLookup,
    pub builtin_types: &'a BuiltinTable,
    pub bound_impls: &'a mut BoundImpls,
    pub ctx: &'a mut ScopeContext,
    pub body: &'a mut TirData,
    pub scope: &'a mut Scope,
    pub types: &'a mut Types,
    pub temp: &'a mut FramedStack<Tir>,
    pub sources: &'a Sources,
    pub ast: &'a AstData,
    pub modules: &'a mut Modules,
    pub diagnostics: &'a mut errors::Diagnostics,
}

impl<'a> TirBuilder<'a> {

    pub fn build(&mut self) -> errors::Result {
        let TFuncEnt {
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
            if flags.contains(TFuncFlags::EXTERNAL) {
                self.funcs[self.func].kind = TFuncKind::External;
            }
            return Ok(());
        }

        self.scope.mark_frame();
        if let TFuncKind::Owned(ty) = kind {
            let span = self.ast.nodes[ast].span;
            self.scope.push_item("Self", ScopeItem::new(ty, span));
        }

        self.build_generics(params, generics);

        let args = {
            let ast_args =
                &header[ast::FUNCTION_ARG_START..header.len() - ast::FUNCTION_ARG_END];
            let args = self.ty_lists.get(args);
            let mut tir_args = Vec::with_capacity(args.len());
            for (i, (&ast, &ty)) in ast_args.iter().zip(args).enumerate() {
                // println!("=== {} {} {:?}", ty_display!(self, ty), self.ast.nodes[ast].span.log(self.sources), self.types[ty].flags);
                self.ctx.use_type(ty, self.types);

                let &[name, ..] = self.ast.children(ast) else {
                    unreachable!();
                };
                let span = self.ast.nodes[name].span;

                let arg = {
                    let kind = TirKind::Argument(i as u32);
                    let ent = TirEnt::new(kind, ty, span);
                    self.body.ents.push(ent)
                };

                {
                    let id = {
                        let str = self.sources.display(span);
                        ID::new(str)
                    };
                    let item = ScopeItem::new(arg, span);
                    self.scope.push_item(id, item);
                }

                tir_args.push(arg);
            }
            self.body.cons.push(&tir_args)
        };

        let root = self.build_block(body_ast)?;
        self.funcs[self.func].body = root;
        self.funcs[self.func].args = args;

        self.body.used_types = self.ty_lists.push(&self.ctx.used_types);
        self.ctx.used_types.clear();
        self.ctx.used_types_set.clear();

        if !self.body.ents[root].flags.contains(TirFlags::TERMINATING)
            && ret != self.builtin_types.nothing
        {
            let because = Some(self.ast.nodes[ret_ast].span);
            self.expect_tir_ty(root, ret, |_, got, loc| TyError::ReturnTypeMismatch {
                because,
                expected: ret,
                got,
                loc,
            })?;
        }

        self.scope.pop_frame();

        Ok(())
    }

    fn build_generics(&mut self, params: TyList, generics: Ast) {
        if generics.is_reserved_value() {
            return;
        }

        let ast = self.ast.children(generics);
        let bounds = self.ty_lists.get(params);
        for (&item, &bound_combo) in ast.iter().zip(bounds) {
            let name = self.ast.children(item)[0];
            let span = self.ast.nodes[name].span;
            let str = self.sources.display(span);
            self.scope.push_item(str, ScopeItem::new(bound_combo, span));

            let TyKind::Param(_, bounds, ..) = self.types[bound_combo].kind else {
                unreachable!();
            };

            for &bound in self.ty_lists.get(bounds) {
                let TyKind::Bound(funcs) = self.types[bound].kind else {
                    unreachable!();
                };

                // TODO: can caching ids make less cache misses?
                for &func in self.func_lists.get(funcs) {
                    let name = self.funcs[func].name;
                    let id = {
                        let id = self.sources.id(name);
                        let bound = self.types[bound_combo].id;
                        ID::owned_func(bound, id)
                    };
                    self.scope.push_item(id, ScopeItem::new(func, name));
                }
            }
        }
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
            terminating |= self.body.ents[expr].flags.contains(TirFlags::TERMINATING);
            if terminating {
                break; // TODO: emit warning
            }
        }

        let result = {
            let ty = if let Some(expr) = final_expr {
                self.body.ents[expr].ty
            } else {
                self.builtin_types.nothing
            };

            let slice = self.temp.top_frame();
            let items = self.body.cons.push(slice);

            let kind = TirKind::Block(items);
            let flags = TirFlags::TERMINATING & terminating;
            let ent = TirEnt::with_flags(kind, ty, flags, span);
            self.body.ents.push(ent)
        };

        self.temp.pop_frame();
        self.scope.pop_frame();

        Ok(result)
    }

    fn build_expr(&mut self, ast: Ast) -> errors::Result<Tir> {
        let ast::AstEnt { kind, span, .. } = self.ast.nodes[ast];
        let expr = match kind {
            ast::AstKind::Unary => self.build_unary(ast)?,
            ast::AstKind::Binary => self.build_binary(ast)?,
            ast::AstKind::Ident => self.build_ident(ast)?,
            ast::AstKind::If => self.build_if(ast)?,
            ast::AstKind::Return => self.build_return(ast)?,
            ast::AstKind::Int(width) => self.build_int(ast, width)?,
            ast::AstKind::Bool(value) => self.build_bool(ast, value)?,
            ast::AstKind::Char => self.build_char(ast)?,
            ast::AstKind::Variable(mutable) => self.build_variable(mutable, ast)?,
            ast::AstKind::Constructor => self.build_constructor(ast)?,
            ast::AstKind::DotExpr => self.build_dot_expr(ast)?,
            ast::AstKind::Call => self.build_call(ast)?,
            ast::AstKind::Block => self.build_block(ast)?,
            ast::AstKind::Loop => self.build_loop(ast)?,
            ast::AstKind::Break => self.build_break(ast)?,
            ast::AstKind::Deref => self.build_deref(ast)?,
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

        if !matches!(self.types[ty].kind, TyKind::Ptr(..)) {
            self.diagnostics.push(TyError::NonPointerDereference {
                loc: self.ast.nodes[ast].span,
                ty,
            });
            return Err(());
        }

        Ok(self.deref_ptr(target))
    }

    fn build_char(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let ent = TirEnt::new(TirKind::CharLit, self.builtin_types.char, span);
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
                let ty = self.types.base_id_of(operand_ty);

                let op = self.sources.id(span);

                ID::unary(ty, op)
            };

            self.scope.get::<Func>(self.diagnostics, id, span)?
        };

        let result = {
            let ty = self.funcs[func].sig.ret;
            let span = self.ast.nodes[ast].span;
            let args = self.body.cons.push(&[expr]);
            let kind = TirKind::Call(operand_ty.into(), func, args);
            let ent = TirEnt::new(kind, ty, span);
            self.body.ents.push(ent)
        };

        Ok(result)
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
            let TirKind::LoopInProgress(ret, infinite) = &mut self.body.ents[loop_expr].kind else {
                unreachable!();
            };

            *infinite = false;

            if let Some(ret) = ret.expand() {
                let TirEnt {
                    span: ret_span, ty, ..
                } = self.body.ents[ret];
                if let Some(value) = value {
                    self.expect_tir_ty(value, ty, |_, got, loc| TyError::BreakValueTypeMismatch {
                        because: ret_span,
                        expected: ty,
                        got,
                        loc,
                    })?;
                } else {
                    self.diagnostics.push(TyError::MissingBreakValue {
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
            let kind = TirKind::Break(loop_expr, value.into());
            let ent = TirEnt::with_flags(
                kind,
                self.builtin_types.nothing,
                TirFlags::TERMINATING,
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
            let kind = TirKind::LoopInProgress(None.into(), true);
            let ent = TirEnt::new(kind, self.builtin_types.nothing, span);
            self.body.ents.push(ent)
        };

        // break and continue will propagate side effects so we save the lookup

        self.scope
            .push_item("<loop>", ScopeItem::new(loop_slot, span));

        let block = self.build_block(body_ast)?;

        self.scope.pop_item();

        {
            let TirKind::LoopInProgress(ret, infinite) = self.body.ents[loop_slot].kind else {
                unreachable!();
            };
            let ty = ret
                .map(|ret| self.body.ents[ret].ty)
                .unwrap_or(self.builtin_types.nothing);
            let flags = TirFlags::TERMINATING & infinite;
            let kind = TirKind::Loop(block);
            let span = span;
            self.body.ents[loop_slot] = TirEnt::with_flags(kind, ty, flags, span);
        }

        Ok(loop_slot)
    }

    fn build_call(&mut self, ast: Ast) -> errors::Result<Tir> {
        let &[caller, ..] = self.ast.children(ast) else {
            unreachable!();
        };

        let (id, fn_span, mut obj, caller, instantiation) =
            if self.ast.nodes[caller].kind == ast::AstKind::DotExpr {
                let &[expr, name] = self.ast.children(caller) else {
                    unreachable!();
                };

                let expr = self.build_expr(expr)?;
                let ty = {
                    let ty = self.body.ents[expr].ty;
                    self.types.base_of(ty)
                };

                let (ident, instantiation) = {
                    if ast::AstKind::Instantiation == self.ast.nodes[name].kind {
                        (self.ast.children(name)[0], Some(name))
                    } else {
                        (name, None)
                    }
                };

                let span = self.body.ents[expr].span;
                let name_id = ident_hasher!(self).ident_id(ident, Some((ty, span)))?;

                let span = self.ast.nodes[name].span;
                (name_id, span, Some(expr), Some(ty), instantiation)
            } else {
                let (ident, instantiation) = {
                    if ast::AstKind::Instantiation == self.ast.nodes[caller].kind {
                        (self.ast.children(caller)[0], Some(caller))
                    } else {
                        (caller, None)
                    }
                };

                let (name_id, owner) = ident_hasher!(self).ident_id_low(ident, None)?;

                let span = self.ast.nodes[caller].span;
                (name_id, span, None, owner.map(|(ty, _)| ty), instantiation)
            };

        // TODO: Handle function pointer as field
        let func = self.scope.get::<Func>(self.diagnostics, id, fn_span)?;

        let TFuncEnt { sig, flags, .. } = self.funcs[func];

        let arg_tys = self.ty_lists.get(sig.args).to_vec(); // TODO: avoid allocation

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
                self.diagnostics.push(TyError::FunctionParamMismatch {
                    because,
                    expected: arg_tys.len(),
                    got: args_len,
                    loc,
                });
                return Err(());
            }

            if !flags.contains(TFuncFlags::GENERIC) {
                // here we type check all arguments and then check for errors
                args.iter()
                    .zip(arg_tys)
                    .map(|(&arg, ty)| {
                        self.expect_tir_ty(arg, ty, |_, got, loc| TyError::CallArgTypeMismatch {
                            because,
                            expected: ty,
                            got,
                            loc,
                        })
                    })
                    .fold(Ok(()), |acc, err| acc.and(err))?;

                (sig.ret, func)
            } else {
                let params = self.ty_lists.get(sig.params);
                
                prepare_params(params, self.types);
                
                let mut param_slots = vec![Ty::default(); params.len()];

                if let Some(instantiation) = instantiation {
                    if params.len() > self.ast.children(instantiation).len() - 1 {
                        todo!("this is an error");
                    }

                    for (i, &param) in self.ast.children(instantiation)[1..].iter().enumerate() {
                        let Ok(ty) = ty_parser!(self).parse_type_optional(param) else {
                            continue;
                        };
                        param_slots[i] = ty;
                    }
                }


                args.iter()
                    .zip(arg_tys)
                    .map(|(&arg, arg_ty)| {
                        let TirEnt { ty, span, .. } = self.body.ents[arg];
                        infer_parameters(
                            ty,
                            arg_ty,
                            &mut param_slots,
                            span,
                            self.types,
                            self.ty_lists,
                            self.bound_impls,
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
                        self.diagnostics.push(TyError::UnknownGenericParam {
                            func: self.funcs[func].name,
                            param,
                            loc: span,
                        });
                        Err(())
                    })?;

                let generic = param_slots
                    .iter()
                    .any(|&params| self.types[params].flags.contains(TyFlags::GENERIC));

                let ret = instantiate(sig.ret, &param_slots, self.types, self.instances)?;

                // println!(",,,,,{}", ty_display!(self, ret));

                self.ctx.use_type(ret, self.types);

                if generic {
                    (ret, func)
                } else {
                    let func = {
                        let i_params = self.ty_lists.push(&param_slots);

                        // println!("====== {} {:?}", Display::new(self.types, self.sources, ret), self.types[ret]);

                        let sig = Sig {
                            params: i_params,
                            ret,
                            ..sig
                        };
                        let ent = TFuncEnt {
                            sig,
                            kind: TFuncKind::Instance(func),
                            flags: self.funcs[func].flags & !TFuncFlags::GENERIC,
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
            let kind = TirKind::Call(caller.into(), func, args);
            let ent = TirEnt::new(kind, ret, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn ptr_correct(&mut self, mut target: Tir, expected: Ty) -> errors::Result<Tir> {
        let ty = self.body.ents[target].ty;

        let target_depth = self.types.depth_of(ty);
        let expected_depth = self.types.depth_of(expected);

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
        let TirEnt {
            ty,
            span,
            flags,
            kind,
            ..
        } = &mut self.body.ents[target];
        let (span, ty) = (*span, *ty);
        if let &mut TirKind::Access(target) = kind {
            self.body.ents[target].flags.insert(TirFlags::SPILLED);
        } else {
            flags.insert(TirFlags::SPILLED);
        }

        let kind = TirKind::TakePtr(target);
        let ptr_ty = pointer_of(ty, self.types, self.instances);
        self.ctx.use_type(ptr_ty, self.types);
        let deref = TirEnt::new(kind, ptr_ty, span);
        self.body.ents.push(deref)
    }

    fn deref_ptr(&mut self, target: Tir) -> Tir {
        let span = self.body.ents[target].span;
        let kind = TirKind::DerefPointer(target);
        let deref_ty = self.types.deref(self.body.ents[target].ty);
        self.ctx.use_type(deref_ty, self.types);
        let deref = TirEnt::new(kind, deref_ty, span);
        self.body.ents.push(deref)
    }

    fn build_dot_expr(&mut self, ast: Ast) -> errors::Result<Tir> {
        let &[header, field] = self.ast.children(ast) else {
            unreachable!();
        };

        let mut header = self.build_expr(header)?;

        let ty = self.body.ents[header].ty;
        let base_ty = self.types.base_of(ty);

        let span = self.ast.nodes[field].span;
        let (field_id, field_ty) = {
            let id = {
                let ty_id = self.types[base_ty].id;
                let str = self.sources.display(span);
                ID::field(ty_id, ID::new(str))
            };
            self.find_field(base_ty, id, span)?
        };

        while base_ty != self.body.ents[header].ty {
            header = self.deref_ptr(header);
        }

        let result = {
            self.ctx.use_type(field_ty, self.types);
            let kind = TirKind::FieldAccess(header, field_id);
            let ent = TirEnt::new(kind, field_ty, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn build_bool(&mut self, ast: Ast, value: bool) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let ty = self.builtin_types.bool;
        let kind = TirKind::BoolLit(value);
        let ent = TirEnt::new(kind, ty, span);
        let result = self.body.ents.push(ent);
        Ok(result)
    }

    fn build_constructor(&mut self, ast: Ast) -> errors::Result<Tir> {
        let &[name, body] = self.ast.children(ast) else {
            unreachable!("{:?}", self.ast.children(ast));
        };

        let span = self.ast.nodes[name].span;
        let ty = {
            let id = ident_hasher!(self).ident_id(name, None)?;
            self.scope.get::<Ty>(self.diagnostics, id, span)?
        };

        self.build_constructor_low(ty, body)
    }

    fn build_constructor_low(&mut self, ty: Ty, body: Ast) -> errors::Result<Tir> {
        self.ctx.use_type(ty, self.types);

        let span = self.ast.nodes[body].span;
        let ty_id = self.types[ty].id;

        let TyKind::Struct(fields) = self.types[ty].kind else {
            self.diagnostics.push(TyError::ExpectedStruct {
                got: ty,
                loc: span,
            });
            return Err(());
        };

        let mut initial_values = vec![Tir::reserved_value(); self.sfields.get(fields).len()]; // TODO: don't allocate
        for &field in self.ast.children(body) {
            let &[name, expr] = self.ast.children(field) else {
                unreachable!();
            };

            let span = self.ast.nodes[name].span;
            let id = {
                let id = self.sources.id(span);
                ID::field(ty_id, id)
            };

            let Ok((field, field_ty)) = self.find_field(ty, id, span) else {
                continue;
            };

            let SFieldEnt {
                span: hint, index, ..
            } = self.sfields[field];

            let Ok(value) = (match self.ast.nodes[expr].kind {
                ast::AstKind::InlineConstructor => self.build_constructor_low(field_ty, expr),
                _ => self.build_expr(expr),
            }) else {
                continue;
            };

            // we ignore this to report more errors
            drop(self.expect_tir_ty(value, field_ty, |_, got, loc| {
                TyError::ConstructorFieldTypeMismatch {
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
                .map(|i| self.sfields.get(fields)[i].span)
                .collect::<Vec<_>>();

            self.diagnostics.push(TyError::ConstructorMissingFields {
                on: ty,
                missing,
                loc: span,
            });

            return Err(());
        }

        let result = {
            let span = self.ast.nodes[body].span;
            let fields = self.body.cons.push(&initial_values);
            let kind = TirKind::Constructor(fields);
            let ent = TirEnt::new(kind, ty, span);
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
            .insert(TirFlags::ASSIGNABLE & mutable);

        {
            let item = ScopeItem::new(value, span);
            self.scope.push_item(id, item);
        }

        let result = {
            let kind = TirKind::Variable(value);
            let ent = TirEnt::new(kind, self.builtin_types.nothing, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn build_int(&mut self, ast: Ast, width: i16) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let kind = TirKind::IntLit(width);
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

        let ent = TirEnt::new(kind, ty, span);
        Ok(self.body.ents.push(ent))
    }

    fn build_return(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let &[value] = self.ast.children(ast) else {
            unreachable!()
        };

        let ret = self.funcs[self.func].sig.ret;

        let value = if value.is_reserved_value() {
            self.expect_ty(self.builtin_types.nothing, ret, |s| {
                TyError::UnexpectedReturnValue {
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
                TyError::ReturnTypeMismatch {
                    because: ret_span,
                    expected: ret,
                    got,
                    loc,
                }
            })?;
            Some(value)
        };

        let result = {
            let kind = TirKind::Return(value.into());
            let ent = TirEnt::with_flags(
                kind,
                self.builtin_types.nothing,
                TirFlags::TERMINATING,
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
            self.expect_tir_ty(cond, self.builtin_types.bool, |_, got, loc| {
                TyError::IfConditionTypeMismatch { got, loc }
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
                self.builtin_types.nothing
            }
        } else {
            self.builtin_types.nothing
        };

        let flags = {
            let ents = &self.body.ents;

            let then_flags = ents[then].flags;
            let otherwise_flags = otherwise
                .map(|otherwise| ents[otherwise].flags)
                .unwrap_or(TirFlags::empty());

            (then_flags & otherwise_flags) & TirFlags::TERMINATING
        };

        let result = {
            let kind = TirKind::If(cond, then, otherwise.into());
            let ent = TirEnt::with_flags(kind, ty, flags, span);
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
            copy.kind = TirKind::Access(value);
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

            let left_id = self.types.base_id_of(left_ty);

            ID::binary(left_id, op_id)
        };

        let Ok(func) = self.scope.get::<Func>(self.diagnostics, id, op_span) else {
            self.diagnostics.push(TyError::BinaryOperatorNotFound {
                left_ty: self.body.ents[left].ty,
                right_ty: self.body.ents[right].ty,
                loc: op_span,
            });
            return Err(());
        };

        /* sanity check */
        {
            let func_ent = &self.funcs[func];
            let args = self.ty_lists.get(func_ent.sig.args);

            let arg_count = args.len();
            if arg_count != 2 {
                self.diagnostics.push(TyError::OperatorArgCountMismatch {
                    because: func_ent.name,
                    expected: 2,
                    got: arg_count,
                    loc: op_span,
                });
                return Err(());
            }

            let expected = args[1];
            self.expect_tir_ty(right, expected, |_, got, loc| TyError::BinaryTypeMismatch {
                expected,
                got,
                loc,
            })?;
        }

        let tir = {
            let ty = self.funcs[func].sig.ret;
            let args = self.body.cons.push(&[left, right]);
            let kind = TirKind::Call(left_ty.into(), func, args);
            let ent = TirEnt::new(kind, ty, span);
            self.body.ents.push(ent)
        };

        Ok(tir)
    }

    fn build_assign(&mut self, left: Tir, right: Tir, span: Span) -> errors::Result<Tir> {
        let TirEnt {
            ty,
            flags,
            span: left_span,
            kind,
        } = self.body.ents[left];

        if !flags.contains(TirFlags::ASSIGNABLE) {
            let because = if let TirKind::Access(here) = kind {
                Some(self.body.ents[here].span)
            } else {
                None
            };
            self.diagnostics.push(TyError::AssignToNonAssignable {
                because,
                loc: left_span,
            });
        }

        self.expect_tir_ty(right, ty, |_, got, loc| TyError::AssignTypeMismatch {
            because: left_span,
            expected: ty,
            got,
            loc,
        })?;

        let result = {
            let kind = TirKind::Assign(left, right);
            let ent = TirEnt::new(kind, ty, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn find_field(&mut self, on: Ty, id: ID, loc: Span) -> errors::Result<(SField, Ty)> {
        self
            .sfield_lookup
            .get(id)
            .map(|f| {
                let ty = self.sfields[f.field].ty;
                if let TyKind::Instance(_, params) = self.types[on].kind 
                    && let TyKind::Param(i, ..) = self.types[ty].kind {
                    (f.field, self.ty_lists.get(params)[i as usize])
                } else {
                    (f.field, ty)
                }
            })
            .ok_or_else(|| {
                let candidates = if let TyKind::Struct(fields) = self.types[on].kind {
                    self
                        .sfields
                        .get(fields)
                        .iter()
                        .map(|sfref| sfref.span)
                        .collect()
                } else {
                    Vec::new()
                };

                self.diagnostics.push(TyError::UnknownField {
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
        err_fact: impl Fn(&mut Self, Ty, Span) -> TyError,
    ) -> errors::Result {
        let TirEnt { ty: got, span, .. } = self.body.ents[right];
        self.expect_ty(ty, got, |s| err_fact(s, got, span))
    }

    fn expect_ty(
        &mut self,
        ty: Ty,
        got: Ty,
        err_fact: impl Fn(&mut Self) -> TyError,
    ) -> errors::Result {
        if ty != got {
            let error = err_fact(self);
            self.diagnostics.push(error);
            Err(())
        } else {
            Ok(())
        }
    }
}

pub struct IdentHasher<'a> {
    pub sources: &'a Sources,
    pub ast: &'a AstData,
    pub scope: &'a Scope,
    pub diagnostics: &'a mut Diagnostics,
    pub types: &'a Types,
}

impl<'a> IdentHasher<'a> {
    pub fn new(
        sources: &'a Sources,
        ast: &'a AstData,
        scope: &'a Scope,
        diagnostics: &'a mut Diagnostics,
        types: &'a Types,
    ) -> Self {
        Self {
            sources,
            ast,
            scope,
            diagnostics,
            types,
        }
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
                    let name = ast::id_of(item, self.ast, self.sources);
                    let ty = self.types.base_id_of(ty);
                    ID::owned_func(ty, name)
                };
    
                Ok((id + module_id, Some((ty, span))))
            }
            (&[module_or_type, item], None) => {
                let item_id = ast::id_of(item, self.ast, self.sources);
    
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
                            ID::owned_func(self.types.base_id_of(ty), item_id),
                            Some((ty, span)),
                        )
                    },
                )
            }
            (&[], None) => return Ok((ast::id_of(ast, self.ast, self.sources), None)),
            (&[], Some((ty, span))) => {
                let name = ast::id_of(ast, self.ast, self.sources);
                let ty_id = self.types.base_id_of(ty);
                Ok((ID::owned_func(ty_id, name), Some((ty, span))))
            }
            _ => {
                self.diagnostics.push(TyError::InvalidPath {
                    loc: self.ast.nodes[ast].span,
                });
                Err(())
            }
        }
    }    
}