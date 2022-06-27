use module_types::*;

use crate::*;
use ast::*;
use lexer::*;
use storage::*;
use typec_types::*;

impl BoundVerifier<'_> {
    pub fn verify(&mut self) {
        while let Some((implementor, bound, impl_block)) = self.scope_context.bounds_to_verify.pop()
        {
            // let implementor = self.types.base_of(implementor); // already based

            let &[.., implementor_ast, body] = self.ast_data.children(impl_block) else {
                unreachable!();
            };
            let implementor_span = self.ast_data.nodes[implementor_ast].span;

            let implementor_id = self.types[implementor].id;
            let id = {
                let bound = self.types[bound].id;
                ID::bound_impl(bound, implementor_id)
            };

            let Some(bound_ent) = self.bound_impls.get_mut(id) else {
                unreachable!();
            };

            let param_offset = self.ty_lists.len_of(bound_ent.params);

            if !body.is_reserved_value() {
                for &ast in self.ast_data.children(body) {
                    if self.ast_data.nodes[ast].kind != AstKind::UseBoundFunc {
                        continue;
                    }

                    let &[func, ident] = self.ast_data.children(ast) else {
                        unreachable!();
                    };

                    let Ok(id) = ident_hasher!(self).ident_id(func, Some((implementor, implementor_span))) else {
                        continue;
                    };

                    let span = self.ast_data.nodes[func].span;
                    let matcher = matcher!(Ty = "type");
                    let handler = scope_error_handler(
                        self.diagnostics,
                        not_found_handler(span),
                        span,
                        "function",
                        matcher,
                    );
                    let Ok(func) = self.scope.get_concrete::<Func>(id).map_err(handler) else {
                        continue;
                    };

                    let id = {
                        let func_id = ast::id_of(ident, self.ast_data, self.sources);
                        let bound_id = self.types[bound].id;

                        let bound_func_id = ID::owned(bound_id, func_id);

                        let matcher = matcher!(Ty = "type");
                        let handler = scope_error_handler(
                            self.diagnostics,
                            || TyError::UnexpectedBoundFunc { bound, loc: span },
                            span,
                            "function",
                            matcher,
                        );

                        let Ok(bound_func) = self.scope.get_concrete::<Func>(bound_func_id).map_err(handler) else {
                            continue;
                        };

                        drop(self.compare_signatures(bound_func, func, param_offset));

                        ID::bound_impl_func(implementor_id, func_id)
                    };

                    {
                        let item = ModuleItem::new(id, func, span);
                        self.scope.insert_current(self.diagnostics, item);
                        self.modules[span.source()].items.push(item);
                    }
                }
            }

            let funcs = {
                let TyKind::Bound(funcs) = self.types[bound].kind else {
                    unreachable!();
                };

                // TODO: don't allocate if it impacts performance
                let mut vec = self.vec_pool.alloc(self.func_lists.get(funcs));
                for func in vec.iter_mut() {
                    let ent = &self.funcs[func.meta()];

                    let (sugar_id, certain_id) = {
                        let func = self.sources.id_of(ent.name);
                        let bound = self.types[bound].id;
                        let certain_id =
                            ID::bound_impl_func(implementor_id, ID::owned(bound, func));
                        let sugar_id = ID::owned(implementor_id, func);
                        (sugar_id, certain_id)
                    };

                    let maybe_other = Err(ScopeFindError::NotFound) // looks better
                        .or_else(|_| self.scope.get_concrete::<Func>(certain_id))
                        .or_else(|_| self.scope.get_concrete::<Func>(sugar_id));

                    let Ok(other) = maybe_other else {
                        todo!("{maybe_other:?}");
                    };

                    drop(self.compare_signatures(*func, other, param_offset));

                    *func = other;
                }

                self.func_lists.push(&vec)
            };

            let Some(bound_ent) = self.bound_impls.get_mut(id) else {
                unreachable!();
            };

            bound_ent.funcs = funcs;
        }
    }

    pub fn compare_signatures(
        &mut self,
        bound_func: Func,
        impl_func: Func,
        param_offset: usize,
    ) -> errors::Result {
        let FuncMeta {
            params: a_params,
            sig: a,
            ..
        } = self.funcs[impl_func.meta()];
        let FuncMeta {
            params: b_params,
            sig: b,
            ..
        } = self.funcs[bound_func.meta()];

        println!("{:?} {:?}", self.ty_lists.get(a_params), self.ty_lists.get(b_params));

        let a_param_len = self.ty_lists.len_of(a_params);
        let b_param_len = self.ty_lists.len_of(b_params);

        // println!("{}", self.func_meta[bound_func].name.log(self.sources));

        if a_param_len - param_offset != b_param_len - 1 {
            self.diagnostics.push(TyError::BoundImplFuncParamCount {
                impl_func: self.funcs[impl_func.meta()].name,
                bound_func: self.funcs[bound_func.meta()].name,
                expected: b_param_len - 1,
                found: a_param_len,
            });
            return Err(());
        }

        let iter = {
            // TODO: same here
            let a_args = self.ty_comps.get(a.args);
            let b_args = self.ty_comps.get(b.args);

            debug_assert_eq!(a_args.len(), b_args.len());

            let ast_params = {
                let children = self
                    .ast_data
                    .children(self.scope_context.func_ast[impl_func]);
                let has_ret = (a.ret != self.builtin_types.nothing) as usize;
                &children[ast::FUNCTION_ARG_START..children.len() - ast::FUNCTION_RET + has_ret]
            };

            let a = a_args
                .iter()
                .map(|arg| arg.ty)
                .chain(std::iter::once(a.ret));
            let b = b_args
                .iter()
                .map(|arg| arg.ty)
                .chain(std::iter::once(b.ret));
            a.zip(b).zip(ast_params)
        };

        // TODO: don't allocate if this becomes issue, bounds might get implemented a lot
        let mut params = self.vec_pool.of_size(Ty::reserved_value(), b_param_len);

        let mut failed = false;
        for ((referenced, parametrized), &ast) in iter {
            let span = self.ast_data.nodes[ast].span;
            if let Err(err) = bound_checker!(self).infer_parameters(
                referenced,
                parametrized,
                &mut params,
                self.ty_lists.get(b_params),
                span,
                true,
            ) {
                if let Some(err) = err {
                    self.diagnostics.push(err);
                } else {
                    failed = true;
                }
            }
        }

        if failed {
            Err(())
        } else {
            Ok(())
        }
    }
}
