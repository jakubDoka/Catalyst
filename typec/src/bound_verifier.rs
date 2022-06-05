
use module_types::scope::ScopeFindError;

use crate::{TyError, *};

impl BoundVerifier<'_> {
    pub fn verify(&mut self) {
        // we have to collect all functions first and the check if all bound
        // functions are implemented, its done here
        while let Some((implementor, bound, impl_block)) = self.scope_context.bounds_to_verify.pop() {
            let &[.., implementor_ast, body] = self.ast_data.children(impl_block) else {
                unreachable!();
            };
            let implementor_span = self.ast_data.nodes[implementor_ast].span;

            // handle use expressions
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
                    let func = self.scope.get_concrete::<Func>(id);
                    let Ok(func) = func else {
                        todo!("{func:?}");
                    };

                    let id = {
                        let func = ast::id_of(ident, self.ast_data, self.sources);
                        let bound = self.types[bound].id;
                        let implementor = self.types[implementor].id;
                        ID::bound_impl_func(bound, implementor, func)
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

            // let impl_span = self.ast_data.nodes[impl_block].span;

            // check if all functions exist and match the signature
            let funcs = {
                let TyKind::Bound(funcs) = self.types[bound].kind else {
                    unreachable!();
                };

                // TODO: don't allocate if it impacts performance
                let mut vec = self.func_lists.get(funcs).to_vec();
                for func in &mut vec {
                    let ent = &self.funcs[func.meta()];

                    let (sugar_id, certain_id) = {
                        let func = self.sources.id_of(ent.name);
                        let ty = self.types[implementor].id;
                        let sugar_id = ID::owned(ty, func);
                        let bound = self.types[bound].id;
                        let certain_id = ID::bound_impl_func(bound, ty, func);
                        (sugar_id, certain_id)
                    };

                    let maybe_other = Err(ScopeFindError::NotFound) // looks better
                        .or_else(|_| self.scope.get_concrete::<Func>(certain_id))
                        .or_else(|_| self.scope.get_concrete::<Func>(sugar_id));

                    let Ok(other) = maybe_other else {
                        todo!("{maybe_other:?}");
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
                span: self.ast_data.nodes[impl_block].span,
                funcs,
            };

            assert!(self.bound_impls.insert(id, bound).is_some());
        }
    }

    pub fn compare_signatures(&mut self, bound_func: Func, impl_func: Func) -> errors::Result {
        let FuncMeta { params: a_params, sig: a, .. } = self.funcs[impl_func.meta()];
        let FuncMeta { params: b_params, sig: b, .. } = self.funcs[bound_func.meta()];

        let a_param_len = self.ty_lists.len(a_params);
        let b_param_len = self.ty_lists.len(b_params);

        // println!("{}", self.func_meta[bound_func].name.log(self.sources));

        if a_param_len != b_param_len - 1 {
            self.diagnostics.push(TyError::BoundImplFuncParamCount {
                impl_func: self.funcs[impl_func.meta()].name,
                bound_func: self.funcs[bound_func.meta()].name,
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
                let children = self.ast_data.children(self.scope_context.func_ast[impl_func]);
                let has_ret = (a.ret != self.builtin_types.nothing) as usize;
                &children[ast::FUNCTION_ARG_START..children.len() - ast::FUNCTION_RET + has_ret]
            };

            let a = a_args.iter().chain(std::iter::once(&a.ret));
            let b = b_args.iter().chain(std::iter::once(&b.ret));
            a.zip(b).zip(ast_params)
        };

        for ((&referenced, &parametrized), &ast) in iter {
            let span = self.ast_data.nodes[ast].span;
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
