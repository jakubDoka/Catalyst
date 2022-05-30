use module_types::module::Modules;

use crate::{scope::ScopeContext, TyError, *};

pub struct BoundVerifier<'a> {
    pub ctx: &'a mut ScopeContext,
    pub ast_data: &'a AstData,
    pub types: &'a Types,
    pub ty_lists: &'a mut TyLists,
    pub builtin_types: &'a BuiltinTypes,
    pub modules: &'a mut Modules,
    pub scope: &'a mut Scope,
    pub diagnostics: &'a mut errors::Diagnostics,
    pub funcs: &'a mut Funcs,
    pub func_lists: &'a mut TFuncLists,
    pub bound_impls: &'a mut BoundImpls,
    pub sources: &'a Sources,
    pub func_meta: &'a FuncMeta,
}

#[macro_export]
macro_rules! bound_verifier {
    ($self:expr) => {
        BoundVerifier::new(
            &mut $self.scope_context,
            &$self.ast_data,
            &$self.types,
            &mut $self.ty_lists,
            &$self.builtin_types,
            &mut $self.modules,
            &mut $self.scope,
            &mut $self.diagnostics,
            &mut $self.funcs,
            &mut $self.func_lists,
            &mut $self.bound_impls,
            &$self.sources,
            &$self.func_meta,
        )
    };
}

impl<'a> BoundVerifier<'a> {
    pub fn new(
        ctx: &'a mut ScopeContext,
        ast: &'a AstData,
        types: &'a Types,
        ty_lists: &'a mut TyLists,
        builtin_types: &'a BuiltinTypes,
        modules: &'a mut Modules,
        scope: &'a mut Scope,
        diagnostics: &'a mut errors::Diagnostics,
        funcs: &'a mut Funcs,
        func_lists: &'a mut TFuncLists,
        bound_impls: &'a mut BoundImpls,
        sources: &'a Sources,
        func_meta: &'a FuncMeta,
    ) -> Self {
        Self {
            ctx,
            ast_data: ast,
            types,
            ty_lists,
            builtin_types,
            modules,
            scope,
            diagnostics,
            funcs,
            func_lists,
            bound_impls,
            sources,
            func_meta,
        }
    }

    pub fn verify(&mut self) {
        // we have to collect all functions first and the check if all bound
        // functions are implemented, its done here
        while let Some((implementor, bound, impl_block)) = self.ctx.bounds_to_verify.pop() {
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
                    let Ok(func) = self.scope.get::<Func>(self.diagnostics, id, span) else {
                        continue;
                    };

                    let id = {
                        let func = ast::id_of(ident, self.ast_data, self.sources);
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

            let impl_span = self.ast_data.nodes[impl_block].span;

            // check if all functions exist and match the signature
            let funcs = {
                let TyKind::Bound(funcs) = self.types[bound].kind else {
                    unreachable!();
                };

                // TODO: don't allocate if it impacts performance
                let mut vec = self.func_lists.get(funcs).to_vec();
                for func in &mut vec {
                    let ent = &self.func_meta[*func];

                    let (sugar_id, certain_id) = {
                        let func = self.sources.id_of(ent.name);
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
                span: self.ast_data.nodes[impl_block].span,
                funcs,
            };

            assert!(self.bound_impls.insert(id, bound).is_some());
        }
    }

    pub fn compare_signatures(&mut self, bound_func: Func, impl_func: Func) -> errors::Result {
        let a = self.func_meta[impl_func].sig;
        let b = self.func_meta[bound_func].sig;

        let a_param_len = self.ty_lists.len(a.params);
        let b_param_len = self.ty_lists.len(b.params);

        // println!("{}", self.func_meta[bound_func].name.log(self.sources));

        if a_param_len != b_param_len - 1 {
            self.diagnostics.push(TyError::BoundImplFuncParamCount {
                impl_func: self.func_meta[impl_func].name,
                bound_func: self.func_meta[bound_func].name,
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
                let children = self.ast_data.children(self.ctx.func_ast[impl_func]);
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
