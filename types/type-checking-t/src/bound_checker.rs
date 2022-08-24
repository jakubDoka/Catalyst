use storage::*;

use crate::*;

impl BoundChecker<'_> {
    pub fn infer_type(
        &self,
        _reference: Ty,
        _template: Ty,
        _infer_slots: &[Ty],
    ) -> Result<(), TyInferenceError> {
        todo!()
    }

    pub fn compare_bound_signatures(
        &mut self,
        r#impl: Impl,
        bound_func_index: usize,
        def: Def,
    ) -> Result<(), SignatureError> {
        let impl_ent = self.typec.impls[r#impl];
        let def_ent = &self.typec.defs[def];
        let base_bound = self.typec.instance_base_of(impl_ent.bound);
        let bound_func_ent = self.typec.func_of_bound(base_bound, bound_func_index);

        let (bound_func_params, impl_func_params) = (
            &self.typec.ty_lists[bound_func_ent.params],
            &self.typec.ty_lists[def_ent.params],
        );

        if bound_func_params.len() != impl_func_params.len() {
            return Err(SignatureError::ParamCount(
                bound_func_params.len(),
                impl_func_params.len(),
            ));
        }

        let params = None
            .into_iter()
            .chain(self.typec.params_of(base_bound))
            .chain(bound_func_params)
            .copied()
            .collect::<BumpVec<_>>();

        let params_instances = None
            .into_iter()
            .chain(self.typec.params_of(impl_ent.bound))
            .chain(impl_func_params)
            .copied()
            .collect::<BumpVec<_>>();

        self.typec.re_index_params(&params);
        self.typec.re_index_params(&params_instances);

        let cmp = |a, b| {
            self.cmp_traversal(
                a,
                b,
                |a, b| {
                    (a == base_bound && b == impl_ent.implementor)
                        || matches!(self.typec.types[a].kind, TyKind::Param { index, .. } 
                    if params_instances[index as usize] == b)
                },
                |a, b| a == b,
            )
        };

        let def_ent = &self.typec.defs[def];

        let (bound_func_args, impl_func_args) = (
            &self.typec.ty_lists[bound_func_ent.sig.args],
            &self.typec.ty_lists[def_ent.sig.args],
        );

        if bound_func_args.len() != impl_func_args.len() {
            return Err(SignatureError::ArgCount(
                bound_func_args.len(),
                impl_func_args.len(),
            ));
        }

        let mismatch = bound_func_args
            .iter()
            .zip(impl_func_args.iter())
            .enumerate()
            .find(|(.., (&a, &b))| !cmp(a, b));

        if let Some((i, (&a, &b))) = mismatch {
            return Err(SignatureError::Arg(i, a, b));
        }

        let (ret_a, ret_b) = (bound_func_ent.sig.ret.expand(), def_ent.sig.ret.expand());
        if ret_a == ret_b
            || matches!((ret_a, ret_b), (Some(ret_a), Some(ret_b)) if cmp(ret_a, ret_b))
        {
            Ok(())
        } else {
            Err(SignatureError::Ret(ret_a, ret_b))
        }
    }

    pub fn impls_overlap(&self, a: Impl, b: Impl) -> bool {
        let (a_ent, b_ent) = (&self.typec.impls[a], &self.typec.impls[b]);
        self.types_overlap(a_ent.bound, b_ent.bound)
            && self.types_overlap(a_ent.implementor, b_ent.implementor)
    }

    pub fn types_overlap(&self, a: Ty, b: Ty) -> bool {
        self.cmp_traversal(a, b, |_, _| false, |_, _| true)
    }

    pub fn cmp_traversal(
        &self,
        a: Ty,
        b: Ty,
        skip_case: impl Fn(Ty, Ty) -> bool,
        param_cmp: impl Fn(TyKind, TyKind) -> bool,
    ) -> bool {
        let mut frontier = vec![(a, b)];

        while let Some((a, b)) = frontier.pop() {
            if a == b {
                continue;
            }

            if skip_case(a, b) {
                continue;
            }

            let (a_ent, b_ent) = (&self.typec.types[a], &self.typec.types[b]);

            match (a_ent.kind, b_ent.kind) {
                (TyKind::Instance { base: base_a }, TyKind::Instance { base: base_b }) => {
                    if base_a != base_b {
                        return false;
                    }

                    frontier.extend(
                        self.typec.ty_lists[a_ent.params]
                            .iter()
                            .zip(self.typec.ty_lists[b_ent.params].iter())
                            // Associated types should not be checked for overlap otherwise there would be
                            // no difference compared to generic parameters.
                            .take(
                                self.typec.param_count(base_a)
                                    - self.typec.assoc_ty_count_of_bound(base_a),
                            )
                            .map(|(a, b)| (*a, *b)),
                    );
                }
                (
                    TyKind::Ptr {
                        base: base_a,
                        depth: depth_a,
                    },
                    TyKind::Ptr {
                        base: base_b,
                        depth: depth_b,
                    },
                ) => {
                    if depth_a != depth_b {
                        return false;
                    }

                    frontier.push((base_a, base_b));
                }
                (a @ TyKind::Param { .. }, b) | (a, b @ TyKind::Param { .. }) => {
                    if !param_cmp(a, b) {
                        return false;
                    }
                }
                (TyKind::FuncPtr { sig: sig_a }, TyKind::FuncPtr { sig: sig_b }) => {
                    if sig_a.cc != sig_b.cc {
                        return false;
                    }

                    frontier.extend(
                        self.typec.ty_lists[sig_a.args]
                            .iter()
                            .zip(self.typec.ty_lists[sig_b.args].iter())
                            .map(|(a, b)| (*a, *b)),
                    );

                    match (sig_a.ret.expand(), sig_b.ret.expand()) {
                        (Some(ret_a), Some(ret_b)) => frontier.push((ret_a, ret_b)),
                        (None, None) => (),
                        _ => return false,
                    }
                }
                (a, b) if a == b => (),
                _ => return false,
            }
        }

        true
    }
}

pub enum SignatureError {
    CallConv(Maybe<Ident>, Maybe<Ident>),
    ArgCount(usize, usize),
    ParamCount(usize, usize),
    Arg(usize, Ty, Ty),
    Ret(Option<Ty>, Option<Ty>),
}

pub enum TyInferenceError {
    InvalidExpr,
    TypeMismatch(Maybe<Ty>, Maybe<Ty>),
}
