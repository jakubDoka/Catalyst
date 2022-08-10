use storage::*;

use crate::*;

impl BoundChecker<'_> {
    pub fn compare_bound_signatures(
        &self,
        bound_func_index: usize,
        implementor_params: Maybe<TyList>,
        implementor_sig: Sig,
        r#impl: Impl,
    ) -> Result<(), SignatureError> {
        let bound_sig = self.typec.func_of_impl(r#impl, bound_func_index).sig;

        if bound_sig.cc != implementor_sig.cc {
            return Err(SignatureError::CallConv(bound_sig.cc, implementor_sig.cc));
        }

        let args_a = &self.typec.ty_lists[bound_sig.args];
        let args_b = &self.typec.ty_lists[implementor_sig.args];
        if args_a.len() != args_b.len() {
            return Err(SignatureError::ArgCount(args_a.len(), args_b.len()));
        }

        let ImplEnt {
            bound, implementor, ..
        } = self.typec.impls[r#impl];
        let base_bound = self.typec.instance_base_of(bound);
        let assoc_ty_count = self.typec.assoc_ty_count_of_bound(base_bound);
        let param_slice = &self.typec.ty_lists[self.typec.instance_params(bound)];
        let implementor_param_slice = &self.typec.ty_lists[implementor_params];

        let ty_comparator = |a, b| {
            self.types_overlap_low(
                a,
                b,
                |a, b| {
                    (a == bound && b == implementor)
                        || matches!(self.typec.types[a].kind, TyKind::AssocType { index, .. }
                    if param_slice[index as usize + param_slice.len() - assoc_ty_count] == b)
                        || matches!(self.typec.types[a].kind, TyKind::Param { index, .. }
                    if (self.typec.types[a].flags.contains(TyFlags::TY_PARAM) &&
                        param_slice[index as usize] == b)
                    || (!self.typec.types[a].flags.contains(TyFlags::TY_PARAM) &&
                        implementor_param_slice.get(index as usize).map_or(false, |&a| a == b)))
                },
                |a, b| a == b,
            )
        };

        let params_not_equal = args_a
            .iter()
            .zip(args_b.iter())
            .enumerate()
            .find(|(.., (&a, &b))| !ty_comparator(a, b));

        if let Some((i, (&a, &b))) = params_not_equal {
            return Err(SignatureError::Arg(i, a, b));
        }

        if matches!((bound_sig.ret.expand(), implementor_sig.ret.expand()), (Some(ret_a), Some(ret_b)) if !ty_comparator(ret_a, ret_b))
            || bound_sig.ret != implementor_sig.ret
        {
            return Err(SignatureError::Ret(bound_sig.ret, implementor_sig.ret));
        }

        Ok(())
    }

    pub fn impls_overlap(&self, a: Impl, b: Impl) -> bool {
        let (a_ent, b_ent) = (&self.typec.impls[a], &self.typec.impls[b]);
        self.types_overlap(a_ent.bound, b_ent.bound)
            && self.types_overlap(a_ent.implementor, b_ent.implementor)
    }

    pub fn types_overlap(&self, a: Ty, b: Ty) -> bool {
        self.types_overlap_low(a, b, |_, _| false, |_, _| true)
    }

    pub fn types_overlap_low(
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
                (
                    TyKind::Instance {
                        base: base_a,
                        params: params_a,
                    },
                    TyKind::Instance {
                        base: base_b,
                        params: params_b,
                    },
                ) => {
                    if base_a != base_b {
                        return false;
                    }

                    frontier.extend(
                        self.typec
                            .ty_lists
                            .get(params_a)
                            .iter()
                            .zip(self.typec.ty_lists.get(params_b).iter())
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
    Arg(usize, Ty, Ty),
    Ret(Maybe<Ty>, Maybe<Ty>),
}
