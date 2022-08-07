use crate::*;

impl BoundChecker<'_> {
    pub fn compare_bound_signatures(&self, a: Sig, b: Sig, bound: Ty, implementor: Ty) -> bool {
        if a.cc != b.cc {
            return false;
        }

        let params_a = &self.typec.ty_lists[a.args];
        let params_b = &self.typec.ty_lists[b.args];
        if params_a.len() != params_b.len() {
            return false;
        }

        let ty_comparator = |a, b| {
            self.types_overlap_low(a, b, |a, b| a == bound && b == implementor, |a, b| a == b)
        };

        let params_not_equal = params_a
            .iter()
            .zip(params_b.iter())
            .any(|(&a, &b)| ty_comparator(a, b));

        if params_not_equal {
            return false;
        }

        if let (Some(ret_a), Some(ret_b)) = (a.ret.expand(), b.ret.expand()) && !ty_comparator(ret_a, ret_b) {
            return false;
        }

        a.ret == b.ret
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
        ty_cmp: impl Fn(Ty, Ty) -> bool,
        param_cmp: impl Fn(TyKind, TyKind) -> bool,
    ) -> bool {
        let mut frontier = vec![(a, b)];

        while let Some((a, b)) = frontier.pop() {
            if a == b {
                continue;
            }

            if ty_cmp(a, b) {
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
