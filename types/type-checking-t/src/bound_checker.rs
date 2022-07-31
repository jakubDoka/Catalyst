use crate::*;

impl BoundChecker<'_> {
    pub fn impls_overlap(&self, a: Impl, b: Impl) -> bool {
        let (a_ent, b_ent) = (&self.types.impls[a], &self.types.impls[b]);
        self.types_overlap(a_ent.bound, b_ent.bound)
            && self.types_overlap(a_ent.implementor, b_ent.implementor)
    }

    pub fn types_overlap(&self, a: Ty, b: Ty) -> bool {
        let mut frontier = vec![(a, b)];

        while let Some((a, b)) = frontier.pop() {
            if a == b {
                continue;
            }

            let (a_ent, b_ent) = (&self.types.ents[a], &self.types.ents[b]);

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
                        self.types
                            .slices
                            .get(params_a)
                            .iter()
                            .zip(self.types.slices.get(params_b).iter())
                            // Associated types should not be checked for overlap otherwise there would be
                            // no difference compared to generic parameters.
                            .take(
                                self.types.param_count(base_a) - self.types.assoc_ty_count(base_a),
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
                (TyKind::Param { .. }, _) | (_, TyKind::Param { .. }) => continue,
                (TyKind::FuncPtr { sig: sig_a }, TyKind::FuncPtr { sig: sig_b }) => {
                    if sig_a.cc != sig_b.cc {
                        return false;
                    }

                    frontier.extend(
                        self.types.slices[sig_a.args]
                            .iter()
                            .zip(self.types.slices[sig_b.args].iter())
                            .map(|(a, b)| (*a, *b)),
                    );

                    match (sig_a.ret.expand(), sig_b.ret.expand()) {
                        (Some(ret_a), Some(ret_b)) => frontier.push((ret_a, ret_b)),
                        (None, None) => (),
                        _ => return false,
                    }
                }
                (a, b) if a == b => continue,
                _ => return false,
            }
        }

        true
    }
}
