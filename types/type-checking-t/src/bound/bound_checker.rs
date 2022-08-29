use std::default::default;

use lexing_t::Span;
use storage::*;

use crate::*;

use super::BoundBase;

impl BoundChecker<'_> {
    pub fn implements(&mut self, _ty: VRef<Ty>, _bound: VRef<Bound>) -> bool {
        todo!()
    }

    pub fn bound_implements(&mut self, impl_bound: VRef<Bound>, bound: VRef<Bound>) -> bool {
        if bound == Bound::ANY {
            return true;
        }

        let impl_bounds = &[impl_bound];
        let implemented = self.typec.unwrap_anon(impl_bounds);
        let bounds = &[bound];
        let to_implement = self.typec.unwrap_anon(bounds);

        let mut missing = to_implement.to_bumpvec();
        missing.retain(|&bound| {
            implemented.iter().any(|&impl_bound| {
                bound == impl_bound
                    || self.typec.computed_impls.contains_key(&IdentPair(
                        self.typec.bounds.id(bound),
                        self.typec.bounds.id(impl_bound),
                    ))
            })
        });
        missing.is_empty()
    }

    pub fn infer_type(
        &mut self,
        _reference: VRef<Ty>,
        _template: VRef<Ty>,
        _infer_slots: &mut [VRef<Ty>],
        _bounds: &[VRef<Bound>],
    ) -> Result<(), TyInferenceError> {
        todo!()
    }

    pub fn compare_bound_signatures(
        &self,
        r#impl: VRef<Impl>,
        bound_func_index: usize,
        def: VRef<Def>,
    ) -> Result<(), SignatureError> {
        let impl_ent = self.typec.impls[r#impl];
        let def_ent = &self.typec.defs[def];
        let base_bound = self.typec.bound_base(impl_ent.bound);
        let bound_base = self.typec.bounds[base_bound]
            .kind
            .downcast::<BoundBase>()
            .unwrap();
        let bound_func_ent = self.typec.bound_funcs[bound_base.funcs][bound_func_index];

        let (bound_func_params, impl_func_params) = (
            &self.typec.bound_lists[bound_func_ent.params],
            &self.typec.bound_lists[def_ent.generics],
        );

        if bound_func_params.len() != impl_func_params.len() {
            return Err(SignatureError::ParamCount(
                bound_func_params.len(),
                impl_func_params.len(),
            ));
        }

        let mismatch = bound_func_params
            .iter()
            .zip(impl_func_params.iter())
            .enumerate()
            .find(|(_, (bound_param, impl_param))| bound_param != impl_param);

        if let Some((pos, (&bound_param, &impl_param))) = mismatch {
            return Err(SignatureError::Param(pos, bound_param, impl_param));
        }

        let bound_instance = self.typec.bounds[impl_ent.bound]
            .kind
            .downcast::<BoundInstance>()
            .unwrap();

        let params_instances = None
            .into_iter()
            .chain(&self.typec.ty_lists[bound_instance.params])
            .chain(&self.typec.ty_lists[bound_instance.assoc_types])
            .copied()
            .collect::<BumpVec<_>>();

        let cmp = |a, b| {
            self.type_comparison(
                a,
                b,
                |a, b| match self.typec.types[a].kind {
                    TyKind::Param(index) => params_instances
                        .get(index as usize)
                        .map_or(true, |&a| a == b),
                    TyKind::SelfBound => b == impl_ent.implementor,
                    _ => false,
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

    pub fn impls_overlap(&self, a: VRef<Impl>, b: VRef<Impl>) -> bool {
        let (a_ent, b_ent) = (&self.typec.impls[a], &self.typec.impls[b]);
        self.bounds_overlap(a_ent.bound, b_ent.bound)
            && self.types_overlap(a_ent.implementor, b_ent.implementor)
    }

    pub fn bounds_overlap(&self, a: VRef<Bound>, b: VRef<Bound>) -> bool {
        let (a_ent, b_ent) = (&self.typec.bounds[a], &self.typec.bounds[b]);
        match (a_ent.kind, b_ent.kind) {
            (BoundKind::Instance(inst_a), BoundKind::Instance(inst_b)) => {
                assert!(inst_a.base == inst_b.base);

                let (a_params, b_params) = (
                    &self.typec.ty_lists[inst_a.params],
                    &self.typec.ty_lists[inst_b.params],
                );

                a_params
                    .iter()
                    .zip(b_params)
                    .all(|(&a, &b)| self.types_overlap(a, b))
            }
            _ => unreachable!(),
        }
    }

    pub fn types_overlap(&self, a: VRef<Ty>, b: VRef<Ty>) -> bool {
        self.type_comparison(a, b, |_, _| false, |_, _| true)
    }

    pub fn type_comparison(
        &self,
        a: VRef<Ty>,
        b: VRef<Ty>,
        skip_case: impl Fn(VRef<Ty>, VRef<Ty>) -> bool,
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
                (TyKind::Instance(inst_a), TyKind::Instance(inst_b)) => {
                    if inst_a.base != inst_b.base {
                        return false;
                    }

                    frontier.extend(
                        self.typec.ty_lists[inst_a.params]
                            .iter()
                            .copied()
                            .zip(self.typec.ty_lists[inst_b.params].iter().copied()),
                    );
                }
                (TyKind::Ptr(ptr_a), TyKind::Ptr(ptr_b)) => {
                    if ptr_a.depth != ptr_b.depth {
                        return false;
                    }

                    frontier.push((ptr_a.base, ptr_b.base));
                }
                (a @ TyKind::Param { .. }, b) | (a, b @ TyKind::Param { .. }) => {
                    if !param_cmp(a, b) {
                        return false;
                    }
                }
                (TyKind::FuncPtr(sig_a), TyKind::FuncPtr(sig_b)) => {
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

    pub fn anon_bound_of(&mut self, bounds: &[VRef<Bound>]) -> VRef<Bound> {
        let id = self.anon_bound_id(bounds);
        let bound = self.bound_of(id, bounds, &[], &[], &[]);
        self.typec.bounds[bound].flags |= BoundFlags::ANON;
        bound
    }

    pub fn bound_of(
        &mut self,
        id: Ident,
        inherits: &[VRef<Bound>],
        generics: &[VRef<Bound>],
        assoc_types: &[VRef<Bound>],
        funcs: &[BoundFunc],
    ) -> VRef<Bound> {
        if let Some(already) = self.typec.bounds.index(id) {
            return already;
        }

        let ent = Bound {
            kind: BoundBase {
                inherits: self.typec.bound_lists.bump_slice(inherits),
                assoc_types: self.typec.bound_lists.bump_slice(assoc_types),
                generics: self.typec.bound_lists.bump_slice(generics),
                funcs: self.typec.bound_funcs.bump_slice(funcs),
            }
            .into(),
            flags: BoundFlags::GENERIC & (!generics.is_empty() || !assoc_types.is_empty()),
            ..default()
        };
        self.typec.bounds.insert_unique(id, ent)
    }

    // pub fn bound_instance_of(&mut self, base: VRef<Bound>, params: &[VRef<Ty>], assoc_types: &[VRef<Ty>]) -> VRef<Bound> {
    //     let id = self.bound_id(base, params, assoc_types);

    // }

    // pub fn bound_id(&self, base: VRef<Bound>, params: &[VRef<Ty>], assoc_types: &[VRef<Ty>]) -> Ident {
    //     let segments = ident!(self.typec.bounds.id(base), "[")
    //         .iter().copied()
    //         .chain();
    // }

    pub fn anon_bound_id(&mut self, bounds: &[VRef<Bound>]) -> Ident {
        let Some((&first, others)) = bounds.split_first() else {
            unreachable!()
        };

        let mapper = |&segment| ident!(" + ", self.typec.bounds.id(segment));
        let others = others.iter().flat_map(mapper);
        let iter = ident!(self.typec.bounds.id(first))
            .into_iter()
            .chain(others);
        self.interner.intern(iter)
    }

    pub fn signature_error_to_diag(
        &self,
        err: SignatureError,
        span: Span,
        current_file: Ident,
        r#impl: VRef<Impl>,
    ) -> diags::Diag {
        let loc = self.typec.loc_of(r#impl, self.interner);

        macro_rules! specific_diag {
            ($($tt:tt)*) => {
                diags::diag!(
                    (span, current_file) => $($tt)*,
                    (exp loc) => "related bound defined here",
                )
            };
        }

        match err {
            SignatureError::CallConv(expected, got) => specific_diag!(
                "call convention mismatch, expected {} but got {}" {
                    expected.expand().map_or("\"default\"", |cc| &self.interner[cc]),
                    got.expand().map_or("\"default\"", |cc| &self.interner[cc]),
                }
            ),
            SignatureError::ArgCount(expected, got) => specific_diag!(
                "argument count mismatch, expected {} but got {}" {
                    expected,
                    got,
                }
            ),
            SignatureError::Arg(pos, expected, got) => specific_diag!(
                "argument {} mismatch, expected {} but got {}" {
                    pos + 1,
                    &self.interner[self.typec.types.id(expected)],
                    &self.interner[self.typec.types.id(got)],
                }
            ),
            SignatureError::Ret(expected, got) => specific_diag!(
                "return type mismatch, expected {} but got {}" {
                    expected.map_or("nothing", |expected| &self.interner[self.typec.types.id(expected)]),
                    got.map_or("nothing", |got| &self.interner[self.typec.types.id(got)]),
                }
            ),
            SignatureError::ParamCount(expected, got) => specific_diag!(
                "function parameter count mismatch, expected {} but got {}" {
                    expected,
                    got,
                }
            ),
            SignatureError::Param(index, expected, got) => specific_diag!(
                "function parameter {} mismatch, expected {} but got {}" {
                    index + 1,
                    &self.interner[self.typec.bounds.id(expected)],
                    &self.interner[self.typec.bounds.id(got)],
                }
            ),
        }
    }
}

pub enum SignatureError {
    CallConv(Maybe<Ident>, Maybe<Ident>),
    ArgCount(usize, usize),
    ParamCount(usize, usize),
    Param(usize, VRef<Bound>, VRef<Bound>),
    Arg(usize, VRef<Ty>, VRef<Ty>),
    Ret(Option<VRef<Ty>>, Option<VRef<Ty>>),
}

impl SignatureError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TyInferenceError {
    InvalidExpr,
    Mismatch(Maybe<VRef<Ty>>, Maybe<VRef<Ty>>),
    NotCompatible(VRef<Ty>, VRef<Ty>),
}
