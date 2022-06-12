use crate::*;
use lexer::*;
use storage::*;


impl BoundChecker<'_> {
    pub fn implements_copy(&mut self, ty: Ty) -> bool {
        self.implements(self.builtin_types.copy, ty, false).is_ok()
    }

    pub fn drop_impl(&mut self, ty: Ty) -> errors::Result<BoundImpl> {
        self.implements(self.builtin_types.drop, ty, false).map_err(|_| ())
    }

    pub fn implements_param(&mut self, bound: Ty, implementor: Ty, build_err: bool) -> Result<(), MissingBoundTree> {
        // bound can be pain Bound or BoundCombo
        let TyKind::Param(_, bounds, ..) = self.types[bound].kind else {
            unreachable!();
        };

        let mut is_ok = true;
        let unimplemented: Vec<_> = self
            .ty_lists
            .get(bounds)
            .iter()
            .copied()
            .filter_map(|bound| {
                if let Err(err) = self.implements(bound, implementor, build_err) {
                    is_ok = false;
                    build_err.then_some(err)
                } else {
                    None
                }
            })
            .collect();

        if is_ok {
            Ok(())
        } else {
            Err(MissingBoundTree {
                bound,
                implementor,
                unimplemented,
            })
        }
    }

    pub fn implements(
        &mut self,
        bound: Ty,
        implementor: Ty,
        build_err: bool,
    ) -> Result<BoundImpl, MissingBoundTree> {
        let bound_id = self.types[bound].id;
        let id = ID::bound_impl(bound_id, self.types[implementor].id);

        let default_error = MissingBoundTree {
            bound,
            implementor,
            unimplemented: Vec::new(),
        };

        if let Some(&bound_impl) = self.bound_impls.get(id) {
            if !bound_impl.is_reserved_value() {
                return Ok(bound_impl);
            } else {
                return Err(default_error);
            }
        }

        let base_implementor = self.types.base_of(implementor);

        // this way we don't `try_implement` each time on non generic type
        if implementor == base_implementor {
            return Err(default_error);
        }

        let id = ID::bound_impl(bound_id, self.types[base_implementor].id);

        let Some(&bound_impl) = self.bound_impls.get(id) else {
            return Err(default_error);
        };

        return self.try_implement(bound_impl, bound, implementor, build_err);
    }

    fn try_implement(
        &mut self,
        bound_impl: BoundImpl,
        bound: Ty,
        implementor: Ty,
        build_err: bool,
    ) -> Result<BoundImpl, MissingBoundTree> {
        let param_slice = self.ty_lists.get(bound_impl.params);
        let mut params = vec![Ty::reserved_value(); param_slice.len()];

        prepare_params(param_slice, self.types);

        infer_parameters(
            implementor,
            bound_impl.ty,
            &mut params,
            Span::default(),
            self.types,
            self.ty_lists,
        )
        .unwrap();

        let mut is_ok = true;
        let unimplemented: Vec<_> = params
            .iter()
            .zip(param_slice)
            .filter_map(|(&inferred, &param)| {
                if let Err(err) = self.implements_param(param, inferred, build_err) {
                    is_ok = false;
                    build_err.then_some(err)
                } else {
                    None
                }
            })
            .collect();

        if is_ok {
            return Ok(bound_impl);
        } else {
            return Err(MissingBoundTree {
                bound,
                implementor,
                unimplemented,
            });
        }
    }
}
