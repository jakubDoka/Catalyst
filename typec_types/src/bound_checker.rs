use crate::{*, ty_factory::prepare_params};
use lexer::*;
use storage::*;

impl BoundChecker<'_> {
    pub fn implements_copy(&mut self, ty: Ty) -> bool {
        self.implements(self.builtin_types.copy, ty, false).is_ok()
    }

    pub fn drop_impl(&mut self, ty: Ty) -> errors::Result<BoundImpl> {
        self.implements(self.builtin_types.drop, ty, false)
            .map_err(|_| ())
    }

    pub fn infer_parameters(
        &mut self,
        root_reference: Ty,
        root_parametrized: Ty,
        params: &mut [Option<Ty>],
        subs: &[Ty],
        span: Span,
        check_bounds: bool,
    ) -> std::result::Result<(), Option<TyError>> {
        prepare_params(subs, self.types);

        // TODO: user preallocated vec if needed
        let mut frontier = vec![(root_reference, root_parametrized)];
    
        let error = Err(Some(TyError::GenericTypeMismatch {
            expected: root_parametrized,
            found: root_reference,
            loc: span,
        }));
    
        while let Some((reference, parametrized)) = frontier.pop() {
            let TyEnt { kind, flags, .. } = self.types[parametrized];
            
            if !flags.contains(TyFlags::GENERIC) {
                continue;
            }
    
            match (kind, self.types[reference].kind) {
                (TyKind::Param(index, ..), _) => {
                    let other = params[index as usize];
                
                    if let Some(other) = other && other != reference {
                        return error;
                    } else {
                        if check_bounds && self.implements_param(parametrized, reference, true).is_err() {
                            return Err(None);
                        }
                        params[index as usize] = Some(reference);
                    }
                }           
                (TyKind::Ptr(ty, depth), TyKind::Ptr(ref_ty, ref_depth))
                    if depth == ref_depth
                        && (
                            !self.types[parametrized].flags.contains(TyFlags::MUTABLE)
                            || self.types[reference].flags.contains(TyFlags::MUTABLE) 
                        ) 
                => {
                    frontier.push((ref_ty, ty));
                }
                (TyKind::Instance(base, params), TyKind::Instance(ref_base, ref_params))
                    if base == ref_base =>
                {
                    let params = self.ty_lists.get(params);
                    let ref_params = self.ty_lists.get(ref_params);
                    for (&ref_param, &param) in ref_params.iter().zip(params) {
                        frontier.push((ref_param, param));
                    }
                }
                (a, b) if a == b => {}
                _ => {
                    return error;
                },
            }
        }
    
        Ok(())
    }

    pub fn implements_param(
        &mut self,
        bound: Ty,
        implementor: Ty,
        build_err: bool,
    ) -> Result<(), MissingBoundTree> {
        // bound can be pain Bound or BoundCombo
        // println!("implements_param: {}", ty_display!(self, bound));
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

        return self.try_implement(id, bound_impl, bound, implementor, build_err);
    }

    fn try_implement(
        &mut self,
        id: ID,
        bound_impl: BoundImpl,
        bound: Ty,
        implementor: Ty,
        build_err: bool,
    ) -> Result<BoundImpl, MissingBoundTree> {
        let param_slice = self.ty_lists.get(bound_impl.params);
        let mut params = vec![None; param_slice.len()];

        // println!("try_implement: {} {}", ty_display!(self, implementor), ty_display!(self, bound_impl.ty));

        self.infer_parameters(
            implementor,
            bound_impl.ty,
            &mut params,
            param_slice,
            Span::default(),
            false,
        )
        // .map_err(|err| {
        //     if let Some(TyError::GenericTypeMismatch { expected, found, loc }) = err {
        //         println!("{} {} {}", ty_display!(self, expected), ty_display!(self, found), loc.log(self.sources));
        //     }
        // })
        .unwrap();

        let mut is_ok = true;
        let unimplemented: Vec<_> = params
            .iter()
            .zip(param_slice)
            .filter_map(|(&inferred, &param)| {
                if let Err(err) = self.implements_param(param, inferred.unwrap(), build_err) {
                    is_ok = false;
                    build_err.then_some(err)
                } else {
                    None
                }
            })
            .collect();
        
            
        if is_ok {
            self.bound_impls.insert(id, bound_impl);
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
