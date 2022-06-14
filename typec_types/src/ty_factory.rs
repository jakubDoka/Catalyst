use lexer::Span;
use storage::*;

use crate::*;

impl TyFactory<'_> {
    pub fn subtype(&mut self, parent: Ty, child: Ty) -> Ty {
        // better just assert and force caller to prepare this, in general
        // case this is needed anyway and llvm can optimize this when inlining
        assert_eq!(self.types.ptr_leaf_of(parent), parent);

        if let TyKind::Instance(.., params) = self.types[parent].kind {
            let params = self.vec_pool.alloc(self.ty_lists.get(params));
            return self.instantiate(child, &params);
        }

        child
    }

    pub fn instantiate(&mut self, target: Ty, params: &[Ty]) -> Ty {
        let TyEnt {
            kind, flags, name, ..
        } = self.types[target];

        // println!("instantiate: {}", ty_display!(self, target));
        // for &param in params {
        // println!(" param: {}", ty_display!(self, param));
        // }

        if !flags.contains(TyFlags::GENERIC) {
            return target;
        }

        let result = match kind {
            TyKind::Param(i, ..) => params[i as usize],
            TyKind::Ptr(ty, ..) => {
                let ty = self.instantiate(ty, params);
                pointer_of(
                    ty,
                    flags.contains(TyFlags::MUTABLE),
                    self.types,
                    self.ty_instances,
                )
            }
            TyKind::Instance(base, i_params) => {
                self.ty_lists.mark_frame();
                for param in self.vec_pool.alloc(self.ty_lists.get(i_params)).drain(..) {
                    // TODO: optimize if needed
                    let ty = self.instantiate(param, params);
                    self.ty_lists.push_one(ty);
                }

                let result = self.parse_instance_type_low(base, name);

                let TyEnt { kind, flags, .. } = self.types[result];
                if !flags.contains(TyFlags::GENERIC) {
                    let TyKind::Instance(base, params) = kind else {
                        unreachable!();
                    };

                    let kind = self.types[base].kind;
                    match kind {
                        TyKind::Struct(ty_comps) => {
                            let params = self.vec_pool.alloc(self.ty_lists.get(params));
                            for field in self.ty_comps.get(ty_comps) {
                                self.instantiate(field.ty, &params);
                            }
                        }
                        kind => unimplemented!("{kind:?}"),
                    }
                }

                result
            }
            _ => todo!(),
        };

        result
    }

    /// further specification of [`parse_type`], it expects the `ty` to be of [`ast::Kind::Instantiation`], if instance already exists, it is reused.
    pub fn parse_instance_type_low(&mut self, header: Ty, span: Span) -> Ty {
        let mut id = ID::new("<instance>") + self.types[header].id;
        let mut generic = false;

        for &param in self.ty_lists.top() {
            id = id + self.types[param].id;
            generic |= self.types[param].flags.contains(TyFlags::GENERIC);
        }

        if let Some(&already) = self.ty_instances.get(id) {
            self.ty_lists.discard();
            return already;
        }

        let params = self.ty_lists.pop_frame();

        let result = {
            let ent = TyEnt {
                id,
                name: span,
                kind: TyKind::Instance(header, params),
                flags: TyFlags::GENERIC & generic,
            };
            self.types.push(ent)
        };

        self.ty_instances.insert_unique(id, result);

        result
    }

    pub fn parse_composite_bound_low(&mut self, span: Span) -> Ty {
        self.ty_lists.top_mut().sort_by_key(|ty| ty.0);

        let base_id = self.types[self.builtin_types.any].id;

        let id = self
            .ty_lists
            .top()
            .iter()
            .map(|&ty| self.types[ty].id)
            .fold(base_id, |acc, id| acc + id);

        if let Some(&already) = self.ty_instances.get(id) {
            self.ty_lists.discard();
            return already;
        }

        // make bound combo implement all contained bounds
        for &ty in self.ty_lists.top() {
            let TyKind::Bound(funcs) = self.types[ty].kind else {
                unreachable!();
            };
            let bound = self.types[ty].id;
            let id = ID::bound_impl(bound, id);
            let bound = BoundImpl {
                funcs,
                ..Default::default()
            };
            self.bound_impls.insert_unique(id, bound);
        }

        let combo = {
            let bounds = self.ty_lists.pop_frame();
            let ent = TyEnt {
                id,
                name: span,
                kind: TyKind::Param(0, bounds, None.into()),
                flags: TyFlags::GENERIC,
            };
            self.types.push(ent)
        };

        self.ty_instances.insert_unique(id, combo);

        combo
    }

    pub fn func_pointer_of(&mut self, sig: Sig, generic: bool) -> Ty {
        let id = self.id_of_sig(sig);

        if let Some(&already) = self.ty_instances.get(id) {
            return already;
        }

        if generic {
            panic!("generic function pointer");
        }

        let ty_ent = TyEnt {
            id,
            name: Span::new(self.types[sig.ret].name.source(), 0, 0),
            kind: TyKind::FuncPtr(sig),
            flags: (TyFlags::GENERIC & generic),
        };
        let ty = self.types.push(ty_ent);
        self.ty_instances.insert_unique(id, ty);

        ty
    }

    pub fn id_of_sig(&self, sig: Sig) -> ID {
        let mut id = ID::new("<func_pointer>");

        id = id + ID::new("<args>");

        for &arg in self.ty_lists.get(sig.args) {
            let arg = self.types[arg].id;
            id = id + arg;
        }

        id = id + ID::new("<ret>");

        let ret = self.types[sig.ret].id;
        id + ret
    }
}

pub fn pointer_of(ty: Ty, mutable: bool, types: &mut Types, ty_instances: &mut TyInstances) -> Ty {
    let TyEnt {
        kind,
        id,
        name,
        flags,
        ..
    } = types[ty];
    let id = ID::pointer(id, mutable);

    if let Some(&already) = ty_instances.get(id) {
        return already;
    }

    let depth = if let TyKind::Ptr(.., depth) = kind {
        depth
    } else {
        0
    };

    let ent = TyEnt {
        id,
        name,
        kind: TyKind::Ptr(ty, depth + 1),
        flags: flags & !TyFlags::BUILTIN | TyFlags::MUTABLE & mutable,
    };
    let ptr = types.push(ent);

    assert!(ty_instances.insert(id, ptr).is_none());

    ptr
}

pub fn infer_parameters(
    root_reference: Ty,
    root_parametrized: Ty,
    params: &mut [Ty],
    span: Span,
    types: &Types,
    ty_lists: &TyLists,
) -> std::result::Result<(), TyError> {
    // TODO: user preallocated vec if needed
    let mut frontier = vec![(root_reference, root_parametrized)];

    let error = Err(TyError::GenericTypeMismatch {
        expected: root_parametrized,
        found: root_reference,
        loc: span,
    });

    while let Some((reference, parametrized)) = frontier.pop() {
        let TyEnt { kind, flags, .. } = types[parametrized];
        if !flags.contains(TyFlags::GENERIC) {
            continue;
        }

        match (kind, types[reference].kind) {
            (TyKind::Param(index, ..), _) => {
                // we use `base_of_low` because we want to compare parameter
                // backing bounds correctly
                let other = params[index as usize];

                if !other.is_reserved_value() && other != reference {
                    return error;
                } else {
                    params[index as usize] = reference;
                }
            }
            (TyKind::Ptr(ty, depth), TyKind::Ptr(ref_ty, ref_depth))
                if depth == ref_depth
                    && types[parametrized].flags.contains(TyFlags::MUTABLE)
                        == types[reference].flags.contains(TyFlags::MUTABLE) =>
            {
                frontier.push((ref_ty, ty));
            }
            (TyKind::Instance(base, params), TyKind::Instance(ref_base, ref_params))
                if base == ref_base =>
            {
                let params = ty_lists.get(params);
                let ref_params = ty_lists.get(ref_params);
                for (&ref_param, &param) in ref_params.iter().zip(params) {
                    frontier.push((ref_param, param));
                }
            }
            (a, b) if a == b => {}
            _ => return error,
        }
    }

    Ok(())
}

pub fn prepare_params(params: &[Ty], types: &mut Types) {
    for (i, &param) in params.iter().enumerate() {
        let TyKind::Param(index, ..) = &mut types[param].kind else {
            unreachable!("{:?}", types[param].kind);
        };
        *index = i as u8;
    }
}
