use lexer::Span;
use storage::*;

use crate::*;

impl TyFactory<'_> {
    pub fn subtype(&mut self, parent: Ty, child: Ty, new_instances: &mut Vec<Ty>) -> Ty {
        // better just assert and force caller to prepare this, in general
        // case this is needed anyway and llvm can optimize this when inlining
        assert_eq!(self.types.ptr_leaf_of(parent), parent);

        if let TyKind::Instance(base, params) = self.types[parent].kind {
            let params = self.vec_pool.alloc(self.ty_lists.get(params));
            let parent_params = collect_ty_params(base, self.types, self.vec_pool, self.builtin_types);
            let (ty, new) = self.instantiate_low(child, &params, parent_params.as_slice(), new_instances);
            if new {
                self.ty_graph.add_edge(parent, ty);
            }
            return ty;
        }

        child
    }

    pub fn instantiate(&mut self, target: Ty, params: &[Ty], subs: &[Ty], new_instances: &mut Vec<Ty>) -> Ty {
        self.instantiate_low(target, params, subs, new_instances).0
    }

    pub fn instantiate_low(&mut self, target: Ty, params: &[Ty], subs: &[Ty], new_instances: &mut Vec<Ty>) -> (Ty, bool) {
        let TyEnt {
            kind, flags, name, ..
        } = self.types[target];

        // println!("instantiate: {}", ty_display!(self, target));
        // for &param in params {
        // println!(" param: {}", ty_display!(self, param));
        // }

        if let Some(index) = subs.iter().position(|&x| x == target) {
            return (params[index], false);
        }

        if !flags.contains(TyFlags::GENERIC) {
            return (target, false);
        }

        match kind {
            TyKind::Ptr(ty, ..) => {
                let (ty, _) = self.instantiate_low(ty, params, subs, new_instances);
                let (ptr, new) = self.pointer_of_low(
                    ty,
                    flags.contains(TyFlags::MUTABLE),
                );

                if new {
                    new_instances.push(ptr);
                }

                (ptr, new)
            }
            TyKind::Instance(base, i_params) => {
                self.ty_lists.mark_frame();
                for param in self.vec_pool.alloc(self.ty_lists.get(i_params)).drain(..) {
                    // TODO: optimize if needed
                    let (ty, _) = self.instantiate_low(param, params, subs, new_instances);
                    self.ty_lists.push_one(ty);
                }

                let (result, new) = self.parse_instance_type_low(base, name);
                
                if new {
                    new_instances.push(result);
                }

                let TyEnt { kind, flags, .. } = self.types[result];
                if !flags.contains(TyFlags::GENERIC) {
                    let TyKind::Instance(base, params) = kind else {
                        unreachable!();
                    };

                    let kind = self.types[base].kind;
                    match kind {
                        TyKind::Struct(ty_comps) => {
                            let params = self.vec_pool.alloc(self.ty_lists.get(params));
                            let subs = collect_ty_params(base, self.types, self.vec_pool, self.builtin_types);
                            for field in self.ty_comps.get(ty_comps) {
                                let (ty, new) = self.instantiate_low(field.ty, &params, subs.as_slice(), new_instances);
                                if new {
                                    self.ty_graph.add_edge(result, ty);
                                }
                            }
                        }
                        kind => unimplemented!("{kind:?}"),
                    }
                }

                (result, new)
            }
            TyKind::Struct(ty_comps) => {
                self.ty_lists.mark_frame();
                for &param in params {
                    self.ty_lists.push_one(param);
                }
                let (result, new) = self.parse_instance_type_low(target, name);
                
                if new {
                    new_instances.push(result);
                }

                let subs = collect_ty_params(target, self.types, self.vec_pool, self.builtin_types);
                for field in self.ty_comps.get(ty_comps) {
                    let (ty, new) = self.instantiate_low(field.ty, &params, subs.as_slice(), new_instances);
                    if new {
                        self.ty_graph.add_edge(result, ty);
                    }
                }

                (result, new)
            }
            kind => unimplemented!("{:?}", kind),
        }
    }

    pub fn parse_instance_type(&mut self, header: Ty, span: Span) -> Ty {
        self.parse_instance_type_low(header, span).0
    }

    /// further specification of [`parse_type`], it expects the `ty` to be of [`ast::Kind::Instantiation`], 
    /// if instance already exists, it is reused.
    pub fn parse_instance_type_low(&mut self, header: Ty, span: Span) -> (Ty, bool) {
        let mut id = ID::new("<instance>") + self.types[header].id;
        let mut generic = false;

        for &param in self.ty_lists.top() {
            id = id + self.types[param].id;
            generic |= self.types[param].flags.contains(TyFlags::GENERIC);
        }

        if let Some(&already) = self.ty_instances.get(id) {
            self.ty_lists.discard();
            return (already, false);
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
        self.ty_graph.add_vertex(result);

        (result, true)
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
            let bound = self.types[ty].id;
            let id = ID::bound_impl(bound, id);
            let bound = BoundImpl {
                ty,
                ..Default::default()
            };
            self.bound_impls.insert_unique(id, bound);
        }

        let combo = {
            let bounds = self.ty_lists.pop_frame();
            let ent = TyEnt {
                id,
                name: span,
                kind: TyKind::Param(bounds, None.into()),
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
            todo!("generic function pointer");
        }

        let ty_ent = TyEnt {
            id,
            name: Span::new(self.types[sig.ret].name.source(), 0, 0),
            kind: TyKind::FuncPtr(sig),
            flags: (TyFlags::GENERIC & generic),
        };
        let ty = self.types.push(ty_ent);
        self.ty_instances.insert_unique(id, ty);

        for &arg in self.ty_comps.get(sig.args) {
            self.ty_graph.add_edge(ty, arg.ty);
        }

        self.ty_graph.add_edge(ty, sig.ret);

        ty
    }

    pub fn id_of_sig(&self, sig: Sig) -> ID {
        let mut id = ID::new("<func_pointer>");

        id = id + ID::new("<args>");

        for &arg in self.ty_comps.get(sig.args) {
            let arg = self.types[arg.ty].id;
            id = id + arg;
        }

        id = id + ID::new("<ret>");

        let ret = self.types[sig.ret].id;
        id + ret
    }

    pub fn pointer_of(&mut self, ty: Ty, mutable: bool) -> Ty {
        self.pointer_of_low(ty, mutable).0
    }

    pub fn pointer_of_low(&mut self, ty: Ty, mutable: bool) -> (Ty, bool) {
        let TyEnt {
            kind,
            id,
            name,
            flags,
            ..
        } = self.types[ty];
        let id = ID::pointer(id, mutable);
    
        if let Some(&already) = self.ty_instances.get(id) {
            return (already, false);
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
        let ptr = self.types.push(ent);
    
        assert!(self.ty_instances.insert(id, ptr).is_none());
        self.ty_graph.add_vertex(ptr);

        (ptr, true)
    }
}

pub fn collect_ty_params(ty: Ty, types: &Types, vec_pool: &VecPool, builtin_types: &BuiltinTypes) -> PoolVec<Ty> {
    let max = types[ty].flags.param_count();
    let mut params = vec_pool.with_capacity(max);
    let mut current = Some(builtin_types.ty_any);
    let mut i = 0;
    while i < max && let Some(next) = current {
        params.push(next);
        let TyKind::Param(.., next) = types[next].kind else {
            unreachable!();
        };
        current = next.expand();
        i += 1;
    }
    params
}