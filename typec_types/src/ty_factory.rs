use lexer::Span;
use storage::*;

use crate::*;

impl TyFactory<'_> {
    pub fn subtype(&mut self, parent: Ty, child: Ty) -> Ty {
        // better just assert and force caller to prepare this, in general
        // case this is needed anyway and llvm can optimize this when inlining
        assert_eq!(self.types.ptr_leaf_of(parent), parent);

        if let TyKind::Instance(base, params) = self.types[parent].kind {
            let params = self.vec_pool.alloc(self.ty_lists.get(params));
            let parent_params = collect_ty_params(base, self.types, self.vec_pool, self.builtin_types);
            let (ty, new) = self.instantiate_low(child, &params, parent_params.as_slice());
            if new {
                self.ty_graph.add_edge(parent, ty);
            }
            return ty;
        }

        child
    }

    pub fn make_param_unique(&mut self, ty: Ty) -> Ty {
        let ent = self.types[ty];
        let id = ID::unique(ent.id);
        if let Some(&already) = self.ty_instances.get(id) {
            return already;
        }
        let kind = match ent.kind {
            TyKind::Instance(base, params) => {
                let mut params = self.vec_pool.alloc(self.ty_lists.get(params));
                let new_params = self.vec_pool.alloc_iter(params
                    .drain(..).map(|ty| self.make_param_unique(ty)));
                let new_params = self.ty_lists.push(new_params.as_slice());
                TyKind::Instance(base, new_params)
            },
            _ => ent.kind,
        };
        let ty = self.types.push(TyEnt {id, kind, ..ent});
        self.ty_instances.insert(id, ty);
        ty
    }

    pub fn instantiate(&mut self, target: Ty, params: &[Ty], subs: &[Ty]) -> Ty {
        self.instantiate_low(target, params, subs).0
    }

    pub fn instantiate_low(&mut self, target: Ty, params: &[Ty], subs: &[Ty]) -> (Ty, bool) {
        let mut new_instances = self.vec_pool.get();
        self.instantiate_recur(target, params, subs, &mut new_instances)
    }

    pub fn instantiate_recur(&mut self, target: Ty, params: &[Ty], subs: &[Ty], new_instances: &mut Vec<Ty>) -> (Ty, bool) {
        let TyEnt {
            kind, flags, name, ..
        } = self.types[target];

        // println!("instantiate: {}", ty_display!(self, target));
        // for &param in params {
        // println!(" param: {}", ty_display!(self, param));
        // }

        if !flags.contains(TyFlags::GENERIC) {
            // println!("skip: {}", ty_display!(self, target));
            return (target, false);
        }

        let (ty, new) = match kind {
            TyKind::Param(index, ..) => return (params[index as usize], true),
            TyKind::Ptr(ty, ..) => {
                // println!("ptr: {}", ty_display!(self, ty));
                let (ty, _) = self.instantiate_recur(ty, params, subs, new_instances);
                self.pointer_of_low(ty, flags.contains(TyFlags::MUTABLE))
            }
            TyKind::Instance(base, i_params) => {
                self.ty_lists.mark_frame();
                for param in self.vec_pool.alloc(self.ty_lists.get(i_params)).drain(..) {
                    // println!("param: {} on base: {}", ty_display!(self, param), ty_display!(self, base));
                    let (ty, _) = self.instantiate_recur(param, params, subs, new_instances);
                    self.ty_lists.push_one(ty);
                }
                self.parse_instance_type_low(base, name, new_instances)
            }
            TyKind::Struct(..) => {
                self.ty_lists.mark_frame();
                for &param in params {
                    self.ty_lists.push_one(param);
                }
                self.parse_instance_type_low(target, name, new_instances)
            }
            kind => unimplemented!("{:?}", kind),
        };

        if new {
            new_instances.push(ty);
        }

        // println!("expand: {}", ty_display!(self, ty));
        (ty, new)
    }

    pub fn parse_instance_type(&mut self, header: Ty, span: Span, new_instances: &mut Vec<Ty>) -> Ty {
        self.parse_instance_type_low(header, span, new_instances).0
    }

    /// further specification of [`parse_type`], it expects the `ty` to be of [`ast::Kind::Instantiation`], 
    /// if instance already exists, it is reused.
    pub fn parse_instance_type_low(&mut self, header: Ty, span: Span, new_instances: &mut Vec<Ty>) -> (Ty, bool) {
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

        if !generic {
            match self.types[header].kind {
                TyKind::Struct(ty_comps) => {
                    let params = self.vec_pool.alloc(self.ty_lists.get(params));
                    let subs = collect_ty_params(header, self.types, self.vec_pool, self.builtin_types);
                    for field in self.ty_comps.get(ty_comps) {
                        let (ty, new) = self.instantiate_recur(field.ty, params.as_slice(), subs.as_slice(), new_instances);
                        if new {
                            self.ty_graph.add_edge(result, ty);
                        }
                    }
                }
                kind => unimplemented!("{kind:?}"),
            }

            self.ty_graph.add_vertex(result);
        }
        
        self.ty_instances.insert_unique(id, result);
        new_instances.push(result);

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

        if !flags.contains(TyFlags::GENERIC) {
            self.ty_graph.add_vertex(ptr);
        }

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

pub fn prepare_params(params: &[Ty], types: &mut Types) {
    for (i, &param) in params.iter().enumerate() {
        let TyKind::Param(index, ..) = &mut types[param].kind else {
            unreachable!("{:?}", types[param].kind);
        };
        *index = i as u32;
    }
}