use crate::*;
use std::{default::default, fmt::Write, iter};
use storage::*;
use types::*;

pub mod display;
pub mod spec_solving;

impl<'ctx, 'arena> TypeCreator<'ctx, 'arena> {
    pub fn find_struct_field(
        &mut self,
        struct_id: FragRef<Struct>,
        params: &[Ty],
        field_name: Ident,
    ) -> Option<(usize, FragRef<Field>, Ty)> {
        Struct::find_field(struct_id, field_name, self.types, self.arena)
    }

    pub fn spec_sum(&mut self, specs: &[Spec]) -> FragSlice<CompactSpec> {
        let id = self.display_to_interner(specs);

        if id == Interner::EMPTY {
            return default();
        }

        let remaped = specs.iter().map(|&spec| self.spec(spec));

        self.types
            .mapping
            .spec_sums
            .entry(id)
            .or_insert_with(|| self.types.cache.spec_sums.extend(remaped))
            .to_owned()
    }

    pub fn contains_params(&self, ty: Ty) -> bool {
        !matches!(self.contains_params_low(ty), ParamPresence::Absent)
    }

    pub fn contains_params_low(&self, ty: Ty) -> ParamPresence {
        use ParamPresence::*;
        match ty {
            Ty::Instance(instance) => instance
                .args
                .iter()
                .map(|&ty| self.contains_params_low(ty))
                .fold(Absent, ParamPresence::combine),
            Ty::Pointer(pointer) => self
                .contains_params_low(*pointer.ty)
                .combine(pointer.mutability.into())
                .put_behind_pointer(),
            Ty::Array(array) => self.contains_params_low(*array.item),
            Ty::Param(..) => Present,
            Ty::Base(..) | Ty::Builtin(..) => Absent,
        }
    }

    pub fn init(&mut self, builtin_functions: &mut Vec<FragRef<Func>>) {
        SpecBase::init_water_drops(self.types);
        Enum::init_water_drops(self.types);
        Struct::init_water_drops(self.types);
        Func::init_water_drops(self.types);
        self.init_builtin_funcs(builtin_functions);
    }

    fn init_builtin_funcs(&mut self, builtin_functions: &mut Vec<FragRef<Func>>) {
        builtin_functions.extend(Func::WATER_DROPS.map(|(.., func)| func));
        self.types.cache.funcs[Func::CAST] = Func {
            generics: WhereClause::basic(2, &mut self.types).unwrap(), // F, T
            signature: Signature {
                cc: default(),
                args: self
                    .types
                    .cache
                    .args
                    .extend([TyParamIdx::new(0, 1).unwrap().to_compact_ty()]),
                ret: TyParamIdx::new(0, 0).unwrap().to_compact_ty(),
            },
            name: Interner::CAST,
            flags: FuncFlags::BUILTIN,
            ..default()
        };
        self.types.cache.funcs[Func::SIZEOF] = Func {
            generics: WhereClause::basic(1, &mut self.types).unwrap(), // T
            signature: Signature {
                cc: default(),
                args: default(),
                ret: CompactTy::new(ExpandedTy::Builtin(Builtin::Uint)),
            },
            name: Interner::SIZEOF,
            flags: FuncFlags::BUILTIN,
            ..default()
        };

        let mut create_bin_op = |op, a, b, r| {
            let op = self.interner.intern(op);
            let id =
                self.intern_with(|s, t| s.display_bin_op(op, Ty::Builtin(a), Ty::Builtin(b), t));

            let signature = Signature {
                cc: default(),
                args: self
                    .types
                    .cache
                    .args
                    .extend([a, b].map(ExpandedTy::Builtin).map(CompactTy::new)),
                ret: CompactTy::new(ExpandedTy::Builtin(r)),
            };

            let func = Func {
                signature,
                flags: FuncFlags::BUILTIN,
                name: id,
                ..default()
            };

            if let Some(water_drop) = Func::lookup_water_drop(id.get(self.interner)) {
                self.types.cache.funcs[water_drop] = func;
            } else {
                builtin_functions.push(self.types.cache.funcs.push(func));
            }
        };

        fn op_to_ty<'a>(
            op: &'static str,
            ty: impl IntoIterator<Item = Builtin> + Clone,
            mut mapper: impl FnMut(&'static str, Builtin),
        ) {
            for op in op.split_whitespace() {
                for ty in ty.clone() {
                    mapper(op, ty);
                }
            }
        }

        op_to_ty("+ - / * %", Builtin::INTEGERS, |op, ty| {
            create_bin_op(op, ty, ty, ty)
        });
        op_to_ty("+ - / *", Builtin::FLOATS, |op, ty| {
            create_bin_op(op, ty, ty, ty)
        });
        op_to_ty("== != < > <= >=", Builtin::SCALARS, |op, ty| {
            create_bin_op(op, ty, ty, Builtin::Bool)
        });
        op_to_ty("| & ^", Builtin::BINARY, |op, ty| {
            create_bin_op(op, ty, ty, ty)
        });
        op_to_ty(">> <<", Builtin::INTEGERS, |op, ty| {
            create_bin_op(op, ty, ty, ty)
        });

        let create_conv = |(from, to): (Builtin, Builtin)| {
            let name = self.interner.intern(to.name());
            let id = self.interner.intern_scoped(from.name(), name);

            let signature = Signature {
                cc: default(),
                args: self
                    .types
                    .cache
                    .args
                    .extend(iter::once(CompactTy::new(ExpandedTy::Builtin(from)))),
                ret: CompactTy::new(ExpandedTy::Builtin(to)),
            };

            let func = Func {
                signature,
                flags: FuncFlags::BUILTIN,
                name: id,
                ..default()
            };

            let func = self.types.cache.funcs.push(func);

            builtin_functions.push(func);
        };

        Builtin::SCALARS
            .into_iter()
            .flat_map(|from| Builtin::SCALARS.map(|to| (from, to)))
            .filter(|(from, to)| {
                from != to && (!Builtin::FLOATS.contains(from) || *to != Builtin::Bool)
            })
            .for_each(create_conv)
    }

    pub fn ty(&mut self, ty: Ty) -> CompactTy {
        match ty {
            Ty::Base(b) => CompactTy::new(ExpandedTy::Base(b)),
            Ty::Instance(i) => CompactTy::new(ExpandedTy::Instance(self.instance(i))),
            Ty::Pointer(p) => self.pointer(p),
            Ty::Array(a) => self.array(a),
            Ty::Param(p) => CompactTy::new(ExpandedTy::Param {
                param: p.index,
                asoc: p.asoc,
            }),
            Ty::Builtin(b) => CompactTy::new(ExpandedTy::Builtin(b)),
        }
    }

    pub fn array(&mut self, arr: ExpArray) -> CompactTy {
        let id = self.display_to_interner(Ty::Array(arr));
        if let Some(arr) = self.types.mapping.arrays.get(&id) {
            return CompactTy::new(ExpandedTy::Array(*arr));
        }
        let item = self.ty(*arr.item);
        let arr = self.types.cache.arrays.push(Array { item, len: arr.len });

        self.types.mapping.arrays.insert(id, arr);

        CompactTy::new(ExpandedTy::Array(arr))
    }

    pub fn pointer(&mut self, ptr: Pointer) -> CompactTy {
        let id = self.display_to_interner(Ty::Pointer(ptr));
        if let Some(ty) = self.types.mapping.pointers.get(&id) {
            return CompactTy::new(ExpandedTy::Pointer {
                ty: *ty,
                mutability: ptr.mutability.as_param(),
                depth: ptr.depth,
            });
        }

        let ty = self.types.cache.args.push(self.ty(*ptr.ty));
        self.types.mapping.pointers.insert(id, ty);

        CompactTy::new(ExpandedTy::Pointer {
            ty,
            mutability: ptr.mutability.as_param(),
            depth: ptr.depth,
        })
    }

    pub fn instance(&mut self, instance: ExpInstance) -> FragRef<Instance> {
        let id = self.display_to_interner(instance);
        if let Some(item) = self.types.mapping.instances.get(&id) {
            return *item;
        }

        let ExpInstance { base, args } = instance;
        let args = args.iter().map(|&arg| self.ty(arg));
        let instance = self.types.cache.instances.push(Instance {
            base: CompactBaseTy::new(base),
            args: self.types.cache.args.extend(args),
        });

        self.types.mapping.instances.insert(id, instance);

        instance
    }

    pub fn spec(&mut self, spec: Spec) -> CompactSpec {
        let instance @ ExpInstance { base, args } = match spec {
            Spec::Base(b) => return CompactSpec::new(ExpandedSpec::Base(b)),
            Spec::Instance(i) => i,
        };

        let id = self.display_to_interner(instance);
        if let Some(item) = self.types.mapping.specs.get(&id) {
            return CompactSpec::new(ExpandedSpec::Instance(*item));
        }
        let instance = self.types.cache.spec_instances.push(SpecInstance {
            base,
            args: self
                .types
                .cache
                .args
                .extend(args.iter().map(|&arg| self.ty(arg))),
        });

        self.types.mapping.specs.insert(id, instance);

        CompactSpec::new(ExpandedSpec::Instance(instance))
    }

    pub fn load_ty(&self, ty: CompactTy) -> Ty<'arena> {
        Ty::load(ty, self.types, self.arena)
    }

    pub fn load_ty_slice(&self, slice: impl TypecCtxSlice<CompactTy>) -> &'arena [Ty<'arena>] {
        Ty::load_slice(slice.get(self.types), self.types, self.arena)
    }

    pub fn load_spec(&self, spec: CompactSpec) -> Spec<'arena> {
        Spec::load(spec, self.types, self.arena)
    }

    pub fn load_spec_slice(
        &self,
        slice: impl TypecCtxSlice<CompactSpec>,
    ) -> &'arena [Spec<'arena>] {
        Spec::load_slice(slice.get(self.types), self.types, self.arena)
    }
}
