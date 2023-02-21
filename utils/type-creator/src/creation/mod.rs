use crate::*;
use std::{default::default, fmt::Write, iter};
use storage::*;
use types::*;

pub mod display;

impl<'ctx> TypeCreator<'ctx> {
    pub fn may_need_drop(&mut self, ty: Ty) -> bool {
        self.may_need_drop_low(ty, &[]).0
    }

    fn may_need_drop_low(&mut self, ty: Ty, params: &[Ty]) -> (bool, bool) {
        // its split since map entry handle must be dropped.
        let (is, param) = match ty.to_base_and_params(self.types) {
            Ok((base, params)) => {
                if let Some(is) = self.types.may_need_drop.get(&ty) {
                    return (*is, false);
                }

                let params = self.instantiate_slice(params, params);
                let params = &params[..];
                let types = match base {
                    BaseTy::Struct(s) => self.instantiate_fields(s, params),
                    BaseTy::Enum(e) => self.instantiate_variants(e, params),
                };
                types
                    .into_iter()
                    .map(|ty| self.may_need_drop_low(ty, params))
                    .reduce(|(a, b), (c, d)| (a | c, b | d))
                    .unwrap_or((false, false))
            }
            Err(NonBaseTy::Array(a)) => self.may_need_drop_low(self.types[a].item, params),
            Err(NonBaseTy::Pointer(..) | NonBaseTy::Builtin(..)) => return (true, false),
            Err(NonBaseTy::Param(..)) => return (true, true),
        };

        if !param {
            self.types.may_need_drop.insert(ty, is);
        }

        (is, param)
    }

    pub fn find_struct_field(
        &mut self,
        struct_id: FragRef<Struct>,
        params: impl TypecCtxSlice<Ty>,
        field_name: Ident,
    ) -> Option<(usize, FragRef<Field>, Ty)> {
        let Struct { fields, .. } = self.types[struct_id];
        let (i, id, ty) = self
            .types
            .cache
            .fields
            .indexed(fields)
            .enumerate()
            .find_map(|(i, (id, field))| (field.name == field_name).then_some((i, id, field.ty)))?;
        Some((i, id, self.instantiate(ty, params)))
    }

    pub fn spec_sum(
        &mut self,
        specs: impl Iterator<Item = Spec> + Clone + ExactSizeIterator,
    ) -> FragSlice<Spec> {
        let id = self
            .interner
            .intern_with(|s, t| display_spec_sum(self.types, s, specs.clone(), t));

        if id == Interner::EMPTY {
            return default();
        }

        let res = self
            .types
            .mapping
            .lookup
            .entry(id)
            .or_insert_with(|| ComputedTypecItem::SpecSum(self.types.cache.spec_sums.extend(specs)))
            .to_owned();

        match res {
            ComputedTypecItem::SpecSum(ss) => ss,
            res => unreachable!("{res:?}"),
        }
    }

    pub fn contains_params(&self, ty: Ty) -> bool {
        !matches!(self.contains_params_low(ty), ParamPresence::Absent)
    }

    pub fn contains_params_low(&self, ty: Ty) -> ParamPresence {
        use ParamPresence::*;
        match ty {
            Ty::Instance(instance) => self.types[self.types[instance].args]
                .iter()
                .map(|&ty| self.contains_params_low(ty))
                .fold(Absent, ParamPresence::combine),
            Ty::Pointer(pointer) => self
                .contains_params_low(self.types[pointer.ty()])
                .combine(pointer.mutability.to_mutability().into())
                .put_behind_pointer(),
            Ty::Array(array) => self.contains_params_low(self.types[array].item),
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
            generics: self.types.cache.params.extend([default(), default()]), // F, T
            signature: Signature {
                cc: default(),
                args: self.types.cache.args.extend([Ty::Param(1)]),
                ret: Ty::Param(0),
            },
            name: Interner::CAST,
            flags: FuncFlags::BUILTIN,
            ..default()
        };
        self.types.cache.funcs[Func::SIZEOF] = Func {
            generics: self.types.cache.params.extend([default()]), // T
            signature: Signature {
                cc: default(),
                args: default(),
                ret: Ty::UINT,
            },
            name: Interner::SIZEOF,
            flags: FuncFlags::BUILTIN,
            ..default()
        };

        let mut create_bin_op = |op, a, b, r| {
            let op = self.interner.intern(op);
            let id = self
                .interner
                .intern_with(|s, t| display_bin_op(self.types, s, op, a, b, t));

            let signature = Signature {
                cc: default(),
                args: self.types.cache.args.extend([a, b]),
                ret: r,
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

        fn op_to_ty(
            op: &'static str,
            ty: impl IntoIterator<Item = Ty> + Clone,
            mut mapper: impl FnMut(&'static str, Ty),
        ) {
            for op in op.split_whitespace() {
                for ty in ty.clone() {
                    mapper(op, ty);
                }
            }
        }

        op_to_ty("+ - / * %", Ty::INTEGERS, |op, ty| {
            create_bin_op(op, ty, ty, ty)
        });
        op_to_ty("+ - / *", Ty::FLOATS, |op, ty| {
            create_bin_op(op, ty, ty, ty)
        });
        op_to_ty("== != < > <= >=", Ty::SCALARS, |op, ty| {
            create_bin_op(op, ty, ty, Ty::BOOL)
        });
        op_to_ty("| & ^", Ty::BINARY, |op, ty| create_bin_op(op, ty, ty, ty));
        op_to_ty(">> <<", Ty::INTEGERS, |op, ty| {
            create_bin_op(op, ty, ty, ty)
        });

        let create_conv = |(from, to): (Ty, Ty)| {
            let name = self.interner.intern_with(|_, t| write!(t, "{to}"));
            let id = self.interner.intern_scoped(from, name);

            let signature = Signature {
                cc: default(),
                args: self.types.cache.args.extend(iter::once(from)),
                ret: to,
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

        Ty::SCALARS
            .into_iter()
            .flat_map(|from| Ty::SCALARS.map(|to| (from, to)))
            .filter(|(from, to)| from != to && (!Ty::FLOATS.contains(from) || *to != Ty::BOOL))
            .for_each(create_conv)
    }

    pub fn pointer_to(&mut self, mutability: RawMutability, base: Ty) -> Pointer {
        let id = self.interner.intern_with(|s, t| {
            t.push_str("ptr ");
            base.display(self.types, s, t)
        });
        let depth = base.ptr_depth() + 1;
        let ty = self
            .types
            .mapping
            .lookup
            .entry(id)
            .or_insert_with(|| ComputedTypecItem::Pointer(self.types.cache.args.push(base)))
            .to_owned();

        let ComputedTypecItem::Pointer(ty) = ty else { unreachable!() };
        debug_assert_eq!(self.types[ty], base);

        Pointer::new(ty, mutability, depth)
    }

    pub fn instance(&mut self, base: BaseTy, args: &[Ty]) -> FragRef<Instance> {
        let id = self
            .interner
            .intern_with(|s, t| display_instance(self.types, s, base, args, t));
        let item = self
            .types
            .mapping
            .lookup
            .entry(id)
            .or_insert_with(|| {
                let instance = Instance {
                    base,
                    args: self.types.cache.args.extend(args.iter().cloned()),
                };
                let instance = self.types.cache.instances.push(instance);
                ComputedTypecItem::Instance(instance)
            })
            .to_owned();
        match item {
            ComputedTypecItem::Instance(instance) => instance,
            _ => unreachable!(),
        }
    }

    pub fn spec_instance(&mut self, base: FragRef<SpecBase>, args: &[Ty]) -> FragRef<SpecInstance> {
        let id = self
            .interner
            .intern_with(|s, t| display_spec_instance(self.types, s, base, args, t));
        let item = self
            .types
            .mapping
            .lookup
            .entry(id)
            .or_insert_with(|| {
                let instance = SpecInstance {
                    base,
                    args: self.types.cache.args.extend(args.iter().cloned()),
                };
                let instance = self.types.cache.spec_instances.push(instance);
                ComputedTypecItem::SpecInstance(instance)
            })
            .to_owned();
        match item {
            ComputedTypecItem::SpecInstance(instance) => instance,
            _ => unreachable!(),
        }
    }

    pub fn implements_sum(
        &mut self,
        ty: Ty,
        sum: FragSlice<Spec>,
        params: impl TypecCtxSlice<FragSlice<Spec>>,
        inferred: &[Ty],
        missing_keys: &mut Option<&mut BumpVec<ImplKey>>,
    ) -> bool {
        self.types[sum].to_bumpvec().into_iter().all(|spec| {
            let spec = self.instantiate_spec(spec, inferred);
            self.find_implementation(ty, spec, params, missing_keys)
                .is_some()
        })
    }

    pub fn dereference(&self, ty: Ty) -> Ty {
        match ty {
            Ty::Pointer(ptr, ..) => self.types[ptr.ty()],
            _ => ty,
        }
    }

    pub fn find_implementation(
        &mut self,
        ty: Ty,
        spec: Spec,
        params: impl TypecCtxSlice<FragSlice<Spec>>,
        missing_keys: &mut Option<&mut BumpVec<ImplKey>>,
    ) -> Option<Option<(FragRef<Impl>, FragSlice<Ty>)>> {
        if let Ty::Param(index) = ty {
            let mut frontier = self.types[params.get(self.types)[index as usize]].to_bumpvec();
            while let Some(other_spec) = frontier.pop() {
                if spec == other_spec {
                    return Some(None);
                }

                let params = match other_spec {
                    Spec::Base(..) => default(),
                    Spec::Instance(instance) => self.types[instance].args,
                };

                for inherit in self.types[other_spec.base(self.types)].inherits.keys() {
                    let inherit = self.instantiate_spec(self.types[inherit], params);
                    frontier.push(inherit);
                }
            }
            return None;
        }

        let key = ImplKey { ty, spec };

        if let Some(result) = self.types.implemented.get(&key) {
            return Some(Some(result.to_owned()));
        }

        let ty_base = match ty {
            Ty::Instance(instance) => self.types[instance].base.as_ty(),
            _ => ty,
        };
        let spec_base = spec.base(self.types);

        let base_impls = self
            .types
            .impl_lookup
            .get(&(spec_base, ty_base))
            .map(|i| i.inner.to_owned())
            .into_iter()
            .flatten();

        for r#impl in base_impls {
            let impl_ent = self.types.cache.impls[r#impl];

            let mut generic_slots = bumpvec![None; impl_ent.generics.len()];
            let spec_compatible = self
                .types
                .compatible(&mut generic_slots, ty, impl_ent.key.ty);
            let ty_compatible =
                self.types
                    .compatible_spec(&mut generic_slots, spec, impl_ent.key.spec);

            if ty_compatible.is_err() || spec_compatible.is_err() {
                continue;
            }

            let params = generic_slots
                .into_iter()
                .collect::<Option<BumpVec<_>>>()
                .expect("generic slots should not be empty since we validate the implementation");

            let implements = impl_ent
                .generics
                .keys()
                .zip(params.iter())
                .all(|(specs, &ty)| {
                    self.implements_sum(
                        ty,
                        self.types[specs],
                        impl_ent.generics,
                        &params,
                        missing_keys,
                    )
                });

            if !implements {
                continue;
            }

            let params = self.types.cache.args.extend(params);
            if !self.contains_params(ty) {
                self.types.implemented.insert(key, (r#impl, params));
            }
            return Some(Some((r#impl, params)));
        }

        if let Some(v) = missing_keys {
            v.push(key)
        }

        None
    }

    pub fn balance_pointers(&mut self, mut ty: Ty, reference: Ty) -> Ty {
        let (ty_depth, reference_depth) = (ty.ptr_depth(), reference.ptr_depth());
        match ty_depth.cmp(&reference_depth) {
            std::cmp::Ordering::Less => {
                let muts = (0..(reference_depth - ty_depth))
                    .scan(reference, |r, _| {
                        let mutability = ty.mutability();
                        *r = r.ptr_base(self.types);
                        Some(mutability)
                    })
                    .collect::<BumpVec<_>>();

                for mutability in muts.into_iter().rev() {
                    ty = Ty::Pointer(self.pointer_to(mutability, ty));
                }

                ty
            }
            std::cmp::Ordering::Equal => ty,
            std::cmp::Ordering::Greater => {
                for _ in 0..(ty_depth - reference_depth) {
                    ty = ty.ptr_base(self.types);
                }
                ty
            }
        }
    }

    pub fn instantiate_slice(
        &mut self,
        slice: impl TypecCtxSlice<Ty>,
        params: impl TypecCtxSlice<Ty>,
    ) -> BumpVec<Ty> {
        let mut types = slice.get(self.types).to_bumpvec();
        for ty in types.iter_mut() {
            *ty = self.instantiate(*ty, params);
        }
        types
    }

    pub fn try_instantiate_slice(
        &mut self,
        slice: impl TypecCtxSlice<Ty>,
        params: &[Option<Ty>],
    ) -> Option<BumpVec<Ty>> {
        let mut types = slice.get(self.types).to_bumpvec();
        for ty in types.iter_mut() {
            *ty = self.try_instantiate(*ty, params)?;
        }
        Some(types)
    }

    pub fn instantiate_fields(
        &mut self,
        struct_id: FragRef<Struct>,
        params: impl TypecCtxSlice<Ty>,
    ) -> BumpVec<Ty> {
        let mut fields = self.types[self.types[struct_id].fields]
            .iter()
            .map(|f| f.ty)
            .collect::<BumpVec<_>>();
        for field in fields.iter_mut() {
            *field = self.instantiate(*field, params);
        }
        fields
    }

    pub fn instantiate_variants(
        &mut self,
        enum_id: FragRef<Enum>,
        params: impl TypecCtxSlice<Ty>,
    ) -> BumpVec<Ty> {
        let mut fields = self.types[self.types[enum_id].variants]
            .iter()
            .map(|f| f.ty)
            .collect::<BumpVec<_>>();
        for field in fields.iter_mut() {
            *field = self.instantiate(*field, params);
        }
        fields
    }

    pub fn instantiate(&mut self, ty: Ty, params: impl TypecCtxSlice<Ty>) -> Ty {
        match ty {
            Ty::Base(..) | Ty::Builtin(..) => ty,
            ty if params.is_empty() => ty,
            Ty::Instance(instance) => {
                let Instance { base, args } = self.types[instance];
                let args = args
                    .keys()
                    .map(|arg| self.instantiate(self.types[arg], params))
                    .collect::<BumpVec<_>>();
                Ty::Instance(self.instance(base, args.as_slice()))
            }
            Ty::Pointer(pointer) => {
                let base = self.types[pointer.ty()];
                let base = self.instantiate(base, params);
                let mutability = pointer.mutability.instantiate(params.get(self.types));
                Ty::Pointer(self.pointer_to(mutability, base))
            }
            Ty::Param(index) => params.get(self.types)[index as usize],
            Ty::Array(array) => {
                let Array { item: ty, len } = self.types[array];
                let ty = self.instantiate(ty, params);
                Ty::Array(self.array_of(ty, len))
            }
        }
    }

    pub fn try_instantiate(&mut self, ty: Ty, params: &[Option<Ty>]) -> Option<Ty> {
        Some(match ty {
            Ty::Base(..) | Ty::Builtin(..) => ty,
            ty if params.is_empty() => ty,
            Ty::Instance(instance) => {
                let Instance { base, args } = self.types[instance];
                let args = self.try_instantiate_slice(args, params)?;
                Ty::Instance(self.instance(base, args.as_slice()))
            }
            Ty::Pointer(pointer) => {
                let base = self.types[pointer.ty()];
                let base = self.try_instantiate(base, params)?;
                let mutability = pointer.mutability.try_instantiate(params)?;
                Ty::Pointer(self.pointer_to(mutability, base))
            }
            Ty::Param(index) => return params[index as usize],
            Ty::Array(array) => {
                let Array { item: ty, len } = self.types[array];
                let ty = self.try_instantiate(ty, params)?;
                Ty::Array(self.array_of(ty, len))
            }
        })
    }

    pub fn array_of(&mut self, item: Ty, len: ArraySize) -> FragRef<Array> {
        let id = self
            .interner
            .intern_with(|s, t| display_array(self.types, s, item, len, t));
        let item = self
            .types
            .mapping
            .lookup
            .entry(id)
            .or_insert_with(|| {
                let array = self.types.cache.arrays.push(Array { item, len });
                ComputedTypecItem::Array(array)
            })
            .to_owned();

        match item {
            ComputedTypecItem::Array(array) => array,
            _ => unreachable!(),
        }
    }

    pub fn instantiate_spec(&mut self, spec: Spec, params: impl TypecCtxSlice<Ty>) -> Spec {
        match spec {
            Spec::Base(..) => spec,
            spec if params.is_empty() => spec,
            Spec::Instance(instance) => {
                let SpecInstance { base, args } = self.types[instance];
                let args = self.instantiate_slice(args, params);
                Spec::Instance(self.spec_instance(base, args.as_slice()))
            }
        }
    }

    pub fn try_instantiate_spec(&mut self, spec: Spec, params: &[Option<Ty>]) -> Option<Spec> {
        Some(match spec {
            Spec::Base(..) => spec,
            spec if params.is_empty() => spec,
            Spec::Instance(instance) => {
                let SpecInstance { base, args } = self.types[instance];
                let args = self.try_instantiate_slice(args, params)?;
                Spec::Instance(self.spec_instance(base, args.as_slice()))
            }
        })
    }

    pub fn component_ty(&mut self, ty: Ty, index: usize) -> Option<Ty> {
        let Self { types, .. } = self;
        let getter = |ty| match ty {
            BaseTy::Struct(s) => types[types[s].fields][index].ty,
            BaseTy::Enum(e) => types[types[e].variants][index].ty,
        };
        Some(match ty {
            Ty::Base(base) => getter(base),
            Ty::Instance(i) => {
                let Instance { base, args } = types[i];
                let ty = getter(base);
                self.instantiate(ty, args)
            }
            _ => return None,
        })
    }

    pub fn find_component(&mut self, ty: Ty, name: Ident) -> Option<(usize, Ty)> {
        let Self { types, .. } = self;
        let finder = |ty| match ty {
            BaseTy::Struct(s) => Struct::find_field(s, name, types),
            BaseTy::Enum(e) => Enum::find_variant(e, name, types),
        };
        match ty {
            Ty::Base(base) => finder(base),
            Ty::Instance(i) => {
                let Instance { base, args } = types[i];
                let (index, ty) = finder(base)?;
                Some((index, self.instantiate(ty, args)))
            }
            _ => None,
        }
    }

    /// None - don't know
    /// Some(None) - not drop
    /// Some(Some(..)) - drop
    pub fn is_drop(
        &mut self,
        ty: Ty,
        params: &[FragSlice<Spec>],
    ) -> Option<Option<(FragRef<Impl>, FragSlice<Ty>)>> {
        match ty {
            Ty::Pointer(..) | Ty::Builtin(..) => Some(None),
            Ty::Param(..) => self
                .find_implementation(ty, Spec::Base(SpecBase::COPY), params, &mut None)
                .map(|_| None),
            Ty::Array(a) => self.find_implementation(
                self.types[a].item,
                Spec::Base(SpecBase::DROP),
                params,
                &mut None,
            ),
            Ty::Base(..) | Ty::Instance(..) => Some(
                self.find_implementation(ty, Spec::Base(SpecBase::DROP), params, &mut None)
                    .flatten(),
            ),
        }
    }

    pub fn is_copy(&mut self, ty: Ty, params: &[FragSlice<Spec>]) -> bool {
        match ty {
            Ty::Pointer(..) | Ty::Builtin(..) => true,
            Ty::Array(a) => self
                .find_implementation(
                    self.types[a].item,
                    Spec::Base(SpecBase::COPY),
                    params,
                    &mut None,
                )
                .is_some(),
            Ty::Base(..) | Ty::Instance(..) | Ty::Param(..) => self
                .find_implementation(ty, Spec::Base(SpecBase::COPY), params, &mut None)
                .is_some(),
        }
    }

    pub fn display<T: TypeDisplay>(&self, value: T) -> String {
        display(self.types, self.interner, value)
    }

    pub fn type_diff(&self, a: Ty, b: Ty) -> String {
        type_diff(self.types, self.interner, a, b)
    }
}
