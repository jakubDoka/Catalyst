use super::*;

impl Middleware {
    pub(crate) fn sweep_resources(
        &mut self,
        resources: &Resources,
        main_task: &mut Task,
        module_items: &mut Map<VRef<Source>, ModuleItems>,
        to_remove: Vec<VRef<Source>>,
    ) {
        for removed in to_remove {
            module_items.remove(&removed);
        }

        module_items
            .values()
            .flat_map(|items| items.items.values())
            .for_each(|item| self.mark_module_item(item, resources, main_task));

        self.mark_builtin(resources, main_task);

        self.relocator.relocate(&mut main_task.typec);

        self.rewrite_references(main_task)
            .expect("since we relocated, this should be fine");
    }

    pub fn mark_builtin(&mut self, resources: &Resources, main_task: &mut Task) {
        for (.., wd) in SpecBase::WATER_DROPS {
            self.mark_spec_base(wd, resources, main_task);
        }
        for (.., wd) in Enum::WATER_DROPS {
            self.mark_enum(wd, resources, main_task);
        }
        for (.., wd) in Struct::WATER_DROPS {
            self.mark_struct(wd, resources, main_task);
        }
        for (.., wd) in Func::WATER_DROPS {
            self.mark_func(wd, resources, main_task);
        }
        for &builtin_func in &main_task.typec.builtin_funcs {
            self.mark_func(builtin_func, resources, main_task);
        }
    }

    fn rewrite_references(&mut self, main_task: &mut Task) -> Option<()> {
        for param in main_task.typec.params.iter_mut() {
            *param = self.relocator.spec_sums.project_slice(*param);
        }

        for spec in main_task.typec.spec_sums.iter_mut() {
            *spec = self.relocator.project_spec(*spec)?;
        }

        for Field { ty, .. } in main_task.typec.fields.iter_mut() {
            *ty = self.relocator.project_ty(*ty)?;
        }

        for Struct {
            generics, fields, ..
        } in main_task.typec.structs.iter_mut()
        {
            *generics = self.relocator.params.project_slice(*generics);
            *fields = self.relocator.fields.project_slice(*fields);
        }

        for Variant { ty, .. } in main_task.typec.variants.iter_mut() {
            *ty = self.relocator.project_ty(*ty)?;
        }

        for Enum {
            generics, variants, ..
        } in main_task.typec.enums.iter_mut()
        {
            *generics = self.relocator.params.project_slice(*generics);
            *variants = self.relocator.variants.project_slice(*variants);
        }

        for ty in main_task.typec.args.iter_mut() {
            *ty = self.relocator.project_ty(*ty)?;
        }

        for Func {
            generics,
            owner,
            upper_generics,
            signature,
            ..
        } in main_task.typec.funcs.iter_mut()
        {
            *generics = self.relocator.params.project_slice(*generics);
            *owner = owner
                .map(|owner| self.relocator.project_ty(owner))
                .transpose()?;
            *upper_generics = self.relocator.params.project_slice(*upper_generics);
            *signature = self.relocator.project_signature(*signature)?;
        }

        for Pointer { base, .. } in main_task.typec.pointers.iter_mut() {
            *base = self.relocator.project_ty(*base)?;
        }

        for Instance { args, base } in main_task.typec.instances.iter_mut() {
            *args = self.relocator.args.project_slice(*args);
            *base = self.relocator.project_generic_ty(*base)?;
        }

        for SpecFunc {
            generics,
            signature,
            parent,
            ..
        } in main_task.typec.spec_funcs.iter_mut()
        {
            *generics = self.relocator.params.project_slice(*generics);
            *signature = self.relocator.project_signature(*signature)?;
            *parent = self.relocator.base_specs.project(*parent)?;
        }

        for SpecBase {
            generics,
            inherits,
            methods,
            ..
        } in main_task.typec.base_specs.iter_mut()
        {
            *generics = self.relocator.params.project_slice(*generics);
            *inherits = self.relocator.spec_sums.project_slice(*inherits);
            *methods = self.relocator.spec_funcs.project_slice(*methods);
        }

        for SpecInstance { base, args } in main_task.typec.spec_instances.iter_mut() {
            *base = self.relocator.base_specs.project(*base)?;
            *args = self.relocator.args.project_slice(*args);
        }

        for func in main_task.typec.func_slices.iter_mut() {
            *func = self.relocator.funcs.project(*func)?;
        }

        for Impl {
            generics,
            key,
            methods,
            ..
        } in main_task.typec.impls.iter_mut()
        {
            *generics = self.relocator.params.project_slice(*generics);
            *key = self.relocator.project_impl_key(*key)?;
            *methods = self.relocator.func_slices.project_slice(*methods);
        }

        Some(())
    }

    fn mark_module_item(
        &mut self,
        module_item: &ModuleItem,
        resources: &Resources,
        main_task: &Task,
    ) {
        match module_item.ptr {
            ModuleItemPtr::Func(func) => self.mark_func(func, resources, main_task),
            ModuleItemPtr::Ty(ty) => self.mark_ty(ty, resources, main_task),
            ModuleItemPtr::SpecBase(spec_base) => {
                self.mark_spec_base(spec_base, resources, main_task)
            }
            ModuleItemPtr::Impl(r#impl_base) => self.mark_impl(r#impl_base, resources, main_task),
        };
    }

    fn mark_func(
        &mut self,
        func: FragRef<Func>,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.relocator.funcs.mark(func)?;
        let Func {
            generics,
            owner,
            upper_generics,
            signature,
            ..
        } = main_task.typec[func];
        self.mark_generics(generics, resources, main_task);
        if let Some(owner) = owner {
            self.mark_ty(owner, resources, main_task);
        }
        self.mark_generics(upper_generics, resources, main_task);
        self.mark_signature(signature, resources, main_task);
        self.mark_mir_body(func, resources, main_task);
        Some(())
    }

    fn mark_mir_body(
        &mut self,
        func: FragRef<Func>,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        let body = main_task.mir.bodies.get(&func)?;

        for ty in body.types.values() {
            self.mark_ty(ty.ty, resources, main_task);
        }

        for call in body.calls.values() {
            match call.callable {
                CallableMir::Func(f) => self.mark_func(f, resources, main_task)?,
                CallableMir::SpecFunc(s) => {
                    self.mark_spec_func(main_task.typec[s], resources, main_task)?
                }
                CallableMir::Pointer(..) => (),
            }
        }

        Some(())
    }

    fn mark_ty(&mut self, ty: Ty, resources: &Resources, main_task: &Task) -> Option<()> {
        match ty {
            Ty::Struct(s) => self.mark_struct(s, resources, main_task),
            Ty::Enum(e) => self.mark_enum(e, resources, main_task),
            Ty::Instance(i) => self.mark_instance(i, resources, main_task),
            Ty::Pointer(p, ..) => self.mark_pointer(p, resources, main_task),
            Ty::Param(..) | Ty::Builtin(..) => Some(()),
        }
    }

    fn mark_struct(
        &mut self,
        s: FragRef<Struct>,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.relocator.structs.mark(s)?;
        let Struct {
            generics, fields, ..
        } = main_task.typec[s];
        self.mark_generics(generics, resources, main_task);
        self.relocator.fields.mark_slice_summed(fields);
        for field in &main_task.typec[fields] {
            self.mark_ty(field.ty, resources, main_task);
        }
        Some(())
    }

    fn mark_enum(
        &mut self,
        e: FragRef<Enum>,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.relocator.enums.mark(e)?;
        let Enum {
            generics, variants, ..
        } = main_task.typec[e];
        self.mark_generics(generics, resources, main_task);
        self.relocator.variants.mark_slice_summed(variants);
        for variant in &main_task.typec[variants] {
            self.mark_ty(variant.ty, resources, main_task);
        }
        Some(())
    }

    fn mark_instance(
        &mut self,
        i: FragRef<Instance>,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.relocator.instances.mark(i)?;
        let Instance { base, args } = main_task.typec[i];
        self.mark_ty(base.as_ty(), resources, main_task);
        self.mark_args(args, resources, main_task);
        Some(())
    }

    fn mark_pointer(
        &mut self,
        p: FragRef<Pointer>,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.relocator.pointers.mark(p)?;
        let Pointer { base, .. } = main_task.typec[p];
        self.mark_ty(base, resources, main_task);
        Some(())
    }

    fn mark_generics(
        &mut self,
        generics: Generics,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.relocator.params.mark_slice_summed(generics)?;
        for &spec_sum in &main_task.typec[generics] {
            self.mark_spec_sum(spec_sum, resources, main_task);
        }
        Some(())
    }

    fn mark_args(
        &mut self,
        args: FragSlice<Ty>,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.relocator.args.mark_slice_summed(args)?;
        for &arg in &main_task.typec[args] {
            self.mark_ty(arg, resources, main_task);
        }
        Some(())
    }

    fn mark_spec_sum(
        &mut self,
        spec_sum: FragSlice<Spec>,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.relocator.spec_sums.mark_slice_summed(spec_sum)?;
        for &spec in &main_task.typec[spec_sum] {
            self.mark_spec(spec, resources, main_task);
        }
        Some(())
    }

    fn mark_spec(&mut self, spec: Spec, resources: &Resources, main_task: &Task) -> Option<()> {
        match spec {
            Spec::Base(spec_base) => self.mark_spec_base(spec_base, resources, main_task),
            Spec::Instance(spec_instance) => {
                self.mark_spec_instance(spec_instance, resources, main_task)
            }
        }
    }

    fn mark_spec_base(
        &mut self,
        spec_base: FragRef<SpecBase>,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.relocator.base_specs.mark(spec_base)?;
        let SpecBase {
            generics,
            inherits,
            methods,
            ..
        } = main_task.typec[spec_base];
        self.mark_generics(generics, resources, main_task);
        self.mark_spec_sum(inherits, resources, main_task);
        self.relocator.spec_funcs.mark_slice_summed(methods);
        for &spec_func in &main_task.typec[methods] {
            self.mark_spec_func(spec_func, resources, main_task);
        }
        Some(())
    }

    fn mark_spec_instance(
        &mut self,
        spec_instance: FragRef<SpecInstance>,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.relocator.spec_instances.mark(spec_instance)?;
        let SpecInstance { base, args } = main_task.typec[spec_instance];
        self.mark_spec_base(base, resources, main_task);
        self.mark_args(args, resources, main_task);
        Some(())
    }

    fn mark_spec_func(
        &mut self,
        SpecFunc {
            generics,
            signature,
            ..
        }: SpecFunc,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.mark_generics(generics, resources, main_task);
        self.mark_signature(signature, resources, main_task);
        Some(())
    }

    fn mark_signature(
        &mut self,
        Signature { args, ret, .. }: Signature,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.mark_args(args, resources, main_task);
        self.mark_ty(ret, resources, main_task);
        Some(())
    }

    fn mark_impl(
        &mut self,
        r#impl: FragRef<Impl>,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.relocator.impls.mark(r#impl)?;
        let Impl {
            generics,
            key: ImplKey { ty, spec },
            methods,
            ..
        } = main_task.typec[r#impl];
        self.mark_generics(generics, resources, main_task);
        self.mark_ty(ty, resources, main_task);
        self.mark_spec(spec, resources, main_task);
        self.mark_impl_methods(methods, resources, main_task);
        Some(())
    }

    fn mark_impl_methods(
        &mut self,
        methods: FragRefSlice<Func>,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.relocator.func_slices.mark_slice_summed(methods)?;
        for &method in &main_task.typec[methods] {
            self.mark_func(method, resources, main_task);
        }
        Some(())
    }
}
