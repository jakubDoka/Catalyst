use std::mem;

use gen::*;
use mir_t::*;
use packaging_t::*;
use serde::{Deserialize, Serialize};
use storage::*;
use typec_t::*;

#[derive(Default, Serialize, Deserialize)]
pub struct Incremental {
    pub typec: Typec,
    pub mir: Mir,
    pub gen: Gen,
    pub sources: PushMap<Source>,
    pub loaded: bool,
}

impl Incremental {
    pub fn sweep(&mut self, resources: &Resources, ctx: &mut SweepCtx, interner: &mut Interner) {
        ctx.clear();
        self.module_items(resources, ctx, interner);
        self.impl_lookup(ctx, interner);
        self.mir_bodies(ctx, interner);
        self.compiled_funcs(ctx);
        mem::swap(self, &mut ctx.temp);
    }

    fn module_items(&mut self, resources: &Resources, ctx: &mut SweepCtx, interner: &mut Interner) {
        let mut module_items = mem::take(&mut self.typec.module_items);
        for (module, items) in module_items.iter_mut() {
            if resources.sources[resources.modules[module].source].changed {
                items.clear();
            }
        }
        for item in module_items.values_mut().flat_map(|m| m.items.values_mut()) {
            self.item(item, ctx, interner);
        }
        self.typec.module_items = module_items;
        mem::swap(
            &mut self.typec.module_items,
            &mut ctx.temp.typec.module_items,
        );
    }

    fn impl_lookup(&mut self, ctx: &mut SweepCtx, interner: &mut Interner) {
        for (&key, &r#impl) in &self.typec.impl_lookup {
            let ty_exists = match key.ty.caller(&self.typec) {
                Ty::Struct(s) => ctx.projections.structs[s].is_some(),
                Ty::Enum(e) => ctx.projections.enums[e].is_some(),
                _ => false,
            };
            let spec_exists = ctx.projections.base_specs[key.spec.base(&self.typec)].is_some();

            if !ty_exists || !spec_exists {
                continue;
            }

            let r#impl = self.r#impl(r#impl, ctx, interner);
            ctx.temp
                .typec
                .impl_lookup
                .insert(self.typec.impls[r#impl].key, r#impl);
        }
        self.typec.impl_lookup.clear();
    }

    fn mir_bodies(&mut self, ctx: &mut SweepCtx, interner: &mut Interner) {
        let mut bodies = mem::take(&mut self.mir.bodies);
        for (key, mut func) in bodies.drain() {
            let Some(proj) = ctx.projections.funcs[key] else {
                continue;
            };

            func.dependant_types
                .values_mut()
                .for_each(|ty| ty.ty = self.ty(ty.ty, ctx, interner));

            ctx.temp.mir.bodies.insert(proj, func);
        }
        self.mir.bodies = bodies;
    }

    fn compiled_funcs(&mut self, ctx: &mut SweepCtx) {
        let mut compiled_funcs = mem::take(&mut self.gen.compiled_funcs);
        for (&key, func) in compiled_funcs.iter() {
            let Some(proj) = ctx.projections.funcs[func.func] else {
                continue;
            };

            let func = CompiledFunc {
                func: proj,
                ..func.clone()
            };

            ctx.temp.gen.compiled_funcs.insert(key, func);
        }
        compiled_funcs.clear();
        self.gen.compiled_funcs = compiled_funcs;
    }

    fn r#impl(
        &self,
        r#impl: VRef<Impl>,
        ctx: &mut SweepCtx,
        interner: &mut Interner,
    ) -> VRef<Impl> {
        if let Some(r#impl) = ctx.projections.impls[r#impl] {
            return r#impl;
        }

        let Impl {
            generics,
            key,
            methods,
            next,
            span,
        } = self.typec.impls[r#impl];

        let ent = Impl {
            generics: self.generics(generics, ctx, interner),
            key: ImplKey {
                ty: self.ty(key.ty, ctx, interner),
                spec: self.spec(key.spec, ctx, interner),
            },
            methods: self.func_slice(methods, ctx, interner),
            next: next.map(|next| self.r#impl(next, ctx, interner)),
            span,
        };

        let i = ctx.temp.typec.impls.push(ent);
        ctx.projections.impls[r#impl] = Some(i);
        i
    }

    fn item(&self, item: &mut ModuleItem, ctx: &mut SweepCtx, interner: &mut Interner) {
        match item.ptr {
            ModuleItemPtr::Func(ref mut func) => *func = self.func(*func, ctx, interner),
            ModuleItemPtr::Ty(ref mut ty) => *ty = self.ty(*ty, ctx, interner),
            ModuleItemPtr::SpecBase(ref mut spec) => *spec = self.spec_base(*spec, ctx, interner),
        }
    }

    fn func(&self, func: VRef<Func>, ctx: &mut SweepCtx, interner: &mut Interner) -> VRef<Func> {
        if let Some(func) = ctx.projections.funcs[func] {
            return func;
        }

        let mut func_ent @ Func {
            generics,
            owner,
            upper_generics,
            signature,
            flags: _,
            visibility: _,
            name: _,
            loc: _,
        } = self.typec.funcs[func];

        func_ent.generics = self.generics(generics, ctx, interner);
        func_ent.owner = owner.map(|owner| self.ty(owner, ctx, interner));
        func_ent.upper_generics = self.generics(upper_generics, ctx, interner);
        func_ent.signature = self.signature(signature, ctx, interner);

        let f = ctx.temp.typec.funcs.push(func_ent);
        ctx.projections.funcs[func] = Some(f);
        f
    }

    fn generics(
        &self,
        generics: Generics,
        ctx: &mut SweepCtx,
        interner: &mut Interner,
    ) -> Generics {
        let generics = self.typec.params[generics]
            .iter()
            .map(|&generic| self.spec_sum(generic, ctx, interner))
            .collect::<BumpVec<_>>();
        ctx.temp.typec.params.bump(generics)
    }

    fn spec_sum(
        &self,
        spec_sum: VSlice<Spec>,
        ctx: &mut SweepCtx,
        interner: &mut Interner,
    ) -> VSlice<Spec> {
        let specs = self.typec.spec_sums[spec_sum]
            .iter()
            .map(|&spec| self.spec(spec, ctx, interner))
            .collect::<BumpVec<_>>();
        ctx.temp.typec.spec_sums.bump(specs)
    }

    fn spec(&self, spec: Spec, ctx: &mut SweepCtx, interner: &mut Interner) -> Spec {
        match spec {
            Spec::Base(base) => Spec::Base(self.spec_base(base, ctx, interner)),
            Spec::Instance(instance) => Spec::Instance(self.spec_instance(instance, ctx, interner)),
        }
    }

    fn spec_base(
        &self,
        spec_base: VRef<SpecBase>,
        ctx: &mut SweepCtx,
        interner: &mut Interner,
    ) -> VRef<SpecBase> {
        if let Some(spec_base) = ctx.projections.base_specs[spec_base] {
            return spec_base;
        }

        let mut spec_base_ent @ SpecBase {
            name: _,
            generics,
            loc: _,
            methods,
        } = self.typec.base_specs[spec_base];

        spec_base_ent.generics = self.generics(generics, ctx, interner);
        spec_base_ent.methods = self.spec_funcs(methods, ctx, interner);

        let s = ctx.temp.typec.base_specs.push(spec_base_ent);
        ctx.projections.base_specs[spec_base] = Some(s);
        s
    }

    fn spec_funcs(
        &self,
        spec_methods: VSlice<SpecFunc>,
        ctx: &mut SweepCtx,
        interner: &mut Interner,
    ) -> VSlice<SpecFunc> {
        let spec_methods = self.typec.spec_funcs[spec_methods]
            .iter()
            .map(|&func| self.spec_func(func, ctx, interner))
            .collect::<BumpVec<_>>();
        ctx.temp.typec.spec_funcs.bump(spec_methods)
    }

    fn spec_func(
        &self,
        spec_func: SpecFunc,
        ctx: &mut SweepCtx,
        interner: &mut Interner,
    ) -> SpecFunc {
        let mut spec_func_ent @ SpecFunc {
            name: _,
            generics,
            signature,
            span: _,
            parent: _,
        } = spec_func;

        spec_func_ent.generics = self.generics(generics, ctx, interner);
        spec_func_ent.signature = self.signature(signature, ctx, interner);

        spec_func_ent
    }

    fn signature(
        &self,
        signature: Signature,
        ctx: &mut SweepCtx,
        interner: &mut Interner,
    ) -> Signature {
        let Signature { cc, args, ret } = signature;

        Signature {
            cc,
            args: self.ty_args(args, ctx, interner),
            ret: self.ty(ret, ctx, interner),
        }
    }

    fn spec_instance(
        &self,
        spec_instance: VRef<SpecInstance>,
        ctx: &mut SweepCtx,
        interner: &mut Interner,
    ) -> VRef<SpecInstance> {
        let SpecInstance { base, args } = self.typec.spec_instances[spec_instance];
        let base = self.spec_base(base, ctx, interner);
        let args = self.ty_args_bump(args, ctx, interner);
        ctx.temp.typec.spec_instance(base, &args, interner)
    }

    fn ty_args(&self, args: VSlice<Ty>, ctx: &mut SweepCtx, interner: &mut Interner) -> VSlice<Ty> {
        let args = self.ty_args_bump(args, ctx, interner);
        ctx.temp.typec.args.bump(args)
    }

    fn ty_args_bump(
        &self,
        args: VSlice<Ty>,
        ctx: &mut SweepCtx,
        interner: &mut Interner,
    ) -> BumpVec<Ty> {
        self.typec.args[args]
            .iter()
            .map(|&arg| self.ty(arg, ctx, interner))
            .collect::<BumpVec<_>>()
    }

    fn func_slice(
        &self,
        funcs: VRefSlice<Func>,
        ctx: &mut SweepCtx,
        interner: &mut Interner,
    ) -> VRefSlice<Func> {
        let funcs = self.typec.func_slices[funcs]
            .iter()
            .map(|&func| self.func(func, ctx, interner))
            .collect::<BumpVec<_>>();
        ctx.temp.typec.func_slices.bump(funcs)
    }

    fn ty(&self, ty: Ty, ctx: &mut SweepCtx, interner: &mut Interner) -> Ty {
        match ty {
            Ty::Struct(s) => Ty::Struct(self.r#struct(s, ctx, interner)),
            Ty::Enum(e) => Ty::Enum(self.r#enum(e, ctx, interner)),
            Ty::Instance(i) => Ty::Instance(self.instance(i, ctx, interner)),
            Ty::Pointer(p) => Ty::Pointer(self.pointer(p, ctx, interner)),
            as_is @ (Ty::Param(..) | Ty::Builtin(..)) => as_is,
        }
    }

    fn r#struct(
        &self,
        r#struct: VRef<Struct>,
        ctx: &mut SweepCtx,
        interner: &mut Interner,
    ) -> VRef<Struct> {
        if let Some(r#struct) = ctx.projections.structs[r#struct] {
            return r#struct;
        }

        let mut r#struct_ent @ Struct {
            name: _,
            generics,
            loc: _,
            fields,
        } = self.typec.structs[r#struct];

        r#struct_ent.generics = self.generics(generics, ctx, interner);
        r#struct_ent.fields = self.fields(fields, ctx, interner);

        let s = ctx.temp.typec.structs.push(r#struct_ent);
        ctx.projections.structs[r#struct] = Some(s);
        s
    }

    fn fields(
        &self,
        fields: VSlice<Field>,
        ctx: &mut SweepCtx,
        interner: &mut Interner,
    ) -> VSlice<Field> {
        let fields = self.typec.fields[fields]
            .iter()
            .map(|&field| self.field(field, ctx, interner))
            .collect::<BumpVec<_>>();
        ctx.temp.typec.fields.bump(fields)
    }

    fn field(&self, field: Field, ctx: &mut SweepCtx, interner: &mut Interner) -> Field {
        let mut field_ent @ Field {
            name: _,
            ty,
            span: _,
            vis: _,
            flags: _,
        } = field;

        field_ent.ty = self.ty(ty, ctx, interner);

        field_ent
    }

    fn r#enum(
        &self,
        r#enum: VRef<Enum>,
        ctx: &mut SweepCtx,
        interner: &mut Interner,
    ) -> VRef<Enum> {
        if let Some(r#enum) = ctx.projections.enums[r#enum] {
            return r#enum;
        }

        let mut r#enum_ent @ Enum {
            name: _,
            generics,
            loc: _,
            variants,
        } = self.typec.enums[r#enum];

        r#enum_ent.generics = self.generics(generics, ctx, interner);
        r#enum_ent.variants = self.variants(variants, ctx, interner);

        let e = ctx.temp.typec.enums.push(r#enum_ent);
        ctx.projections.enums[r#enum] = Some(e);
        e
    }

    fn variants(
        &self,
        variants: VSlice<Variant>,
        ctx: &mut SweepCtx,
        interner: &mut Interner,
    ) -> VSlice<Variant> {
        let variants = self.typec.variants[variants]
            .iter()
            .map(|&variant| self.variant(variant, ctx, interner))
            .collect::<BumpVec<_>>();
        ctx.temp.typec.variants.bump(variants)
    }

    fn variant(&self, variant: Variant, ctx: &mut SweepCtx, interner: &mut Interner) -> Variant {
        let mut variant_ent @ Variant {
            name: _,
            ty,
            span: _,
        } = variant;

        variant_ent.ty = self.ty(ty, ctx, interner);

        variant_ent
    }

    fn instance(
        &self,
        instance_ty: VRef<Instance>,
        ctx: &mut SweepCtx,
        interner: &mut Interner,
    ) -> VRef<Instance> {
        let Instance { base, args } = self.typec.instances[instance_ty];
        let base = self.generic_ty(base, ctx, interner);
        let args = self.ty_args_bump(args, ctx, interner);
        ctx.temp.typec.instance(base, &args, interner)
    }

    fn pointer(
        &self,
        pointer_ty: VRef<Pointer>,
        ctx: &mut SweepCtx,
        interner: &mut Interner,
    ) -> VRef<Pointer> {
        let Pointer {
            base,
            mutability,
            depth: _,
        } = self.typec.pointers[pointer_ty];
        let base = self.ty(base, ctx, interner);
        ctx.temp.typec.pointer_to(mutability, base, interner)
    }

    fn generic_ty(
        &self,
        generic_ty: GenericTy,
        ctx: &mut SweepCtx,
        interner: &mut Interner,
    ) -> GenericTy {
        match generic_ty {
            GenericTy::Struct(s) => GenericTy::Struct(self.r#struct(s, ctx, interner)),
            GenericTy::Enum(e) => GenericTy::Enum(self.r#enum(e, ctx, interner)),
        }
    }

    pub fn clear(&mut self) {
        self.typec.clear();
        self.mir.clear();
        self.gen.clear();
    }
}

#[derive(Default)]
pub struct SweepCtx {
    temp: Incremental,
    projections: SweepProjections,
}

impl SweepCtx {
    fn clear(&mut self) {
        self.temp.clear();
        self.projections.clear();
    }
}

#[derive(Default)]
struct SweepProjections {
    pub structs: ShadowMap<Struct, OptVRef<Struct>>,
    pub base_specs: ShadowMap<SpecBase, OptVRef<SpecBase>>,
    pub funcs: ShadowMap<Func, OptVRef<Func>>,
    pub impls: ShadowMap<Impl, OptVRef<Impl>>,
    pub enums: ShadowMap<Enum, OptVRef<Enum>>,
}

impl SweepProjections {
    fn clear(&mut self) {
        self.structs.clear();
        self.base_specs.clear();
        self.funcs.clear();
        self.impls.clear();
        self.enums.clear();
    }
}
