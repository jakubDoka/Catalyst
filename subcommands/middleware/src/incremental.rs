// use std::{default::default, mem};

// use gen::*;
// use mir_t::*;
// use packaging_t::*;

// use storage::*;
// use typec_t::*;

// use crate::Task;

// #[derive(Default)]
// pub struct Incremental {
//     pub interner: Interner,
//     pub typec: Typec,
//     pub mir: Mir,
//     pub gen: Gen,
//     pub sources: PoolMap<Source>,
//     pub loaded: bool,
// }

// impl Incremental {
//     pub fn clear(&mut self) {
//         self.typec.clear();
//         self.mir.clear();
//         self.gen.clear();
//         self.sources.clear();
//         self.loaded = false;
//     }
// }

// pub struct IncrementalBorrow<'ctx> {
//     pub typec: &'ctx mut Typec,
//     pub mir: &'ctx mut Mir,
//     pub gen: &'ctx mut Gen,
// }

// impl<'ctx> IncrementalBorrow<'ctx> {
//     pub fn from_incremental(incremental: &'ctx mut Incremental) -> Self {
//         Self {
//             typec: &mut incremental.typec,
//             mir: &mut incremental.mir,
//             gen: &mut incremental.gen,
//         }
//     }

//     pub fn from_task(task: &'ctx mut Task) -> Self {
//         Self {
//             typec: &mut task.typec,
//             mir: &mut task.mir,
//             gen: &mut task.gen,
//         }
//     }

//     pub fn sweep(
//         &mut self,
//         to_sweep: &BitSet,
//         ctx: &mut SweepCtx,
//         interner: &mut InternerTransfer,
//     ) {
//         ctx.clear();
//         self.module_items(to_sweep, ctx, interner);
//         self.impl_lookup(ctx, interner);
//         self.mir_bodies(ctx, interner);
//         self.compiled_funcs(ctx, interner);
//     }

//     fn module_items(&self, to_sweep: &BitSet, ctx: &mut SweepCtx, interner: &mut InternerTransfer) {
//         let mut modules = mem::take(&mut ctx.temp.typec.module_items);
//         for (key, module) in self.typec.module_items.iter() {
//             if !to_sweep.contains(key.index()) {
//                 continue;
//             }

//             let target_module = &mut modules[key].items;
//             target_module.clear();
//             for mut item in module.items.values().copied() {
//                 self.item(&mut item, ctx, interner);
//                 target_module.push(item);
//             }
//         }
//         ctx.temp.typec.module_items = modules;
//     }

//     fn impl_lookup(&mut self, ctx: &mut SweepCtx, interner: &mut InternerTransfer) {
//         for (&key, &r#impl) in &self.typec.impl_lookup {
//             let ty_exists = match key.ty.caller(self.typec) {
//                 Ty::Struct(s) => ctx.projections.structs[s].is_some(),
//                 Ty::Enum(e) => ctx.projections.enums[e].is_some(),
//                 _ => false,
//             };
//             let spec_exists = ctx.projections.base_specs[key.spec.base(self.typec)].is_some();

//             if !ty_exists || !spec_exists {
//                 continue;
//             }

//             let r#impl = self.r#impl(r#impl, ctx, interner);
//             ctx.temp
//                 .typec
//                 .impl_lookup
//                 .insert(self.typec.impls[r#impl].key, r#impl);
//         }
//         self.typec.impl_lookup.clear();
//     }

//     fn mir_bodies(&mut self, ctx: &mut SweepCtx, interner: &mut InternerTransfer) {
//         let mut bodies = mem::take(&mut self.mir.bodies);
//         for (key, mut func) in bodies.drain() {
//             let Some(proj) = ctx.projections.funcs[key] else {
//                 continue;
//             };

//             func.dependant_types
//                 .values_mut()
//                 .for_each(|ty| ty.ty = self.ty(ty.ty, ctx, interner));

//             ctx.temp.mir.bodies.insert(proj, func);
//         }
//         self.mir.bodies = bodies;
//     }

//     fn compiled_funcs(&mut self, ctx: &mut SweepCtx, interner: &mut InternerTransfer) {
//         let mut compiled_funcs = mem::take(&mut self.gen.compiled_funcs);
//         for (&key, func) in compiled_funcs.iter() {
//             // TODO: This may cause some compiled functions to be
//             // ignored and recompiled multiple times
//             let Some(proj) = ctx.projections.funcs[func.func] else {
//                 continue;
//             };

//             let func = CompiledFunc {
//                 func: proj,
//                 ..func.clone()
//             };

//             let key = interner.transfer(key);
//             ctx.temp.gen.compiled_funcs.insert(key, func);
//         }
//         compiled_funcs.clear();
//         self.gen.compiled_funcs = compiled_funcs;
//     }

//     fn r#impl(
//         &self,
//         r#impl: FragRef<Impl>,
//         ctx: &mut SweepCtx,
//         interner: &mut InternerTransfer,
//     ) -> FragRef<Impl> {
//         if let Some(r#impl) = ctx.projections.impls[r#impl] {
//             return r#impl;
//         }

//         let Impl {
//             generics,
//             key,
//             methods,
//             next,
//             span,
//         } = self.typec.impls[r#impl];

//         let ent = Impl {
//             generics: self.generics(generics, ctx, interner),
//             key: ImplKey {
//                 ty: self.ty(key.ty, ctx, interner),
//                 spec: self.spec(key.spec, ctx, interner),
//             },
//             methods: self.func_slice(methods, ctx, interner),
//             next: next.map(|next| self.r#impl(next, ctx, interner)),
//             span,
//         };

//         let i = ctx.temp.typec.impls.push(ent);
//         ctx.projections.impls[r#impl] = Some(i);
//         i
//     }

//     fn item(&self, item: &mut ModuleItem, ctx: &mut SweepCtx, interner: &mut InternerTransfer) {
//         match item.ptr {
//             ModuleItemPtr::Func(ref mut func) => *func = self.func(*func, ctx, interner),
//             ModuleItemPtr::Ty(ref mut ty) => *ty = self.ty(*ty, ctx, interner),
//             ModuleItemPtr::SpecBase(ref mut spec) => *spec = self.spec_base(*spec, ctx, interner),
//         }
//     }

//     fn func(
//         &self,
//         func: FragRef<Func>,
//         ctx: &mut SweepCtx,
//         interner: &mut InternerTransfer,
//     ) -> FragRef<Func> {
//         if let Some(func) = ctx.projections.funcs[func] {
//             return func;
//         }

//         let mut func_ent @ Func {
//             generics,
//             owner,
//             upper_generics,
//             signature,
//             flags: _,
//             visibility: _,
//             name,
//             loc: _,
//         } = self.typec.funcs[func];

//         func_ent.generics = self.generics(generics, ctx, interner);
//         func_ent.owner = owner.map(|owner| self.ty(owner, ctx, interner));
//         func_ent.upper_generics = self.generics(upper_generics, ctx, interner);
//         func_ent.signature = self.signature(signature, ctx, interner);
//         func_ent.name = interner.transfer(name);

//         let f = ctx.temp.typec.funcs.push(func_ent);
//         ctx.projections.funcs[func] = Some(f);
//         f
//     }

//     fn generics(
//         &self,
//         generics: Generics,
//         ctx: &mut SweepCtx,
//         interner: &mut InternerTransfer,
//     ) -> Generics {
//         let generics = self.typec.params[generics]
//             .iter()
//             .map(|&generic| self.spec_sum(generic, ctx, interner))
//             .collect::<BumpVec<_>>();
//         ctx.temp.typec.params.bump(generics)
//     }

//     fn spec_sum(
//         &self,
//         spec_sum: FragSlice<Spec>,
//         ctx: &mut SweepCtx,
//         interner: &mut InternerTransfer,
//     ) -> FragSlice<Spec> {
//         let specs = self.typec.spec_sums[spec_sum]
//             .iter()
//             .map(|&spec| self.spec(spec, ctx, interner))
//             .collect::<BumpVec<_>>();
//         ctx.temp.typec.spec_sums.bump(specs)
//     }

//     fn spec(&self, spec: Spec, ctx: &mut SweepCtx, interner: &mut InternerTransfer) -> Spec {
//         match spec {
//             Spec::Base(base) => Spec::Base(self.spec_base(base, ctx, interner)),
//             Spec::Instance(instance) => Spec::Instance(self.spec_instance(instance, ctx, interner)),
//         }
//     }

//     fn spec_base(
//         &self,
//         spec_base: FragRef<SpecBase>,
//         ctx: &mut SweepCtx,
//         interner: &mut InternerTransfer,
//     ) -> FragRef<SpecBase> {
//         if let Some(spec_base) = ctx.projections.base_specs[spec_base] {
//             return spec_base;
//         }

//         let SpecBase {
//             name,
//             generics,
//             loc,
//             methods,
//         } = self.typec.base_specs[spec_base];

//         let s = ctx.temp.typec.base_specs.push(default());
//         ctx.projections.base_specs[spec_base] = Some(s);
//         let spec_base_ent = SpecBase {
//             name: interner.transfer(name),
//             generics: self.generics(generics, ctx, interner),
//             methods: self.spec_funcs(methods, ctx, interner),
//             loc,
//         };

//         ctx.temp.typec.base_specs[s] = spec_base_ent;
//         s
//     }

//     fn spec_funcs(
//         &self,
//         spec_methods: FragSlice<SpecFunc>,
//         ctx: &mut SweepCtx,
//         interner: &mut InternerTransfer,
//     ) -> FragSlice<SpecFunc> {
//         let spec_methods = self.typec.spec_funcs[spec_methods]
//             .iter()
//             .map(|&func| self.spec_func(func, ctx, interner))
//             .collect::<BumpVec<_>>();
//         ctx.temp.typec.spec_funcs.bump(spec_methods)
//     }

//     fn spec_func(
//         &self,
//         spec_func: SpecFunc,
//         ctx: &mut SweepCtx,
//         interner: &mut InternerTransfer,
//     ) -> SpecFunc {
//         let SpecFunc {
//             name,
//             generics,
//             signature,
//             span,
//             parent,
//         } = spec_func;

//         SpecFunc {
//             name: interner.transfer(name),
//             generics: self.generics(generics, ctx, interner),
//             signature: self.signature(signature, ctx, interner),
//             span,
//             parent: self.spec_base(parent, ctx, interner),
//         }
//     }

//     fn signature(
//         &self,
//         signature: Signature,
//         ctx: &mut SweepCtx,
//         interner: &mut InternerTransfer,
//     ) -> Signature {
//         let Signature { cc, args, ret } = signature;

//         Signature {
//             cc,
//             args: self.ty_args(args, ctx, interner),
//             ret: self.ty(ret, ctx, interner),
//         }
//     }

//     fn spec_instance(
//         &self,
//         spec_instance: FragRef<SpecInstance>,
//         ctx: &mut SweepCtx,
//         interner: &mut InternerTransfer,
//     ) -> FragRef<SpecInstance> {
//         let SpecInstance { base, args } = self.typec.spec_instances[spec_instance];
//         let base = self.spec_base(base, ctx, interner);
//         let args = self.ty_args_bump(args, ctx, interner);
//         ctx.temp.typec.spec_instance(base, &args, interner.dest())
//     }

//     fn ty_args(
//         &self,
//         args: FragSlice<Ty>,
//         ctx: &mut SweepCtx,
//         interner: &mut InternerTransfer,
//     ) -> FragSlice<Ty> {
//         let args = self.ty_args_bump(args, ctx, interner);
//         ctx.temp.typec.args.bump(args)
//     }

//     fn ty_args_bump(
//         &self,
//         args: FragSlice<Ty>,
//         ctx: &mut SweepCtx,
//         interner: &mut InternerTransfer,
//     ) -> BumpVec<Ty> {
//         self.typec.args[args]
//             .iter()
//             .map(|&arg| self.ty(arg, ctx, interner))
//             .collect::<BumpVec<_>>()
//     }

//     fn func_slice(
//         &self,
//         funcs: VRefSlice<Func>,
//         ctx: &mut SweepCtx,
//         interner: &mut InternerTransfer,
//     ) -> VRefSlice<Func> {
//         let funcs = self.typec.func_slices[funcs]
//             .iter()
//             .map(|&func| self.func(func, ctx, interner))
//             .collect::<BumpVec<_>>();
//         ctx.temp.typec.func_slices.bump(funcs)
//     }

//     fn ty(&self, ty: Ty, ctx: &mut SweepCtx, interner: &mut InternerTransfer) -> Ty {
//         dbg!(ty);
//         match ty {
//             Ty::Struct(s) => Ty::Struct(self.r#struct(s, ctx, interner)),
//             Ty::Enum(e) => Ty::Enum(self.r#enum(e, ctx, interner)),
//             Ty::Instance(i) => Ty::Instance(self.instance(i, ctx, interner)),
//             Ty::Pointer(p) => Ty::Pointer(self.pointer(p, ctx, interner)),
//             as_is @ (Ty::Param(..) | Ty::Builtin(..)) => as_is,
//         }
//     }

//     fn r#struct(
//         &self,
//         r#struct: FragRef<Struct>,
//         ctx: &mut SweepCtx,
//         interner: &mut InternerTransfer,
//     ) -> FragRef<Struct> {
//         if let Some(r#struct) = ctx.projections.structs[r#struct] {
//             return r#struct;
//         }

//         let Struct {
//             name,
//             generics,
//             loc,
//             fields,
//         } = self.typec.structs[r#struct];

//         let struct_ent = Struct {
//             name: interner.transfer(name),
//             generics: self.generics(generics, ctx, interner),
//             fields: self.fields(fields, ctx, interner),
//             loc,
//         };

//         let s = ctx.temp.typec.structs.push(struct_ent);
//         ctx.projections.structs[r#struct] = Some(s);
//         s
//     }

//     fn fields(
//         &self,
//         fields: FragSlice<Field>,
//         ctx: &mut SweepCtx,
//         interner: &mut InternerTransfer,
//     ) -> FragSlice<Field> {
//         let fields = self.typec.fields[fields]
//             .iter()
//             .map(|&field| self.field(field, ctx, interner))
//             .collect::<BumpVec<_>>();
//         ctx.temp.typec.fields.bump(fields)
//     }

//     fn field(&self, field: Field, ctx: &mut SweepCtx, interner: &mut InternerTransfer) -> Field {
//         let mut field_ent @ Field {
//             name,
//             ty,
//             span: _,
//             vis: _,
//             flags: _,
//         } = field;

//         field_ent.name = interner.transfer(name);
//         field_ent.ty = self.ty(ty, ctx, interner);

//         field_ent
//     }

//     fn r#enum(
//         &self,
//         r#enum: FragRef<Enum>,
//         ctx: &mut SweepCtx,
//         interner: &mut InternerTransfer,
//     ) -> FragRef<Enum> {
//         if let Some(r#enum) = ctx.projections.enums[r#enum] {
//             return r#enum;
//         }

//         let Enum {
//             name,
//             generics,
//             loc,
//             variants,
//         } = self.typec.enums[r#enum];

//         let enum_ent = Enum {
//             name: interner.transfer(name),
//             generics: self.generics(generics, ctx, interner),
//             variants: self.variants(variants, ctx, interner),
//             loc,
//         };

//         let e = ctx.temp.typec.enums.push(enum_ent);
//         ctx.projections.enums[r#enum] = Some(e);
//         e
//     }

//     fn variants(
//         &self,
//         variants: FragSlice<Variant>,
//         ctx: &mut SweepCtx,
//         interner: &mut InternerTransfer,
//     ) -> FragSlice<Variant> {
//         let variants = self.typec.variants[variants]
//             .iter()
//             .map(|&variant| self.variant(variant, ctx, interner))
//             .collect::<BumpVec<_>>();
//         ctx.temp.typec.variants.bump(variants)
//     }

//     fn variant(
//         &self,
//         variant: Variant,
//         ctx: &mut SweepCtx,
//         interner: &mut InternerTransfer,
//     ) -> Variant {
//         let Variant { name, ty, span } = variant;

//         Variant {
//             name: interner.transfer(name),
//             ty: self.ty(ty, ctx, interner),
//             span,
//         }
//     }

//     fn instance(
//         &self,
//         instance_ty: FragRef<Instance>,
//         ctx: &mut SweepCtx,
//         interner: &mut InternerTransfer,
//     ) -> FragRef<Instance> {
//         let Instance { base, args } = self.typec.instances[instance_ty];
//         let base = self.generic_ty(base, ctx, interner);
//         let args = self.ty_args_bump(args, ctx, interner);
//         ctx.temp.typec.instance(base, &args, interner.dest())
//     }

//     fn pointer(
//         &self,
//         pointer_ty: FragRef<Pointer>,
//         ctx: &mut SweepCtx,
//         interner: &mut InternerTransfer,
//     ) -> FragRef<Pointer> {
//         let Pointer {
//             base,
//             mutability,
//             depth: _,
//         } = self.typec.pointers[pointer_ty];
//         let base = self.ty(base, ctx, interner);
//         ctx.temp.typec.pointer_to(mutability, base, interner.dest())
//     }

//     fn generic_ty(
//         &self,
//         generic_ty: GenericTy,
//         ctx: &mut SweepCtx,
//         interner: &mut InternerTransfer,
//     ) -> GenericTy {
//         match generic_ty {
//             GenericTy::Struct(s) => GenericTy::Struct(self.r#struct(s, ctx, interner)),
//             GenericTy::Enum(e) => GenericTy::Enum(self.r#enum(e, ctx, interner)),
//         }
//     }
// }

// pub struct InternerTransfer<'i> {
//     from: &'i mut Interner,
//     to: &'i mut Interner,
// }

// impl<'i> InternerTransfer<'i> {
//     pub fn new(from: &'i mut Interner, to: &'i mut Interner) -> Self {
//         Self { from, to }
//     }

//     pub fn transfer(&mut self, ident: FragSlice<u8>) -> FragSlice<u8> {
//         self.to.intern(&self.from[ident])
//     }

//     pub fn dest(&mut self) -> &mut Interner {
//         self.to
//     }
// }

// pub struct SweepCtx<'ctx> {
//     pub temp: IncrementalBorrow<'ctx>,
//     pub projections: &'ctx mut SweepProjections,
// }

// impl<'ctx> SweepCtx<'ctx> {
//     fn clear(&mut self) {
//         self.projections.clear();
//     }
// }

// #[derive(Default)]
// pub struct SweepProjections {
//     pub structs: ShadowMap<Struct, OptFragRef<Struct>>,
//     pub base_specs: ShadowMap<SpecBase, OptFragRef<SpecBase>>,
//     pub funcs: ShadowMap<Func, OptFragRef<Func>>,
//     pub impls: ShadowMap<Impl, OptFragRef<Impl>>,
//     pub enums: ShadowMap<Enum, OptFragRef<Enum>>,
// }

// impl SweepProjections {
//     fn clear(&mut self) {
//         self.structs.clear();
//         self.base_specs.clear();
//         self.funcs.clear();
//         self.impls.clear();
//         self.enums.clear();
//     }
// }
