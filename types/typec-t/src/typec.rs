use core::fmt;
use std::fmt::Write;
use std::mem;
use std::{
    collections::hash_map::Entry,
    default::default,
    iter,
    ops::{Index, IndexMut},
};

use crate::*;
use packaging_t::Module;
use storage::*;

#[derive(Default)]
pub struct Typec {
    lookup: TypecLookup,
    pub structs: Structs,
    pub pointers: Pointers,
    pub instances: Instances,
    pub base_specs: BaseSpecs,
    pub spec_instances: SpecInstances,
    pub funcs: Funcs,
    pub fields: Fields,
    pub impls: Impls,
    pub params: ParamSlices,
    pub spec_sums: SpecSums,
    pub args: ArgSlices,
    pub impl_lookup: ImplLookup,
    pub func_slices: FuncSlices,
    pub spec_funcs: SpecFuncs,
    pub builtin_funcs: Vec<VRef<Func>>,
    pub module_items: ShadowMap<Module, PushMap<ModuleItem>>,
}

macro_rules! gen_index {
    (
        indexes {
            $($ty:ty => $storage:ident)*
        }
        slices {
            $($slice_ty:ty => $slice_storage:ident)*
        }
    ) => {
        $(
            impl Index<VRef<$ty>> for Typec {
                type Output = $ty;

                fn index(&self, index: VRef<$ty>) -> &Self::Output {
                    &self.$storage[index]
                }
            }

            impl IndexMut<VRef<$ty>> for Typec {
                fn index_mut(&mut self, index: VRef<$ty>) -> &mut Self::Output {
                    &mut self.$storage[index]
                }
            }
        )*

        $(
            impl Index<VSlice<$slice_ty>> for Typec {
                type Output = [$slice_ty];

                fn index(&self, index: VSlice<$slice_ty>) -> &Self::Output {
                    &self.$slice_storage[index]
                }
            }

            impl IndexMut<VSlice<$slice_ty>> for Typec {
                fn index_mut(&mut self, index: VSlice<$slice_ty>) -> &mut Self::Output {
                    &mut self.$slice_storage[index]
                }
            }
        )*
    };
}

gen_index! {
    indexes {
        Struct => structs
        Pointer => pointers
        SpecBase => base_specs
        SpecInstance => spec_instances
        Func => funcs
        Field => fields
        Instance => instances
    }
    slices {
        Field => fields
        VSlice<Spec> => params
        Spec => spec_sums
        Ty => args
        VRef<Func> => func_slices
        SpecFunc => spec_funcs
    }
}

impl Typec {
    pub fn display_sig(
        &self,
        func: VRef<Func>,
        interner: &Interner,
        buffer: &mut String,
    ) -> fmt::Result {
        let Func {
            signature,
            generics,
            upper_generics,
            ..
        } = self.funcs[func];
        write!(
            buffer,
            "fn {}[{}] {}({}) -> {} ",
            signature
                .cc
                .map(|cc| &interner[cc])
                .map_or(default(), |cc| format!("\"{}\" ", cc)),
            self[upper_generics]
                .iter()
                .chain(&self[generics])
                .map(|&ty| self.display_spec_sum(ty, interner))
                .intersperse(", ".into())
                .collect::<String>(),
            &interner[self[func].name],
            self[signature.args]
                .iter()
                .map(|&ty| self.display_ty(ty, interner))
                .enumerate()
                .map(|(i, str)| format!("var{}: {}", i, str))
                .intersperse(", ".into())
                .collect::<String>(),
            self.display_ty(signature.ret, interner),
        )
    }

    pub fn func_name(&self, func: VRef<Func>, to: &mut String, interner: &Interner) {
        let Func {
            name, loc, owner, ..
        } = self.funcs[func];
        if let Some(loc) = loc {
            write!(to, "{}\\", loc.module.index()).unwrap();
        }
        if let Some(owner) = owner {
            write!(to, "{}\\", owner).unwrap();
        }
        to.push_str(&interner[name]);
    }

    pub fn display_ty(&self, ty: Ty, interner: &Interner) -> String {
        let mut str = String::new();
        self.display_ty_to(ty, &mut str, interner);
        str
    }

    pub fn display_ty_to(&self, ty: Ty, to: &mut String, interner: &Interner) {
        match ty {
            Ty::Struct(r#struct) => {
                write!(to, "{}\\", self[r#struct].loc.module.index()).unwrap();
                to.push_str(&interner[self[r#struct].name])
            }
            Ty::Instance(instance) => {
                let Instance { base, args } = self[instance];
                self.instance_id(base, &self[args], to, interner);
            }
            Ty::Pointer(ptr) => {
                let Pointer {
                    base, mutability, ..
                } = self[ptr];
                self.pointer_id(mutability, base, to, interner);
            }
            Ty::Param(i) => write!(to, "param{}", i).unwrap(),
            Ty::Builtin(b) => to.push_str(b.name()),
        }
    }

    pub fn display_spec(&self, spec: Spec, interner: &Interner) -> String {
        let mut str = String::new();
        self.display_spec_to(spec, &mut str, interner);
        str
    }

    pub fn display_spec_to(&self, spec: Spec, to: &mut String, interner: &Interner) {
        match spec {
            Spec::Base(base) => to.push_str(&interner[self[base].name]),
            Spec::Instance(instance) => {
                let SpecInstance { base, args } = self[instance];
                self.spec_instance_id(base, &self[args], to, interner)
            }
        }
    }

    pub fn display_spec_sum(&self, spec: VSlice<Spec>, interner: &Interner) -> String {
        let mut str = String::new();
        self.display_spec_sum_to(spec, &mut str, interner);
        str
    }

    pub fn display_spec_sum_to(&self, spec: VSlice<Spec>, to: &mut String, interner: &Interner) {
        if let Some((&last, spec)) = self[spec].split_last() {
            for &spec in spec {
                self.display_spec_to(spec, to, interner);
                to.push_str(" + ");
            }
            self.display_spec_to(last, to, interner);
        }
    }

    pub fn type_diff(&self, pattern: Ty, value: Ty, interner: &Interner) -> String {
        let mut buffer = String::new();
        self.type_diff_recurse(pattern, value, &mut buffer, interner);
        buffer
    }

    fn type_diff_recurse(&self, pattern: Ty, value: Ty, to: &mut String, interner: &Interner) {
        if pattern == value {
            to.push('_');
            return;
        }

        match (pattern, value) {
            (Ty::Pointer(pattern), Ty::Pointer(value)) => {
                to.push('^');
                if self[pattern].mutability != self[value].mutability {
                    write!(to, "{}", self[pattern].mutability).unwrap();
                }
                self.type_diff_recurse(self[pattern].base, self[value].base, to, interner);
            }
            (Ty::Instance(pattern), Ty::Instance(value)) => {
                self.type_diff_recurse(
                    self[pattern].base.as_ty(),
                    self[value].base.as_ty(),
                    to,
                    interner,
                );
                let Some((&pattern_first, pattern_others)) = self[self[pattern].args].split_first() else {
                    return;
                };
                let Some((&value_first, value_others)) = self[self[value].args].split_first() else {
                    return;
                };

                to.push('[');
                self.type_diff_recurse(pattern_first, value_first, to, interner);
                for (&pattern, &value) in pattern_others.iter().zip(value_others) {
                    to.push_str(", ");
                    self.type_diff_recurse(pattern, value, to, interner);
                }
                to.push(']');
            }
            _ => self.display_ty_to(pattern, to, interner),
        }
    }

    pub fn contains_params(&self, ty: Ty) -> bool {
        match ty {
            Ty::Instance(instance) => self[self[instance].args]
                .iter()
                .any(|&ty| self.contains_params(ty)),
            Ty::Pointer(pointer) => {
                self.contains_params(self[pointer].base)
                    || matches!(self[pointer].mutability, Mutability::Param(..))
            }
            Ty::Param(..) => true,
            Ty::Struct(..) | Ty::Builtin(..) => false,
        }
    }

    pub fn init(&mut self, interner: &mut Interner) {
        self.init_builtin_funcs(interner);
    }

    fn init_builtin_funcs(&mut self, interner: &mut Interner) {
        self.funcs.push(Default::default());

        let int_bin_ops = "+ - / *".split_whitespace();

        for op in int_bin_ops {
            for ty in Ty::INTEGERS {
                let op = interner.intern(op);
                let id = interner.intern_with(|s, t| self.binary_op_id(op, ty, ty, t, s));

                let signature = Signature {
                    cc: default(),
                    args: self.args.bump([ty, ty]),
                    ret: ty,
                };

                let func = self.funcs.push(Func {
                    signature,
                    flags: FuncFlags::BUILTIN,
                    name: id,
                    ..default()
                });

                self.builtin_funcs.push(func);
            }
        }
    }

    pub fn pack_func_param_specs(
        &self,
        func: VRef<Func>,
    ) -> impl Iterator<Item = VSlice<Spec>> + '_ {
        let Func {
            generics,
            upper_generics,
            ..
        } = self[func];
        iter::empty()
            .chain(&self[upper_generics])
            .chain(&self[generics])
            .copied()
    }

    pub fn pack_spec_func_param_specs(
        &self,
        func: SpecFunc,
    ) -> impl Iterator<Item = VSlice<Spec>> + '_ {
        let SpecFunc {
            generics, parent, ..
        } = func;
        let SpecBase {
            generics: upper_generics,
            ..
        } = self[parent];
        iter::empty()
            .chain(self[upper_generics].iter().copied())
            .chain(iter::once(VSlice::empty()))
            .chain(self[generics].iter().copied())
    }

    pub fn binary_op_id(
        &self,
        op: VRef<str>,
        lhs: Ty,
        rhs: Ty,
        to: &mut String,
        interner: &Interner,
    ) {
        self.display_ty_to(lhs, to, interner);
        to.push(' ');
        to.push_str(&interner[op]);
        to.push(' ');
        self.display_ty_to(rhs, to, interner);
    }

    pub fn pointer_to(
        &mut self,
        mutability: Mutability,
        base: Ty,
        interner: &mut Interner,
    ) -> VRef<Pointer> {
        let id = interner.intern_with(|s, t| self.pointer_id(mutability, base, t, s));
        let depth = base.ptr_depth(self) + 1;

        match self.lookup.entry(id) {
            Entry::Occupied(occ) => match occ.get() {
                &ComputedTypecItem::Pointer(pointer) => pointer,
                _ => unreachable!(),
            },
            Entry::Vacant(entry) => {
                let pointer = Pointer {
                    mutability,
                    base,
                    depth,
                };
                let ptr = self.pointers.push(pointer);
                entry.insert(ComputedTypecItem::Pointer(ptr));
                ptr
            }
        }
    }

    pub fn pointer_id(
        &self,
        mutability: Mutability,
        base: Ty,
        to: &mut String,
        interner: &Interner,
    ) {
        to.push('^');
        write!(to, "{}", mutability).unwrap();
        self.display_ty_to(base, to, interner);
    }

    pub fn instance_id<'a>(
        &'a self,
        base: GenericTy,
        params: &'a [Ty],
        to: &'a mut String,
        interner: &'a Interner,
    ) {
        self.display_ty_to(base.as_ty(), to, interner);
        to.push('[');
        if let Some((&last, params)) = params.split_last() {
            for &param in params {
                self.display_ty_to(param, to, interner);
                to.push_str(", ");
            }
            self.display_ty_to(last, to, interner);
        }
        to.push(']');
    }

    pub fn instance(
        &mut self,
        base: GenericTy,
        args: &[Ty],
        interner: &mut Interner,
    ) -> VRef<Instance> {
        let id = interner.intern_with(|s, t| self.instance_id(base, args, t, s));
        match self.lookup.entry(id) {
            Entry::Occupied(occ) => match occ.get() {
                &ComputedTypecItem::Instance(instance) => instance,
                _ => unreachable!(),
            },
            Entry::Vacant(entry) => {
                let instance = Instance {
                    base,
                    args: self.args.bump_slice(args),
                };
                let instance = self.instances.push(instance);
                entry.insert(ComputedTypecItem::Instance(instance));
                instance
            }
        }
    }

    pub fn spec_instance(
        &mut self,
        base: VRef<SpecBase>,
        args: &[Ty],
        interner: &mut Interner,
    ) -> VRef<SpecInstance> {
        let id = interner.intern_with(|s, t| self.spec_instance_id(base, args, t, s));
        match self.lookup.entry(id) {
            Entry::Occupied(occ) => match occ.get() {
                &ComputedTypecItem::SpecInstance(instance) => instance,
                _ => unreachable!(),
            },
            Entry::Vacant(entry) => {
                let instance = SpecInstance {
                    base,
                    args: self.args.bump_slice(args),
                };
                let instance = self.spec_instances.push(instance);
                entry.insert(ComputedTypecItem::SpecInstance(instance));
                instance
            }
        }
    }

    pub fn spec_instance_id(
        &self,
        base: VRef<SpecBase>,
        args: &[Ty],
        to: &mut String,
        interner: &Interner,
    ) {
        self.display_spec_to(Spec::Base(base), to, interner);
        to.push('[');
        if let Some((&last, args)) = args.split_last() {
            for &arg in args {
                self.display_ty_to(arg, to, interner);
                to.push_str(", ");
            }
            self.display_ty_to(last, to, interner);
        }
        to.push(']');
    }

    pub fn tuple_id(&self, tys: &[Ty], to: &mut String, interner: &Interner) {
        to.push('(');
        if let Some((&last, tys)) = tys.split_last() {
            for &ty in tys {
                self.display_ty_to(ty, to, interner);
                to.push_str(", ");
            }
            self.display_ty_to(last, to, interner);
        }
        to.push(')');
    }

    pub fn implements_sum(&mut self, ty: Ty, sum: VSlice<Spec>, params: &[VSlice<Spec>]) -> bool {
        self[sum]
            .to_bumpvec()
            .into_iter()
            .all(|spec| self.find_implementation(ty, spec, params).is_some())
    }

    pub fn deref(&self, ty: Ty) -> Ty {
        match ty {
            Ty::Pointer(ptr) => self[ptr].base,
            _ => ty,
        }
    }

    pub fn find_implementation(
        &mut self,
        ty: Ty,
        spec: Spec,
        params: &[VSlice<Spec>],
    ) -> Option<Option<VRef<Impl>>> {
        if let Ty::Param(index) = ty {
            let specs = params[index as usize];
            return self[specs].contains(&spec).then_some(None);
        }

        let key = ImplKey { ty, spec };

        if let Some(&result) = self.impl_lookup.get(&key) {
            return Some(result);
        }

        let ty_base = match ty {
            Ty::Instance(instance) => self[instance].base.as_ty(),
            _ => ty,
        };
        let spec_base = match spec {
            Spec::Instance(instance) => Spec::Base(self[instance].base),
            _ => spec,
        };

        let base_key = ImplKey {
            ty: ty_base,
            spec: spec_base,
        };

        let Some(&(mut base_impl)) = self.impl_lookup.get(&base_key) else {
            self.impl_lookup.insert(key, None);
            return None;
        };

        while let Some(current) = base_impl {
            let impl_ent = self.impls[current];
            base_impl = impl_ent.next;

            let generics = self[impl_ent.generics].to_bumpvec();
            let mut generic_slots = bumpvec![None; generics.len()];
            let spec_compatible = self.compatible(&mut generic_slots, ty, impl_ent.key.ty);
            let ty_compatible = self.compatible_spec(&mut generic_slots, spec, impl_ent.key.spec);

            if ty_compatible.is_err() || spec_compatible.is_err() {
                continue;
            }

            let implements = generics
                .into_iter()
                .zip(generic_slots.into_iter().map(|s| s.unwrap()))
                .all(|(specs, ty)| self.implements_sum(ty, specs, params));

            if !implements {
                continue;
            }

            self.impl_lookup.insert(key, Some(current));
            return Some(Some(current));
        }

        self.impl_lookup.insert(key, None);
        None
    }

    pub fn compatible_spec(
        &self,
        params: &mut [Option<Ty>],
        reference: Spec,
        template: Spec,
    ) -> Result<(), SpecCmpError> {
        match (reference, template) {
            _ if reference == template => Ok(()),
            (Spec::Instance(reference), Spec::Instance(template)) => {
                let reference = self[reference];
                let template = self[template];

                if reference.base != template.base {
                    return Err(SpecCmpError::Specs(
                        Spec::Base(reference.base),
                        Spec::Base(template.base),
                    ));
                }

                self[reference.args]
                    .iter()
                    .zip(&self[template.args])
                    .fold(Ok(()), |acc, (&reference, &template)| {
                        acc.and(self.compatible(params, reference, template))
                    })
                    .map_err(|(a, b)| SpecCmpError::Args(a, b))?;

                Ok(())
            }
            _ => Err(SpecCmpError::Specs(reference, template)),
        }
    }

    pub fn compatible(
        &self,
        params: &mut [Option<Ty>],
        reference: Ty,
        template: Ty,
    ) -> Result<(), (Ty, Ty)> {
        let mut stack = bumpvec![(reference, template)];

        let check = |a, b| Ty::compatible(a, b).then_some(()).ok_or((a, b));

        while let Some((reference, template)) = stack.pop() {
            if reference == template && !matches!(template, Ty::Param(..)) {
                continue;
            }

            match (reference, template) {
                (Ty::Pointer(reference), Ty::Pointer(template)) => {
                    stack.push((self[reference].base, self[template].base));
                }
                (Ty::Instance(reference), Ty::Instance(template)) => {
                    check(self[reference].base.as_ty(), self[template].base.as_ty())?;
                    stack.extend(
                        self[self[reference].args]
                            .iter()
                            .copied()
                            .zip(self[self[template].args].iter().copied()),
                    );
                }
                (_, Ty::Param(index)) if let Some(inferred) = params[index as usize] => {
                    check(inferred, reference)?;
                }
                (_, Ty::Param(index)) => params[index as usize] = Some(reference),
                _ => return Err((reference, template)),
            }
        }

        Ok(())
    }

    pub fn instantiate(&mut self, ty: Ty, params: &[Ty], interner: &mut Interner) -> Ty {
        unsafe {
            self.instantiate_low(ty, mem::transmute(params), interner)
                .unwrap_unchecked()
        }
    }

    pub fn try_instantiate(
        &mut self,
        ty: Ty,
        params: &[Option<Ty>],
        interner: &mut Interner,
    ) -> Option<Ty> {
        self.instantiate_low(ty, params, interner)
    }

    pub fn instantiate_low(
        &mut self,
        ty: Ty,
        params: &[Option<Ty>],
        interner: &mut Interner,
    ) -> Option<Ty> {
        Some(match ty {
            Ty::Instance(instance) => {
                let Instance { base, args } = self[instance];
                let args = self[args]
                    .to_bumpvec()
                    .into_iter()
                    .map(|arg| self.instantiate_low(arg, params, interner))
                    .collect::<Option<BumpVec<_>>>()?;
                Ty::Instance(self.instance(base, args.as_slice(), interner))
            }
            Ty::Pointer(pointer) => {
                let Pointer {
                    base, mutability, ..
                } = self[pointer];
                let base = self.instantiate_low(base, params, interner)?;
                Ty::Pointer(self.pointer_to(mutability, base, interner))
            }
            Ty::Param(index) => return params[index as usize],
            Ty::Struct(..) | Ty::Builtin(_) => ty,
        })
    }
}

pub enum SpecCmpError {
    Specs(Spec, Spec),
    Args(Ty, Ty),
}

#[derive(Clone, Copy)]
pub struct Loc {
    pub module: VRef<Module>,
    pub item: VRef<ModuleItem>,
}
