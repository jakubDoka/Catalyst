use core::fmt;
use std::mem;
use std::{
    default::default,
    iter,
    ops::{Index, IndexMut},
};
use std::{fmt::Write, sync::Arc};

use crate::*;
use packaging_t::Module;

use storage::{dashmap::mapref::entry::Entry, *};

macro_rules! gen_typec {
    ($self:ident $($name:ident: $ty:ty, $size:expr, $({ $($tt:tt)* })?)*) => {
        #[derive(Default, Clone)]
        pub struct Typec {
            pub lookup: TypecLookup,
            $(
                pub $name: FragMap<$ty, $size>,
            )*
            pub impl_lookup: ImplLookup,
            pub module_items: ShadowMap<Module, ModuleItems>,
            pub macros: CMap<Ty, MacroImpl>,
            pub builtin_funcs: Vec<FragRef<Func>>,
            pub may_need_drop: CMap<Ty, bool>,
        }

        #[derive(Default)]
        pub struct TypecRelocator {
            $(
                pub $name: FragRelocator<$ty>,
            )*
        }

        impl TypecRelocator {
            pub fn relocate(&mut $self, typec: &mut Typec) {
                $(
                    $self.$name.relocate(&mut typec.$name);
                    $({ $($tt)* })?
                )*
                $self.clean_maps(typec);
            }

            pub fn clear(&mut $self) {
                $(
                    $self.$name.clear();
                )*
            }
        }
    };
}

impl TypecRelocator {
    fn clean_maps(&self, typec: &mut Typec) {
        fn take_arc<T: Default>(arc: &mut Arc<T>) -> T {
            mem::take(Arc::get_mut(arc).expect("there should be no other refs to typec"))
        }

        typec.lookup.retain(|_, value| {
            let new_value = match *value {
                ComputedTypecItem::Pointer(p) => {
                    self.pointers.project(p).map(ComputedTypecItem::Pointer)
                }
                ComputedTypecItem::Instance(i) => {
                    self.instances.project(i).map(ComputedTypecItem::Instance)
                }
                ComputedTypecItem::SpecInstance(i) => self
                    .spec_instances
                    .project(i)
                    .map(ComputedTypecItem::SpecInstance),
                ComputedTypecItem::SpecSum(s) => {
                    Some(ComputedTypecItem::SpecSum(self.spec_sums.project_slice(s)))
                }
            };

            if let Some(new_v) = new_value {
                *value = new_v;
            }

            new_value.is_some()
        });

        typec.may_need_drop.clear();

        let old_map = take_arc(&mut typec.impl_lookup);
        for (key, (r#impl, params)) in old_map.into_iter() {
            let Some(key) = self.project_impl_key(key) else {
                continue;
            };
            if key.spec.base(typec) == SpecBase::DROP {
                typec.may_need_drop.insert(key.ty, true);
            }
            let Some(r#impl) = self.impls.project(r#impl) else {
                continue;
            };
            let params = self.args.project_slice(params);
            typec.impl_lookup.insert(key, (r#impl, params));
        }

        typec.module_items.clear();

        let old_map = take_arc(&mut typec.macros);
        for (key, r#macro) in old_map.into_iter() {
            let Some(key) = self.project_ty(key) else {
                continue;
            };
            let Some(r#macro) = self.project_macro_impl(r#macro) else {
                continue;
            };
            typec.macros.insert(key, r#macro);
        }

        for func in typec.builtin_funcs.iter_mut() {
            *func = self
                .funcs
                .project(*func)
                .expect("builtin funcs should be valid");
        }

        let old_map = take_arc(&mut typec.may_need_drop);
        for (key, value) in old_map.into_iter() {
            let Some(key) = self.project_ty(key) else {
                continue;
            };
            typec.may_need_drop.insert(key, value);
        }
    }

    fn project_macro_impl(
        &self,
        MacroImpl {
            name,
            r#impl,
            params,
        }: MacroImpl,
    ) -> Option<MacroImpl> {
        let params = self.args.project_slice(params);
        let r#impl = r#impl
            .map(|r#impl| self.impls.project(r#impl))
            .transpose()?;
        Some(MacroImpl {
            name,
            r#impl,
            params,
        })
    }

    fn project_impl_key(&self, ImplKey { ty, spec }: ImplKey) -> Option<ImplKey> {
        let ty = self.project_ty(ty)?;
        let spec = self.project_spec(spec)?;
        Some(ImplKey { ty, spec })
    }

    pub fn project_ty(&self, ty: Ty) -> Option<Ty> {
        match ty {
            Ty::Struct(s) => self.structs.project(s).map(Ty::Struct),
            Ty::Enum(e) => self.enums.project(e).map(Ty::Enum),
            Ty::Instance(i) => self.instances.project(i).map(Ty::Instance),
            Ty::Pointer(p) => self.pointers.project(p).map(Ty::Pointer),
            Ty::Param(index) => Some(Ty::Param(index)),
            Ty::Builtin(b) => Some(Ty::Builtin(b)),
        }
    }

    fn project_spec(&self, spec: Spec) -> Option<Spec> {
        match spec {
            Spec::Base(b) => self.base_specs.project(b).map(Spec::Base),
            Spec::Instance(i) => self.spec_instances.project(i).map(Spec::Instance),
        }
    }
}

gen_typec! {
    self
    structs: Struct, MAX_FRAGMENT_SIZE,
    pointers: Pointer, MAX_FRAGMENT_SIZE,
    instances: Instance, MAX_FRAGMENT_SIZE,
    base_specs: SpecBase, MAX_FRAGMENT_SIZE,
    spec_instances: SpecInstance, MAX_FRAGMENT_SIZE,
    funcs: Func, MAX_FRAGMENT_SIZE,
    fields: Field, MAX_FRAGMENT_SIZE,
    impls: Impl, MAX_FRAGMENT_SIZE,
    spec_sums: Spec, MAX_FRAGMENT_SIZE,
    params: FragSlice<Spec>, MAX_FRAGMENT_SIZE,
    args: Ty, MAX_FRAGMENT_SIZE,
    func_slices: FragRef<Func>, MAX_FRAGMENT_SIZE,
    spec_funcs: SpecFunc, MAX_FRAGMENT_SIZE,
    variants: Variant, MAX_FRAGMENT_SIZE,
    enums: Enum, MAX_FRAGMENT_SIZE,
}

macro_rules! gen_index {
    (
        $($ty:ty => $storage:ident)*
    ) => {
        $(
            impl Index<FragRef<$ty>> for Typec {
                type Output = $ty;

                fn index(&self, index: FragRef<$ty>) -> &Self::Output {
                    &self.$storage[index]
                }
            }

            impl IndexMut<FragRef<$ty>> for Typec {
                fn index_mut(&mut self, index: FragRef<$ty>) -> &mut Self::Output {
                    &mut self.$storage[index]
                }
            }

            impl Index<FragSlice<$ty>> for Typec {
                type Output = [$ty];

                fn index(&self, index: FragSlice<$ty>) -> &Self::Output {
                    &self.$storage[index]
                }
            }

            impl IndexMut<FragSlice<$ty>> for Typec {
                fn index_mut(&mut self, index: FragSlice<$ty>) -> &mut Self::Output {
                    &mut self.$storage[index]
                }
            }
        )*
    };
}

gen_index! {
    Struct => structs
    Pointer => pointers
    SpecBase => base_specs
    SpecInstance => spec_instances
    Func => funcs
    Instance => instances
    Enum => enums
    Impl => impls
    FragSlice<Spec> => params
    Spec => spec_sums
    Field => fields
    Ty => args
    FragRef<Func> => func_slices
    SpecFunc => spec_funcs
    Variant => variants
}

impl Typec {
    pub fn register_ty_generics(&self, ty: Ty, spec_set: &mut SpecSet) {
        self.register_ty_generics_low(ty, default(), spec_set);
    }

    pub fn register_ty_generics_low(
        &self,
        ty: Ty,
        generic: FragSlice<Spec>,
        spec_set: &mut SpecSet,
    ) {
        match ty {
            Ty::Param(index) => spec_set.extend(index as u32, self[generic].iter().copied()),
            Ty::Instance(i) => {
                let Instance { base, args } = self[i];
                let params = match base {
                    GenericTy::Struct(s) => self[s].generics,
                    GenericTy::Enum(e) => self[e].generics,
                };

                for (&param, &arg) in self[params].iter().zip(self[args].iter()) {
                    self.register_ty_generics_low(arg, param, spec_set);
                }
            }
            Ty::Pointer(p) => self.register_ty_generics_low(self[p].base, generic, spec_set),
            Ty::Struct(..) | Ty::Enum(..) | Ty::Builtin(..) => (),
        }
    }

    pub fn register_spec_generics(&self, spec: Spec, spec_set: &mut SpecSet) {
        match spec {
            Spec::Instance(i) => {
                let SpecInstance { base, args } = self[i];
                let params = self[base].generics;

                for (&param, &arg) in self[params].iter().zip(self[args].iter()) {
                    self.register_ty_generics_low(arg, param, spec_set);
                }
            }
            Spec::Base(..) => (),
        }
    }

    pub fn may_need_drop(&mut self, ty: Ty, interner: &mut Interner) -> bool {
        self.may_need_drop_low(ty, &[], interner).0
    }

    fn may_need_drop_low(
        &mut self,
        ty: Ty,
        params: &[Ty],
        interner: &mut Interner,
    ) -> (bool, bool) {
        match ty {
            Ty::Param(index) if let Some(&param) = params.get(index as usize) =>
                return self.may_need_drop_low(param, &[], interner),
            Ty::Param(..) => return (true, true),
            Ty::Pointer(..) | Ty::Builtin(..) => return (false, false),
            ty if let Some(is) = self.may_need_drop.get(&ty) => return (*is, false),
            Ty::Struct(..) |
            Ty::Enum(..) |
            Ty::Instance(..) => (),
        };
        // its split since map entry handle must be dropped.
        let (is, param) = match ty {
            Ty::Struct(s) => self[s]
                .fields
                .keys()
                .map(|f| {
                    let ty = self[f].ty;
                    self.may_need_drop_low(ty, params, interner)
                })
                .reduce(|(a, b), (c, d)| (a | c, b | d))
                .unwrap_or((false, false)),
            Ty::Enum(e) => self[e]
                .variants
                .keys()
                .map(|f| {
                    let ty = self[f].ty;
                    self.may_need_drop_low(ty, params, interner)
                })
                .reduce(|(a, b), (c, d)| (a | c, b | d))
                .unwrap_or((false, false)),
            Ty::Instance(i) => {
                let Instance { base, args } = self[i];
                let params = args
                    .keys()
                    .map(|key| self.instantiate(self[key], params, interner))
                    .collect::<BumpVec<_>>();
                self.may_need_drop_low(base.as_ty(), &params, interner)
            }
            Ty::Param(..) | Ty::Pointer(..) | Ty::Builtin(..) => unreachable!(),
        };
        if !param {
            self.may_need_drop.insert(ty, is);
        }
        (is, param)
    }

    pub fn enum_flag_ty(&self, en: FragRef<Enum>) -> Option<Builtin> {
        Some(match self[self[en].variants].len() {
            256.. => Builtin::U16,
            2.. => Builtin::U8,
            _ => return None,
        })
    }

    pub fn display_sig(
        &self,
        func: FragRef<Func>,
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
                .map_or(default(), |cc| format!("\"{cc}\" ")),
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
                .map(|(i, str)| format!("var{i}: {str}"))
                .intersperse(", ".into())
                .collect::<String>(),
            self.display_ty(signature.ret, interner),
        )
    }

    pub fn func_name(&self, func: FragRef<Func>, to: &mut String, interner: &Interner) {
        let Func {
            name, loc, owner, ..
        } = self.funcs[func];
        if let Some(loc) = loc {
            write!(to, "{}\\", loc.module.index()).unwrap();
        }
        if let Some(owner) = owner {
            self.display_ty_to(owner.base(self), to, interner);
            write!(to, "\\").unwrap();
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
                write!(
                    to,
                    "{}\\",
                    self[r#struct]
                        .loc
                        .map_or(usize::MAX, |loc| loc.module.index())
                )
                .unwrap();
                to.push_str(&interner[self[r#struct].name])
            }
            Ty::Enum(r#enum) => {
                write!(
                    to,
                    "{}\\",
                    self[r#enum]
                        .loc
                        .map_or(usize::MAX, |loc| loc.module.index())
                )
                .unwrap();
                to.push_str(&interner[self[r#enum].name])
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
            Ty::Param(i) => write!(to, "param{i}").unwrap(),
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

    pub fn display_spec_sum(&self, spec: FragSlice<Spec>, interner: &Interner) -> String {
        let mut str = String::new();
        self.display_spec_sum_to(self[spec].iter().copied(), &mut str, interner);
        str
    }

    pub fn display_spec_sum_to(
        &self,
        mut spec: impl Iterator<Item = Spec>,
        to: &mut String,
        interner: &Interner,
    ) {
        if let Some(first) = spec.next() {
            to.push_str(": ");
            self.display_spec_to(first, to, interner);
            for spec in spec {
                to.push_str(" + ");
                self.display_spec_to(spec, to, interner);
            }
        }
    }

    pub fn spec_sum(
        &mut self,
        specs: impl Iterator<Item = Spec> + Clone + ExactSizeIterator,
        interner: &mut Interner,
    ) -> FragSlice<Spec> {
        let id = interner.intern_with(|s, t| self.display_spec_sum_to(specs.clone(), t, s));

        if id == Interner::EMPTY {
            return default();
        }

        let res = self
            .lookup
            .entry(id)
            .or_insert_with(|| ComputedTypecItem::SpecSum(self.spec_sums.extend(specs)))
            .to_owned();

        match res {
            ComputedTypecItem::SpecSum(ss) => ss,
            res => unreachable!("{res:?}"),
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
        !matches!(self.contains_params_low(ty), ParamPresence::Absent)
    }

    pub fn contains_params_low(&self, ty: Ty) -> ParamPresence {
        use ParamPresence::*;
        match ty {
            Ty::Instance(instance) => self[self[instance].args]
                .iter()
                .map(|&ty| self.contains_params_low(ty))
                .fold(Absent, ParamPresence::combine),
            Ty::Pointer(pointer) => self
                .contains_params_low(self[pointer].base)
                .combine(self[pointer].mutability.into())
                .put_behind_pointer(),
            Ty::Param(..) => Present,
            Ty::Struct(..) | Ty::Builtin(..) | Ty::Enum(..) => Absent,
        }
    }

    pub fn init(&mut self, interner: &mut Interner) {
        SpecBase::init_water_drops(self);
        Enum::init_water_drops(self);
        Struct::init_water_drops(self);
        Func::init_water_drops(self);
        self.init_builtin_funcs(interner);
    }

    fn init_builtin_funcs(&mut self, interner: &mut Interner) {
        self.builtin_funcs
            .extend(Func::WATER_DROPS.map(|(.., func)| func));
        self[Func::CAST] = Func {
            generics: self.params.extend([default(), default()]), // F, T
            signature: Signature {
                cc: default(),
                args: self.args.extend([Ty::Param(0)]),
                ret: Ty::Param(1),
            },
            name: Interner::CAST,
            flags: FuncFlags::BUILTIN,
            ..default()
        };
        self[Func::SIZEOF] = Func {
            generics: self.params.extend([default()]), // T
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
            let op = interner.intern(op);
            let id = interner.intern_with(|s, t| self.binary_op_id(op, a, b, t, s));

            let signature = Signature {
                cc: default(),
                args: self.args.extend([a, b]),
                ret: r,
            };

            let func = Func {
                signature,
                flags: FuncFlags::BUILTIN,
                name: id,
                ..default()
            };

            let func = if let Some(water_drop) = Func::lookup_water_drop(&interner[id]) {
                self[water_drop] = func;
                water_drop
            } else {
                self.funcs.push(func)
            };

            self.builtin_funcs.push(func);
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

        op_to_ty("+ - / *", Ty::INTEGERS, |op, ty| {
            create_bin_op(op, ty, ty, ty)
        });
        op_to_ty("== != < > <= >=", Ty::SCALARS, |op, ty| {
            create_bin_op(op, ty, ty, Ty::BOOL)
        });
        op_to_ty("| & ^", Ty::BINARY, |op, ty| create_bin_op(op, ty, ty, ty));
    }

    pub fn pack_func_param_specs(
        &self,
        func: FragRef<Func>,
    ) -> impl Iterator<Item = FragSlice<Spec>> + '_ {
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
    ) -> impl Iterator<Item = FragSlice<Spec>> + '_ {
        let SpecFunc {
            generics, parent, ..
        } = func;
        let SpecBase {
            generics: upper_generics,
            ..
        } = self[parent];
        iter::empty()
            .chain(self[upper_generics].iter().copied())
            .chain(iter::once(FragSlice::empty()))
            .chain(self[generics].iter().copied())
    }

    pub fn binary_op_id(&self, op: Ident, lhs: Ty, rhs: Ty, to: &mut String, interner: &Interner) {
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
    ) -> FragRef<Pointer> {
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
        write!(to, "{mutability}").unwrap();
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
    ) -> FragRef<Instance> {
        let id = interner.intern_with(|s, t| self.instance_id(base, args, t, s));
        match self.lookup.entry(id) {
            Entry::Occupied(occ) => match occ.get() {
                &ComputedTypecItem::Instance(instance) => instance,
                _ => unreachable!(),
            },
            Entry::Vacant(entry) => {
                let instance = Instance {
                    base,
                    args: self.args.extend(args.iter().cloned()),
                };
                let instance = self.instances.push(instance);
                entry.insert(ComputedTypecItem::Instance(instance));
                instance
            }
        }
    }

    pub fn spec_instance(
        &mut self,
        base: FragRef<SpecBase>,
        args: &[Ty],
        interner: &mut Interner,
    ) -> FragRef<SpecInstance> {
        let id = interner.intern_with(|s, t| self.spec_instance_id(base, args, t, s));
        match self.lookup.entry(id) {
            Entry::Occupied(occ) => match occ.get() {
                &ComputedTypecItem::SpecInstance(instance) => instance,
                _ => unreachable!(),
            },
            Entry::Vacant(entry) => {
                let instance = SpecInstance {
                    base,
                    args: self.args.extend(args.iter().cloned()),
                };
                let instance = self.spec_instances.push(instance);
                entry.insert(ComputedTypecItem::SpecInstance(instance));
                instance
            }
        }
    }

    pub fn spec_instance_id(
        &self,
        base: FragRef<SpecBase>,
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

    pub fn implements_sum(
        &mut self,
        ty: Ty,
        sum: FragSlice<Spec>,
        params: impl TypecCtxSlice<FragSlice<Spec>>,
        inferred: &[Ty],
        missing_keys: &mut Option<&mut BumpVec<ImplKey>>,
        interner: &mut Interner,
    ) -> bool {
        self[sum].to_bumpvec().into_iter().all(|spec| {
            let spec = self.instantiate_spec(spec, inferred, interner);
            self.find_implementation(ty, spec, params, missing_keys, interner)
                .is_some()
        })
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
        params: impl TypecCtxSlice<FragSlice<Spec>>,
        missing_keys: &mut Option<&mut BumpVec<ImplKey>>,
        interner: &mut Interner,
    ) -> Option<Option<(FragRef<Impl>, FragSlice<Ty>)>> {
        if let Ty::Param(index) = ty {
            let mut frontier = self[params.get(self)[index as usize]].to_bumpvec();
            while let Some(other_spec) = frontier.pop() {
                if spec == other_spec {
                    return Some(None);
                }

                let params = match other_spec {
                    Spec::Base(..) => default(),
                    Spec::Instance(instance) => self[instance].args,
                };

                for inherit in self[other_spec.base(self)].inherits.keys() {
                    let inherit = self.instantiate_spec(self[inherit], params, interner);
                    frontier.push(inherit);
                }
            }
            return None;
        }

        let key = ImplKey { ty, spec };

        if let Some(result) = self.impl_lookup.get(&key) {
            return Some(Some(result.to_owned()));
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

        let mut base_impl = self.impl_lookup.get(&base_key).map(|i| i.to_owned().0);

        while let Some(current) = base_impl {
            let impl_ent = self.impls[current];
            base_impl = impl_ent.next;

            let mut generic_slots = bumpvec![None; impl_ent.generics.len()];
            let spec_compatible = self.compatible(&mut generic_slots, ty, impl_ent.key.ty);
            let ty_compatible = self.compatible_spec(&mut generic_slots, spec, impl_ent.key.spec);

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
                        self[specs],
                        impl_ent.generics,
                        &params,
                        missing_keys,
                        interner,
                    )
                });

            if !implements {
                continue;
            }

            let params = self.args.extend(params);
            if !self.contains_params(ty) {
                self.impl_lookup.insert(key, (current, params));
            }
            return Some(Some((current, params)));
        }

        if let Some(v) = missing_keys {
            v.push(key)
        }

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
                    .map_err(|(a, b)| SpecCmpError::Args(a, b))
            }
            _ => Err(SpecCmpError::Specs(reference, template)),
        }
    }

    pub fn balance_pointers(&mut self, mut ty: Ty, reference: Ty, interner: &mut Interner) -> Ty {
        let (ty_depth, reference_depth) = (ty.ptr_depth(self), reference.ptr_depth(self));
        match ty_depth.cmp(&reference_depth) {
            std::cmp::Ordering::Less => {
                let muts = (0..(reference_depth - ty_depth))
                    .scan(reference, |r, _| {
                        let mutability = ty.mutability(self);
                        *r = r.ptr_base(self);
                        Some(mutability)
                    })
                    .collect::<BumpVec<_>>();

                for mutability in muts.into_iter().rev() {
                    ty = Ty::Pointer(self.pointer_to(mutability, ty, interner));
                }

                ty
            }
            std::cmp::Ordering::Equal => ty,
            std::cmp::Ordering::Greater => {
                for _ in 0..(ty_depth - reference_depth) {
                    ty = ty.ptr_base(self);
                }
                ty
            }
        }
    }

    pub fn infer_ty_params(&self, param_count: usize, reference: Ty, template: Ty) -> BumpVec<Ty> {
        let mut params = bumpvec![None; param_count];
        let res = self.compatible(&mut params, reference, template);
        assert!(res.is_ok());
        assert!(params.iter().all(|p| p.is_some()));
        const _: () = assert!(mem::size_of::<Option<Ty>>() == mem::size_of::<Ty>());
        unsafe { mem::transmute(params) }
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
            if reference == template && !self.contains_params(template) {
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

    pub fn instantiate(
        &mut self,
        ty: Ty,
        params: impl TypecCtxSlice<Ty>,
        interner: &mut Interner,
    ) -> Ty {
        match ty {
            Ty::Struct(..) | Ty::Builtin(..) | Ty::Enum(..) => ty,
            ty if params.is_empty() => ty,
            Ty::Instance(instance) => {
                let Instance { base, args } = self[instance];
                let args = args
                    .keys()
                    .map(|arg| self.instantiate(self[arg], params, interner))
                    .collect::<BumpVec<_>>();
                Ty::Instance(self.instance(base, args.as_slice(), interner))
            }
            Ty::Pointer(pointer) => {
                let Pointer {
                    base, mutability, ..
                } = self[pointer];
                let base = self.instantiate(base, params, interner);
                Ty::Pointer(self.pointer_to(mutability, base, interner))
            }
            Ty::Param(index) => params.get(self)[index as usize],
        }
    }

    pub fn try_instantiate(
        &mut self,
        ty: Ty,
        params: &[Option<Ty>],
        interner: &mut Interner,
    ) -> Option<Ty> {
        Some(match ty {
            Ty::Struct(..) | Ty::Builtin(..) | Ty::Enum(..) => ty,
            ty if params.is_empty() => ty,
            Ty::Instance(instance) => {
                let Instance { base, args } = self[instance];
                let args = self[args]
                    .to_bumpvec()
                    .into_iter()
                    .map(|arg| self.try_instantiate(arg, params, interner))
                    .collect::<Option<BumpVec<_>>>()?;
                Ty::Instance(self.instance(base, args.as_slice(), interner))
            }
            Ty::Pointer(pointer) => {
                let Pointer {
                    base, mutability, ..
                } = self[pointer];
                let base = self.try_instantiate(base, params, interner)?;
                Ty::Pointer(self.pointer_to(mutability, base, interner))
            }
            Ty::Param(index) => return params[index as usize],
        })
    }

    pub fn instantiate_spec(
        &mut self,
        spec: Spec,
        params: impl TypecCtxSlice<Ty>,
        interner: &mut Interner,
    ) -> Spec {
        match spec {
            Spec::Base(..) => spec,
            spec if params.is_empty() => spec,
            Spec::Instance(instance) => {
                let SpecInstance { base, args } = self[instance];
                let args = self[args]
                    .to_bumpvec()
                    .into_iter()
                    .map(|arg| self.instantiate(arg, params, interner))
                    .collect::<BumpVec<_>>();
                Spec::Instance(self.spec_instance(base, args.as_slice(), interner))
            }
        }
    }

    pub fn try_instantiate_spec(
        &mut self,
        spec: Spec,
        params: &[Option<Ty>],
        interner: &mut Interner,
    ) -> Option<Spec> {
        Some(match spec {
            Spec::Base(..) => spec,
            spec if params.is_empty() => spec,
            Spec::Instance(instance) => {
                let SpecInstance { base, args } = self[instance];
                let args = self[args]
                    .to_bumpvec()
                    .into_iter()
                    .map(|arg| self.try_instantiate(arg, params, interner))
                    .collect::<Option<BumpVec<_>>>()?;
                Spec::Instance(self.spec_instance(base, args.as_slice(), interner))
            }
        })
    }
}

#[derive(Debug)]
pub enum SpecCmpError {
    Specs(Spec, Spec),
    Args(Ty, Ty),
}

#[derive(Clone, Copy)]
pub struct Loc {
    pub module: VRef<Module>,
    pub item: VRef<ModuleItem>,
}

pub const fn sorted_water_drops<T, const LEN: usize>(
    mut drops: [(&'static str, FragRef<T>); LEN],
) -> [(&'static str, FragRef<T>); LEN] {
    let mut i = 0;
    while i < LEN {
        let mut j = i + 1;
        let mut min = drops[i].0;
        let mut min_index = i;
        while j < LEN {
            if let std::cmp::Ordering::Less = compare_strings(drops[j].0.as_bytes(), min.as_bytes())
            {
                min = drops[j].0;
                min_index = j;
            }
            j += 1;
        }
        drops.swap(i, min_index);
        i += 1;
    }

    // check correctness
    i = 0;
    while i < LEN - 1 {
        assert!(matches!(
            compare_strings(drops[i].0.as_bytes(), drops[i + 1].0.as_bytes()),
            std::cmp::Ordering::Less
        ));
        i += 1;
    }

    drops
}

const fn compare_strings(a: &[u8], b: &[u8]) -> std::cmp::Ordering {
    let mut i = 0;
    while i < a.len() && i < b.len() {
        if a[i] < b[i] {
            return std::cmp::Ordering::Less;
        } else if a[i] > b[i] {
            return std::cmp::Ordering::Greater;
        }

        i += 1;
    }

    if a.len() < b.len() {
        std::cmp::Ordering::Less
    } else if a.len() > b.len() {
        std::cmp::Ordering::Greater
    } else {
        std::cmp::Ordering::Equal
    }
}

pub fn lookup_water_drop<T>(drops: &[(&str, FragRef<T>)], name: &str) -> Option<FragRef<T>> {
    drops
        .binary_search_by_key(&name, |(str, _)| str)
        .map(|i| drops[i].1)
        .ok()
}

#[derive(Clone, Copy, Debug)]
pub struct MacroImpl {
    pub name: Ident,
    pub r#impl: OptFragRef<Impl>,
    pub params: FragSlice<Ty>,
}

#[derive(Default, Clone, PartialEq, Eq, Debug)]
pub struct ModuleItems {
    pub items: PushMap<ModuleItem>,
}

impl ModuleItems {
    pub fn clear(&mut self) {
        self.items.clear();
    }
}

pub enum ParamPresence {
    Present,
    Absent,
    BehindPointer,
}

impl ParamPresence {
    pub fn combine(self, other: ParamPresence) -> ParamPresence {
        use ParamPresence::*;
        match (self, other) {
            (Present, ..) | (.., Present) => Present,
            (BehindPointer, ..) | (.., BehindPointer) => BehindPointer,
            (Absent, Absent) => Absent,
        }
    }

    pub fn put_behind_pointer(self) -> ParamPresence {
        use ParamPresence::*;
        match self {
            Absent => Absent,
            Present | BehindPointer => BehindPointer,
        }
    }
}

impl From<Mutability> for ParamPresence {
    fn from(m: Mutability) -> ParamPresence {
        use {Mutability::*, ParamPresence::*};
        match m {
            Immutable => Absent,
            Mutable => Present,
            Param(..) => BehindPointer,
        }
    }
}

pub trait TypecCtxSlice<T: 'static>: Copy {
    fn get<'a, 'b: 'a>(&'b self, typec: &'a Typec) -> &'a [T];
    fn is_empty(&self) -> bool;
}

impl<T: 'static> TypecCtxSlice<T> for FragSlice<T>
where
    Typec: Index<FragSlice<T>, Output = [T]>,
{
    fn get<'a, 'b: 'a>(&'b self, typec: &'a Typec) -> &'a [T] {
        &typec[*self]
    }

    fn is_empty(&self) -> bool {
        self.is_empty()
    }
}

impl<T: 'static> TypecCtxSlice<T> for &[T] {
    fn get<'a, 'b: 'a>(&'b self, _: &'a Typec) -> &'a [T] {
        self
    }

    fn is_empty(&self) -> bool {
        (*self).is_empty()
    }
}
