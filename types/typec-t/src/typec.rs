use core::fmt;
use std::mem;
use std::{
    default::default,
    ops::{Index, IndexMut},
};

use crate::*;
use lexing_t::Span;
use packaging_t::Module;
use storage::*;

#[derive(Default)]
pub struct Typec {
    pub types: Types,
    pub funcs: Funcs,
    pub fields: Fields,
    pub impls: Impls,
    pub impl_lookup: ImplLookup,
    pub ty_slices: TySlices,
    pub func_slices: FuncSlices,
    pub spec_funcs: SpecFuncs,
    pub builtin_funcs: Vec<VRef<Func>>,
    pub module_items: ShadowMap<Module, PushMap<ModuleItem>>,
}

macro_rules! gen_index {
    (
        $($ty:ty => $storage:ident)*
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
    };
}

gen_index! {
    Ty => types
    Func => funcs
    Field => fields
}

macro_rules! assert_init {
    (
        ($self:expr, $interner:expr)
        $(
            $name:ident {
                $($body:tt)*
            }
        )*
    ) => {
        const _: () = {
            let mut index = 0;
            $(
                assert!(Ty::$name.index() == Ty::ALL[index].index());
                index += 1;
            )*
            _ = index;
        };

        $(
            let name = if Ty::$name == Ty::UNIT {
                "()"
            } else {
                stringify!($name)
            };

            $self.add_builtin_ty(name, Ty {
                $($body)*
                ..Default::default()
            }, $interner);
        )*
    };
}

impl Typec {
    pub fn display_sig(
        &self,
        func: VRef<Func>,
        interner: &Interner,
        buffer: &mut String,
    ) -> fmt::Result {
        let Func { signature, .. } = self.funcs[func];
        use fmt::Write;
        write!(
            buffer,
            "fn {}[todo] {}({}) -> {} ",
            signature
                .cc
                .map(|cc| &interner[cc])
                .map_or(default(), |cc| format!("\"{}\" ", cc)),
            &interner[self.funcs.id(func)],
            self.ty_slices[signature.args]
                .iter()
                .map(|&ty| &interner[self.types.id(ty)])
                .enumerate()
                .map(|(i, str)| format!("var{}: {}", i, str))
                .intersperse(String::from(", "))
                .collect::<String>(),
            &interner[self.types.id(signature.ret)],
        )
    }

    pub fn init(&mut self, interner: &mut Interner) {
        self.init_builtin_types(interner);
        self.init_builtin_funcs(interner);
        self.init_builtin_impls(interner);
    }

    fn init_builtin_impls(&mut self, _interner: &mut Interner) {
        self.impls.push(Impl {
            generics: default(),
            ty: Ty::UNIT,
            spec: Ty::ANY,
            methods: default(),
            next: None,
            span: None,
        });
    }

    fn init_builtin_types(&mut self, interner: &mut Interner) {
        assert_init! {
            (self, interner)
            MUTABLE {}
            IMMUTABLE {}
            ANY {
                kind: TyKind::Spec(default()),
            }
            UNIT {
                kind: TyKind::Struct(default()),
            }
            UINT {
                kind: TyKind::Integer(default()),
            }
            U32 {
                kind: TyKind::Integer(TyInteger { size: 4, signed: false }),
            }
            CHAR {
                kind: TyKind::Integer(TyInteger { size: 4, signed: false }),
            }
            TERMINAL {}
        }
    }

    fn init_builtin_funcs(&mut self, interner: &mut Interner) {
        let anon_temp = interner.intern_str("anon_temp");
        let anon_temp = self.funcs.insert_unique(anon_temp, Default::default());
        assert!(anon_temp == Func::ANON_TEMP);

        let int_bin_ops = "+ - / *".split_whitespace();

        for op in int_bin_ops {
            for &ty in Ty::INTEGERS {
                let op = interner.intern_str(op);
                let segments = self.binary_op_id(op, ty, ty);
                let id = interner.intern(segments);

                let signature = Signature {
                    cc: default(),
                    args: self.ty_slices.bump([ty, ty]),
                    ret: ty,
                };

                let func = self.funcs.insert_unique(
                    id,
                    Func {
                        signature,
                        flags: FuncFlags::BUILTIN,
                        loc: Loc::Builtin(op),
                        name: op,
                        ..default()
                    },
                );

                self.builtin_funcs.push(func);
            }
        }
    }

    pub fn add_builtin_ty(&mut self, name: &str, mut ty: Ty, interner: &mut Interner) {
        let lower_name = name.to_lowercase();
        let ident = interner.intern_str(lower_name.as_str());
        ty.loc = Loc::Builtin(ident);
        self.types.insert(ident, ty);
    }

    pub fn binary_op_id(
        &self,
        op: VRef<str>,
        lhs: VRef<Ty>,
        rhs: VRef<Ty>,
    ) -> impl Iterator<Item = InternedSegment<'static>> {
        ident!(self.types.id(lhs), " ", op, " ", self.types.id(rhs)).into_iter()
    }

    pub fn pointer_id(
        &self,
        mutability: VRef<Ty>,
        base: VRef<Ty>,
    ) -> impl Iterator<Item = InternedSegment<'static>> {
        ident!("^", self.types.id(mutability), " ", self.types.id(base)).into_iter()
    }

    pub fn instance_id<'a>(
        &'a self,
        base: VRef<Ty>,
        params: &'a [VRef<Ty>],
    ) -> impl Iterator<Item = InternedSegment<'static>> + 'a {
        let prefix = ident!(self.types.id(base), "[").into_iter();
        let params = ident_join(", ", params.iter().map(|&p| self.types.id(p)));
        let suffix = ident!("]");
        prefix.chain(params).chain(suffix)
    }

    pub fn instance(
        &mut self,
        base: VRef<Ty>,
        args: &[VRef<Ty>],
        interner: &mut Interner,
    ) -> VRef<Ty> {
        let id = self.instance_id(base, args);
        let id = interner.intern(id);
        self.types.get_or_insert(id, |_| Ty {
            kind: TyInstance {
                base,
                args: self.ty_slices.bump_slice(args),
            }
            .into(),
            ..Default::default()
        })
    }

    pub fn bound_sum_id<'a>(
        &'a self,
        bounds: &'a [VRef<Ty>],
    ) -> impl Iterator<Item = InternedSegment<'static>> + 'a {
        ident_join(" + ", bounds.iter().map(|&b| self.types.id(b)))
    }

    pub fn tuple_id<'a>(
        &'a self,
        tys: &'a [VRef<Ty>],
    ) -> impl Iterator<Item = InternedSegment<'static>> + 'a {
        let start = ident!("(");
        let body = ident_join(", ", tys.iter().map(|&t| self.types.id(t)));
        let end = ident!(")");

        start.into_iter().chain(body).chain(end)
    }

    pub fn nth_param(&mut self, index: usize, interner: &mut Interner) -> VRef<Ty> {
        let key = interner.intern(ident!("param ", index as u32));

        let fallback = |_: &mut Types| Ty {
            kind: TyKind::Param(index as u32),
            flags: TyFlags::GENERIC,
            loc: default(),
        };

        self.types.get_or_insert(key, fallback)
    }

    pub fn span<T: Located>(&self, target: VRef<T>) -> Option<Span>
    where
        Self: Index<VRef<T>, Output = T>,
    {
        match self[target].loc() {
            Loc::Module { module, item } => Some(self.module_items[module][item].span),
            Loc::Builtin(..) => None,
        }
    }

    pub fn implements(&mut self, ty: VRef<Ty>, spec: VRef<Ty>) -> Option<VRef<Impl>> {
        if spec == Ty::ANY {
            return Some(Impl::ANY);
        }

        let key = (ty, spec);

        if let Some(&result) = self.impl_lookup.get(&key) {
            return result;
        }

        let ty_base = self.types.base(ty);
        let spec_base = self.types.base(spec);

        let base_key = (ty_base, spec_base);

        let Some(&(mut base_impl)) = self.impl_lookup.get(&base_key) else {
            self.impl_lookup.insert(dbg!(key), None);
            return None;
        };

        while let Some(current) = base_impl {
            let impl_ent = self.impls[current];
            base_impl = impl_ent.next;

            let generics = self.ty_slices[impl_ent.generics].to_bumpvec();
            let mut generic_slots = bumpvec![None; generics.len()];
            let spec_compatible = self.compatible(&mut generic_slots, ty, impl_ent.ty);
            let ty_compatible = self.compatible(&mut generic_slots, spec, impl_ent.spec);

            if ty_compatible.is_err() || spec_compatible.is_err() {
                continue;
            }

            let implements = generics
                .into_iter()
                .zip(generic_slots.into_iter().map(|s| s.unwrap()))
                .all(|(spec, ty)| self.implements(ty, spec).is_some());

            if !implements {
                continue;
            }

            self.impl_lookup.insert(key, Some(current));
            return Some(current);
        }

        self.impl_lookup.insert(key, None);
        None
    }

    pub fn compatible(
        &self,
        params: &mut [Option<VRef<Ty>>],
        reference: VRef<Ty>,
        template: VRef<Ty>,
    ) -> Result<(), (VRef<Ty>, VRef<Ty>)> {
        let mut stack = bumpvec![(reference, template)];

        let check = |a, b| Ty::compatible(a, b).then_some(()).ok_or((a, b));

        while let Some((reference, template)) = stack.pop() {
            if reference == template {
                continue;
            }

            match (
                self.types[reference].kind,
                self.types[template].kind,
            ) {
                (TyKind::Pointer(reference), TyKind::Pointer(template)) => {
                    stack.push((reference.base, template.base));
                    stack.push((reference.mutability, template.mutability));
                }
                (TyKind::Instance(reference), TyKind::Instance(template)) => {
                    check(reference.base, template.base)?;
                    stack.extend(
                        self.ty_slices[reference.args]
                            .iter()
                            .copied()
                            .zip(self.ty_slices[template.args].iter().copied()),
                    );
                }
                (_, TyKind::Param(index)) if let Some(inferred) = params[index as usize] => {
                    check(inferred, reference)?;
                }
                (_, TyKind::Param(index)) => params[index as usize] = Some(reference),
                _ => return Err((reference, template)),
            }
        }

        Ok(())
    }

    pub fn instantiate(
        &mut self,
        ty: VRef<Ty>,
        params: &[VRef<Ty>],
        interner: &mut Interner,
    ) -> VRef<Ty> {
        unsafe {
            self.instantiate_low(ty, mem::transmute(params), interner)
                .unwrap_unchecked()
        }
    }

    pub fn try_instantiate(
        &mut self,
        ty: VRef<Ty>,
        params: &[Option<VRef<Ty>>],
        interner: &mut Interner,
    ) -> Option<VRef<Ty>> {
        self.instantiate_low(ty, params, interner)
    }

    pub fn instantiate_low(
        &mut self,
        ty: VRef<Ty>,
        params: &[Option<VRef<Ty>>],
        interner: &mut Interner,
    ) -> Option<VRef<Ty>> {
        Some(match self.types[ty].kind {
            TyKind::Instance(TyInstance { base, args }) => {
                let params = self.ty_slices[args]
                    .to_bumpvec()
                    .into_iter()
                    .map(|arg| self.instantiate_low(arg, params, interner))
                    .collect::<Option<BumpVec<_>>>()?;

                let generic = params.iter().any(|&arg| self.types.is_generic(arg));

                let segments = self.instance_id(base, &params);
                let id = interner.intern(segments);

                let fallback = |types: &mut Types| Ty {
                    kind: TyKind::Instance(TyInstance {
                        base,
                        args: self.ty_slices.bump(params),
                    }),
                    flags: TyFlags::GENERIC & generic,
                    ..types[ty]
                };

                self.types.get_or_insert(id, fallback)
            }
            TyKind::Pointer(TyPointer {
                base,
                mutability,
                depth,
            }) => {
                let base = self.instantiate_low(base, params, interner)?;
                let mutability = self.instantiate_low(mutability, params, interner)?;
                let generic = self.types.is_generic(base) | self.types.is_generic(mutability);

                let segments = self.pointer_id(base, mutability);
                let id = interner.intern(segments);

                let fallback = |types: &mut Types| Ty {
                    kind: TyKind::Pointer(TyPointer {
                        base,
                        mutability,
                        depth,
                    }),
                    flags: TyFlags::GENERIC & generic,
                    ..types[ty]
                };

                self.types.get_or_insert(id, fallback)
            }
            TyKind::Param(index) => return params[index as usize],
            TyKind::Struct(..) | TyKind::Integer(..) | TyKind::Bool | TyKind::Spec(..) => ty,
        })
    }
}

pub trait Located {
    fn loc(&self) -> Loc;
}

#[derive(Clone, Copy)]
pub enum Loc {
    Module {
        module: VRef<Module>,
        item: VRef<ModuleItem>,
    },
    Builtin(VRef<str>),
}

impl Default for Loc {
    fn default() -> Self {
        Loc::Builtin(VRef::default())
    }
}
