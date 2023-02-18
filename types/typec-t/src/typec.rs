use std::sync::Arc;
use std::{
    default::default,
    iter,
    ops::{Index, IndexMut},
};
use std::{mem, ops::Deref};

use crate::*;

use diags::{SourceLoc, Workspace};
use packaging_t::{Resources, Source};

use rkyv::{Archive, Deserialize, Serialize};
use storage::*;

pub type TypecLookup = CMap<Ident, ComputedTypecItem>;
pub type ImplLookup = CMap<(FragRef<SpecBase>, Ty), ImplList>;
pub type Implemented = CMap<ImplKey, (FragRef<Impl>, FragSlice<Ty>)>;
pub type Macros = CMap<Ty, MacroImpl>;
pub type MayNeedDrop = CMap<Ty, bool>;

#[derive(Clone, Archive, Serialize, Deserialize, Default)]

pub struct ImplList {
    #[with(SmallVecArchiver)]
    pub inner: SmallVec<[FragRef<Impl>; 4]>,
}

derive_relocated!(struct ImplList { inner });

#[derive(Default, Serialize, Deserialize, Archive)]

pub struct Mapping {
    pub lookup: TypecLookup,
    pub impl_lookup: ImplLookup,
    pub implemented: Implemented,
    pub macros: Macros,
    pub may_need_drop: MayNeedDrop,
}

derive_relocated!(struct Mapping { lookup impl_lookup implemented macros may_need_drop });

#[derive(Serialize, Deserialize, Archive)]

pub struct TypecBase {
    pub cache: TypecCacheBase,
    pub mapping: Arc<Mapping>,
}

impl TypecBase {
    pub fn new(thread_count: u8) -> Self {
        Self {
            cache: TypecCacheBase::new(thread_count),
            mapping: default(),
        }
    }

    pub fn split(&self) -> impl Iterator<Item = Typec> + '_ {
        self.cache.split().map(|cache| Typec {
            cache,
            mapping: self.mapping.clone(),
            module_items: default(),
        })
    }

    pub fn register<'a>(&'a mut self, objects: &mut RelocatedObjects<'a>) {
        objects.add_cleared(&mut self.mapping);
        self.cache.register(objects);
    }
}

impl Default for Typec {
    fn default() -> Self {
        let base = TypecBase::new(1);
        let typec = { base.split().next().unwrap() };
        typec
    }
}

pub struct Typec {
    pub mapping: Arc<Mapping>,
    pub cache: TypecCache,
    pub module_items: ShadowMap<Source, ModuleItems>,
}

impl Deref for Typec {
    type Target = Mapping;

    fn deref(&self) -> &Self::Target {
        &self.mapping
    }
}

impl Index<VRef<Source>> for Typec {
    type Output = ModuleItems;

    fn index(&self, index: VRef<Source>) -> &Self::Output {
        &self.module_items[index]
    }
}

impl IndexMut<VRef<Source>> for Typec {
    fn index_mut(&mut self, index: VRef<Source>) -> &mut Self::Output {
        &mut self.module_items[index]
    }
}

macro_rules! gen_cache {
    (
        $self:ident
        $($name:ident: $ty:ty,)*
        [sync]
        $($sync_name:ident: $sync_ty:ty,)*
    ) => {
        pub struct TypecCache {
            $(
                pub $name: FragMap<$ty>,
            )*
            $(
                pub $sync_name: SyncFragMap<$sync_ty>,
            )*
        }

        impl TypecCache {
            pub fn commit(&mut self, base: &mut TypecCacheBase) {
                $(
                    self.$name.commit(&mut base.$name);
                )*
            }

            pub fn pull(&mut self, base: &TypecCacheBase) {
                $(
                    self.$name.pull(&base.$name);
                )*
            }

            pub fn commit_unique(self, base: &mut TypecCacheBase) {
                $(
                    self.$name.commit_unique(&mut base.$name);
                )*
            }
        }

        $(
            impl Index<FragRef<$ty>> for Typec {
                type Output = $ty;

                #[inline]
                fn index(&self, index: FragRef<$ty>) -> &Self::Output {
                    &self.cache.$name[index]
                }
            }

            impl IndexMut<FragRef<$ty>> for Typec {
                #[inline]
                fn index_mut(&mut self, index: FragRef<$ty>) -> &mut Self::Output {
                    &mut self.cache.$name[index]
                }
            }

            impl Index<FragSlice<$ty>> for Typec {
                type Output = [$ty];

                #[inline]
                fn index(&self, index: FragSlice<$ty>) -> &Self::Output {
                    &self.cache.$name[index]
                }
            }

            impl IndexMut<FragSlice<$ty>> for Typec {
                #[inline]
                fn index_mut(&mut self, index: FragSlice<$ty>) -> &mut Self::Output {
                    &mut self.cache.$name[index]
                }
            }
        )*

        $(
            impl Index<FragRef<$sync_ty>> for Typec {
                type Output = $sync_ty;

                #[inline]
                fn index(&self, index: FragRef<$sync_ty>) -> &Self::Output {
                    &self.cache.$sync_name[index]
                }
            }

            impl Index<FragSlice<$sync_ty>> for Typec {
                type Output = [$sync_ty];

                #[inline]
                fn index(&self, index: FragSlice<$sync_ty>) -> &Self::Output {
                    &self.cache.$sync_name[index]
                }
            }
        )*

        #[derive(Serialize, Deserialize, Archive)]

        pub struct TypecCacheBase {
            $(
                pub $name: FragBase<$ty>,
            )*
            $(
                pub $sync_name: SyncFragBase<$sync_ty>,
            )*
        }

        impl TypecCacheBase {
            pub fn new(thread_count: u8) -> Self {
                Self {
                    $(
                        $name: FragBase::new(thread_count),
                    )*
                    $(
                        $sync_name: SyncFragBase::new(thread_count),
                    )*
                }
            }

            pub fn split(&self) -> impl Iterator<Item = TypecCache> + '_ {
                $(
                    let mut $name = self.$name.split();
                )*
                $(
                    let mut $sync_name = self.$sync_name.split();
                )*
                iter::from_fn(move || {
                    Some(TypecCache {
                        $(
                            $name: $name.next()?,
                        )*
                        $(
                            $sync_name: $sync_name.next()?,
                        )*
                    })
                })
            }

            pub fn register<'a>(&'a mut self, frags: &mut RelocatedObjects<'a>) {
                $(
                    frags.add(&mut self.$name);
                )*
                $(
                    frags.add(&mut self.$sync_name);
                )*
            }
        }
    };
}

gen_cache! {
    self
    structs: Struct,
    base_specs: SpecBase,
    funcs: Func,
    fields: Field,
    impls: Impl,
    params: FragSlice<Spec>,
    func_slices: FragRef<Func>,
    spec_funcs: SpecFunc,
    variants: Variant,
    enums: Enum,
    consts: Const,
    [sync]
    pointers: Pointer,
    spec_sums: Spec,
    spec_instances: SpecInstance,
    instances: Instance,
    args: Ty,
    arrays: crate::Array,
}

impl Typec {
    pub fn pull(&mut self, base: &TypecBase) {
        self.cache.pull(&base.cache);
    }

    pub fn commit(&mut self, base: &mut TypecBase) {
        self.cache.commit(&mut base.cache);
    }

    pub fn commit_unique(self, base: &mut TypecBase) {
        self.cache.commit_unique(&mut base.cache);
    }

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
            Ty::Pointer(p, ..) => {
                // TODO: handle mutability
                self.register_ty_generics_low(self[p.ty()], generic, spec_set)
            }
            Ty::Array(a) => self.register_ty_generics_low(self[a].item, generic, spec_set),
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

    pub fn enum_flag_ty(&self, en: FragRef<Enum>) -> Builtin {
        match self[en].variants.len() {
            256.. => Builtin::U16,
            2.. => Builtin::U8,
            _ => Builtin::Unit,
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
                .contains_params_low(self[pointer.ty()])
                .combine(pointer.mutability.to_mutability().into())
                .put_behind_pointer(),
            Ty::Array(array) => self.contains_params_low(self[array].item),
            Ty::Param(..) => Present,
            Ty::Struct(..) | Ty::Builtin(..) | Ty::Enum(..) => Absent,
        }
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

    pub fn dereference(&self, ty: Ty) -> Ty {
        match ty {
            Ty::Pointer(ptr, ..) => self[ptr.ty()],
            _ => ty,
        }
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
                (Ty::Pointer(reference_p), Ty::Pointer(template_p)) => {
                    match (reference_p.mutability.to_mutability(), template_p.mutability.to_mutability()) {
                        (val, Mutability::Param(i)) => params[i as usize] = Some(val.as_ty()),
                        _ if reference_p.mutability.compatible(template_p.mutability) => (),
                        _ => return Err((reference, template)),
                    }
                    stack.push((self[reference_p.ty()], self[template_p.ty()]));
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
                    check(reference, inferred)?;
                }
                (_, Ty::Param(index)) => params[index as usize] = Some(reference),
                _ => return Err((reference, template)),
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum SpecCmpError {
    Specs(Spec, Spec),
    Args(Ty, Ty),
}

#[derive(Clone, Copy, Deserialize, Serialize, Archive)]

pub struct Loc {
    source: VRef<Source>,
    item: Option<VRef<ModuleItem>>,
}

impl Loc {
    pub fn new(source: VRef<Source>, item: impl Into<Option<VRef<ModuleItem>>>) -> Self {
        Self {
            source,
            item: item.into(),
        }
    }

    pub fn source(self) -> VRef<Source> {
        self.source
    }

    pub fn source_loc(self, typec: &Typec) -> Option<SourceLoc> {
        Some(SourceLoc {
            origin: self.source,
            span: typec.module_items[self.source].items[self.item?].span,
        })
    }
}

impl Default for Loc {
    fn default() -> Self {
        Self {
            source: Resources::BUILTIN_SOURCE,
            item: None,
        }
    }
}

#[derive(Clone, Copy, Deserialize, Serialize, Archive)]
pub struct GuaranteedLoc {
    source: VRef<Source>,
    item: VRef<ModuleItem>,
}

impl GuaranteedLoc {
    pub fn new(source: VRef<Source>, item: VRef<ModuleItem>) -> Self {
        Self { source, item }
    }

    pub fn source(self) -> VRef<Source> {
        self.source
    }

    pub fn source_loc(self, typec: &Typec) -> SourceLoc {
        SourceLoc {
            origin: self.source,
            span: typec.module_items[self.source].items[self.item].span,
        }
    }
}

impl Into<Loc> for GuaranteedLoc {
    fn into(self) -> Loc {
        Loc {
            source: self.source,
            item: Some(self.item),
        }
    }
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

#[derive(Clone, Copy, Debug, Serialize, Deserialize, Archive)]

pub struct MacroImpl {
    pub name: Ident,
    pub r#impl: OptFragRef<Impl>,
    pub params: FragSlice<Ty>,
}

derive_relocated! {
    struct MacroImpl { r#impl params }
}

#[derive(Default, Clone, PartialEq, Eq, Debug, Serialize, Deserialize, Archive)]

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

#[derive(Clone, Archive, Serialize, Deserialize, Default)]
pub struct FolderValue(u64);

impl FolderValue {
    pub fn new_register(value: u64) -> Self {
        FolderValue(value)
    }

    pub fn as_array_size(&self) -> Option<ArraySize> {
        Some(self.0 as ArraySize)
    }

    pub fn as_register(&self) -> Option<u64> {
        Some(self.0)
    }
}

pub struct ConstFolderContext<'ctx> {
    pub typec: &'ctx mut Typec,
    pub interner: &'ctx mut Interner,
    pub resources: &'ctx Resources,
    pub workspace: &'ctx mut Workspace,
}

pub trait ConstFolder {
    fn fold(&mut self, ty: Ty, code: TirNode, ctx: ConstFolderContext) -> FolderValue;
}
