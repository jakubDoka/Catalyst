use std::ops::Deref;
use std::sync::Arc;
use std::{
    default::default,
    iter,
    ops::{Index, IndexMut},
};

use crate::ty::compact::CompactTy;
use crate::*;

use diags::{SourceLoc, Workspace};
use resources::{Resources, Source};

use rkyv::{Archive, Deserialize, Serialize};
use storage::*;

#[derive(Clone, Archive, Serialize, Deserialize, Default)]
pub struct ImplList {
    #[with(SmallVecArchiver)]
    pub inner: SmallVec<[FragRef<Impl>; 4]>,
}

derive_relocated!(struct ImplList { inner });

#[derive(Default, Serialize, Deserialize, Archive)]
pub struct Mapping {
    pub specs: CMap<Ident, FragRef<SpecInstance>>,
    pub spec_sums: CMap<Ident, FragSlice<CompactSpec>>,
    pub pointers: CMap<Ident, FragRef<CompactTy>>,
    pub instances: CMap<Ident, FragRef<Instance>>,
    pub arrays: CMap<Ident, FragRef<Array>>,
    pub impl_lookup: CMap<ImplKey, ImplList>,
    pub macros: CMap<CompactTy, MacroImpl>,
    pub may_need_drop: CMap<CompactTy, bool>,
}

derive_relocated!(struct Mapping { specs pointers instances arrays impl_lookup macros may_need_drop });

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

    pub fn expand(&mut self, thread_count: u8) {
        self.cache.expand(thread_count);
    }

    pub fn split(&mut self) -> impl Iterator<Item = Types> + '_ {
        self.cache.split().map(|cache| Types {
            cache,
            mapping: self.mapping.clone(),
            module_items: default(),
            generic_names: default(),
        })
    }

    pub fn register<'a>(&'a mut self, objects: &mut RelocatedObjects<'a>) {
        objects.add_cleared(&mut self.mapping);
        self.cache.register(objects);
    }
}

impl Default for Types {
    fn default() -> Self {
        let mut base = TypecBase::new(1);
        let types = { base.split().next().unwrap() };
        types
    }
}

pub struct Types {
    pub mapping: Arc<Mapping>,
    pub cache: TypeCache,
    pub module_items: ShadowMap<Source, ModuleItems>,
    pub generic_names: Vec<Ident>,
}

impl Deref for Types {
    type Target = Mapping;

    fn deref(&self) -> &Self::Target {
        &self.mapping
    }
}

impl Index<VRef<Source>> for Types {
    type Output = ModuleItems;

    fn index(&self, index: VRef<Source>) -> &Self::Output {
        &self.module_items[index]
    }
}

impl IndexMut<VRef<Source>> for Types {
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
        pub struct TypeCache {
            $(
                pub $name: FragMap<$ty>,
            )*
            $(
                pub $sync_name: SyncFragMap<$sync_ty>,
            )*
        }

        impl TypeCache {
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
            impl Index<FragRef<$ty>> for Types {
                type Output = $ty;

                #[inline]
                fn index(&self, index: FragRef<$ty>) -> &Self::Output {
                    &self.cache.$name[index]
                }
            }

            impl IndexMut<FragRef<$ty>> for Types {
                #[inline]
                fn index_mut(&mut self, index: FragRef<$ty>) -> &mut Self::Output {
                    &mut self.cache.$name[index]
                }
            }

            impl Index<FragSlice<$ty>> for Types {
                type Output = [$ty];

                #[inline]
                fn index(&self, index: FragSlice<$ty>) -> &Self::Output {
                    &self.cache.$name[index]
                }
            }

            impl IndexMut<FragSlice<$ty>> for Types {
                #[inline]
                fn index_mut(&mut self, index: FragSlice<$ty>) -> &mut Self::Output {
                    &mut self.cache.$name[index]
                }
            }
        )*

        $(
            impl Index<FragRef<$sync_ty>> for Types {
                type Output = $sync_ty;

                #[inline]
                fn index(&self, index: FragRef<$sync_ty>) -> &Self::Output {
                    &self.cache.$sync_name[index]
                }
            }

            impl Index<FragSlice<$sync_ty>> for Types {
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

            pub fn expand(&mut self, thread_count: u8) {
                $(
                    self.$name.expand(thread_count);
                )*
                $(
                    self.$sync_name.expand(thread_count);
                )*
            }

            pub fn split(&mut self) -> impl Iterator<Item = TypeCache> + '_ {
                $(
                    let mut $name = self.$name.split();
                )*
                $(
                    let mut $sync_name = self.$sync_name.split();
                )*
                iter::from_fn(move || {
                    Some(TypeCache {
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
    func_slices: FragRef<Func>,
    spec_funcs: SpecFunc,
    variants: Variant,
    enums: Enum,
    consts: Const,
    predicates: WherePredicate,
    [sync]
    spec_sums: CompactSpec,
    spec_instances: SpecInstance,
    instances: Instance,
    args: CompactTy,
    arrays: crate::Array,
}

impl Types {
    pub fn pull(&mut self, base: &TypecBase) {
        self.cache.pull(&base.cache);
    }

    pub fn commit(&mut self, base: &mut TypecBase) {
        self.cache.commit(&mut base.cache);
    }

    pub fn commit_unique(self, base: &mut TypecBase) {
        self.cache.commit_unique(&mut base.cache);
    }

    pub fn enum_flag_ty(&self, en: FragRef<Enum>) -> Builtin {
        match self[en].variants.len() {
            256.. => Builtin::U16,
            2.. => Builtin::U8,
            _ => Builtin::Unit,
        }
    }
}

#[derive(Debug)]
pub enum SpecCmpError<'a> {
    Specs(Spec<'a>, Spec<'a>),
    Args(Ty<'a>, Ty<'a>),
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

    pub fn source_loc(self, types: &Types) -> Option<SourceLoc> {
        Some(SourceLoc {
            origin: self.source,
            span: types.module_items[self.source].items[self.item?].span,
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

    pub fn source_loc(self, types: &Types) -> SourceLoc {
        SourceLoc {
            origin: self.source,
            span: types.module_items[self.source].items[self.item].span,
        }
    }
}

impl From<GuaranteedLoc> for Loc {
    fn from(val: GuaranteedLoc) -> Self {
        Loc {
            source: val.source,
            item: Some(val.item),
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
    pub params: FragSlice<CompactTy>,
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
    fn get<'a, 'b: 'a>(&'b self, types: &'a Types) -> &'a [T];
    fn is_empty(&self) -> bool;
}

impl<T: 'static> TypecCtxSlice<T> for FragSlice<T>
where
    Types: Index<FragSlice<T>, Output = [T]>,
{
    fn get<'a, 'b: 'a>(&'b self, types: &'a Types) -> &'a [T] {
        &types[*self]
    }

    fn is_empty(&self) -> bool {
        self.is_empty()
    }
}

impl<T: 'static> TypecCtxSlice<T> for &[T] {
    fn get<'a, 'b: 'a>(&'b self, _: &'a Types) -> &'a [T] {
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
    pub types: &'ctx mut Types,
    pub interner: &'ctx mut Interner,
    pub resources: &'ctx Resources,
    pub workspace: &'ctx mut Workspace,
}

pub trait ConstFolder {
    fn fold(&mut self, ty: Ty, code: TirNode, ctx: ConstFolderContext) -> FolderValue;
}
