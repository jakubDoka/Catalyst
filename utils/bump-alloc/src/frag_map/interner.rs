use core::slice;
use std::{
    alloc::{Allocator as Alloc, Global, Layout},
    default::default,
    fmt::{Display, Write},
    ops::{Deref, DerefMut},
    ptr::{self, NonNull},
    sync::Arc,
};

use rkyv::string::ArchivedString;

use super::*;
use crate::*;

#[derive(
    Clone, Archive, Serialize, Deserialize, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash,
)]
#[archive_attr(derive(PartialEq, Eq, Hash))]
pub struct Ident(FragRef<&'static str>);

impl Default for Ident {
    fn default() -> Self {
        Interner::EMPTY
    }
}

derive_relocated!(
    struct Ident {}
);

macro_rules! gen_span_constants {
    (
        $($name:ident => $repr:literal,)*
    ) => {
        impl Interner {
            gen_span_constants!(@recur (0) $($name => $repr,)*);

            fn init(&mut self) {
                $(
                    let ident = self.intern($repr);
                    assert_eq!(ident.0, Self::$name.0, "constant {} is not interned", stringify!($name));
                    assert_eq!(&self[ident], $repr, "constant {} repr is not equal to repr", stringify!($name));
                )*
            }
        }
    };

    (@recur ($acc:expr) $name:ident => $repr:literal, $($rest:tt)*) => {
        pub const $name: Ident = Ident(FragRef::new(FragAddr::new($acc as u32, 0)));
        gen_span_constants!(@recur ($acc + 1) $($rest)*);
    };

    (@recur ($acc:expr)) => {};
}

gen_span_constants! {
    EMPTY => "",
    SELF => "Self",
    EQUAL => "==",
    BAND => "bool & bool",
    ASSIGN => "=",
    CAST => "cast",
    TOKEN_MACRO => "token_macro",
    NEW => "new",
    START => "start",
    NEXT => "next",
    CLEAR => "clear",
    DROP => "drop",
    SIZEOF => "sizeof",
    MOIST => "moist",
    BUILTIN => "builtin",
}

pub struct InternerBase {
    index: Arc<CMap<&'static str, Ident>>,
    storage: SyncFragBase<&'static str>,
    cluster: Cluster,
}

#[repr(transparent)]
pub struct ArchivedInterner {
    indices: ArchivedVec<(ArchivedString, ArchivedVec<Archived<usize>>)>,
}

impl Archive for InternerBase {
    type Archived = ArchivedInterner;

    type Resolver = VecResolver;

    unsafe fn resolve(&self, pos: usize, resolver: Self::Resolver, out: *mut Self::Archived) {
        ArchivedVec::resolve_from_len(
            self.storage.views.len(),
            pos,
            resolver,
            out_field!(out.indices).1,
        )
    }
}

impl<S: ScratchSpace + Serializer + ?Sized> Serialize<S> for InternerBase {
    fn serialize(&self, serializer: &mut S) -> Result<Self::Resolver, <S as Fallible>::Error> {
        ArchivedVec::<(ArchivedString, ArchivedVec<Archived<u32>>)>::serialize_from_iter(
            self.storage.views.iter().map(|view| {
                let mut len = 0;
                let lens = unsafe { ArcVecInner::full_data(view.inner.load().0) }
                    .iter()
                    .inspect(|str| len += str.len())
                    .map(|str| str.len() as u32)
                    .collect::<Vec<_>>();
                let mut string = String::with_capacity(len);
                unsafe { ArcVecInner::full_data(view.inner.load().0) }
                    .iter()
                    .copied()
                    .collect_into(&mut string);

                (string, lens)
            }),
            serializer,
        )
    }
}

impl<D: Fallible + ?Sized> Deserialize<InternerBase, D> for ArchivedInterner {
    fn deserialize(&self, _deserializer: &mut D) -> Result<InternerBase, <D as Fallible>::Error> {
        let cluster = Cluster::new(self.indices.len() as u8);
        let storage = SyncFragBase::new(self.indices.len() as u8);
        let index = CMap::default();

        for (((string, lens), mut cluster), mut storage) in self
            .indices
            .iter()
            .zip(cluster.split())
            .zip(storage.split())
        {
            let (str, ..) = unsafe { cluster.alloc(&string) };
            for (start, end) in lens.iter().scan(0, |s, i| {
                let res = (*s, *s + *i as usize);
                *s += *i as usize;
                Some(res)
            }) {
                let sub_str = unsafe { str.get_unchecked(start..end) };
                let id = storage.push(sub_str);
                index.insert(sub_str, Ident(id));
            }
        }

        Ok(InternerBase {
            index: index.into(),
            storage,
            cluster,
        })
    }
}

impl InternerBase {
    pub fn new(thread_count: u8) -> Self {
        let s = Self {
            index: default(),
            storage: SyncFragBase::new(thread_count),
            cluster: Cluster::new(thread_count),
        };

        if let Some(mut s) = s.split().next() {
            s.init()
        }

        s
    }

    pub fn split(&self) -> impl Iterator<Item = Interner> + '_ {
        let mut clusters = self.cluster.split();
        let mut storages = self.storage.split();
        iter::from_fn(move || {
            Some(Interner {
                index: self.index.clone(),
                storage: storages.next()?,
                cluster: clusters.next()?,
                temp: default(),
            })
        })
    }
}

pub struct Interner {
    index: Arc<CMap<&'static str, Ident>>,
    storage: SyncFragMap<&'static str>,
    cluster: ClusterBorrow,
    temp: String,
}

impl Interner {
    pub fn intern_scoped(&mut self, scope: impl Display, name: Ident) -> Ident {
        self.intern_with(|s, t| write!(t, "{}\\{}", scope, &s[name]))
    }

    pub fn intern_with<T>(&mut self, mut builder: impl FnMut(&Self, &mut String) -> T) -> Ident {
        let mut temp = std::mem::take(&mut self.temp);
        builder(self, &mut temp);
        let res = self.intern(&temp);
        temp.clear();
        self.temp = temp;
        res
    }

    pub fn intern(&mut self, s: &str) -> Ident {
        let (slice, pop) = unsafe { self.cluster.alloc(s) };
        match self.index.entry(slice) {
            dashmap::mapref::entry::Entry::Occupied(entry) => {
                unsafe {
                    self.cluster.pop(pop);
                }
                *entry.get()
            }
            dashmap::mapref::entry::Entry::Vacant(entry) => {
                *entry.insert(Ident(self.storage.push(slice)))
            }
        }
    }
}

impl Index<Ident> for Interner {
    type Output = str;

    fn index(&self, index: Ident) -> &Self::Output {
        self.storage[index.0]
    }
}

impl Default for Interner {
    fn default() -> Self {
        InternerBase::new(1).split().next().unwrap()
    }
}

struct Cluster {
    allocs: ArcVec<Allocator>,
}

impl Cluster {
    fn new(thread_count: u8) -> Self {
        let new = ArcVecInner::with_capacity(thread_count as usize, Global);
        unsafe {
            ArcVecInner::extend(new, (0..thread_count).map(|_| Allocator::new()));
        }
        Self {
            allocs: ArcVec(new),
        }
    }

    fn split(&self) -> impl Iterator<Item = ClusterBorrow> + '_ {
        unsafe {
            assert!(ArcVecInner::is_unique(self.allocs.0));
            ArcVecInner::full_data_mut(self.allocs.0)
                .iter_mut()
                .map(|borrowed| ClusterBorrow {
                    borrowed,
                    _backing: self.allocs.clone(),
                })
        }
    }
}

struct ClusterBorrow {
    borrowed: *mut Allocator,
    _backing: ArcVec<Allocator>,
}

unsafe impl Send for ClusterBorrow {}
unsafe impl Sync for ClusterBorrow {}

impl Deref for ClusterBorrow {
    type Target = Allocator;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.borrowed }
    }
}

impl DerefMut for ClusterBorrow {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.borrowed }
    }
}

struct Allocator {
    buckets: Vec<Bucket>,
    start: *mut u8,
    current: *mut u8,
    bucket_size: usize,
}

impl Allocator {
    fn new() -> Self {
        Self {
            buckets: default(),
            start: NonNull::dangling().as_ptr(),
            current: NonNull::dangling().as_ptr(),
            bucket_size: 1 << 15,
        }
    }

    unsafe fn alloc(&mut self, content: &str) -> (&'static str, Pop) {
        if self.start.add(content.len()) > self.current {
            self.grow(content.len());
        }

        let pop = Pop(self.current);

        let addr = self.current.sub(content.len());
        ptr::copy_nonoverlapping(content.as_ptr(), addr, content.len());
        self.current = addr;

        (
            std::str::from_utf8_unchecked(slice::from_raw_parts(addr, content.len())),
            pop,
        )
    }

    unsafe fn pop(&mut self, pop: Pop) {
        self.current = pop.0;
    }

    #[cold]
    #[inline(never)]
    unsafe fn grow(&mut self, min_size: usize) {
        let new_size = self.bucket_size.max(min_size);
        let bucket = Bucket::new(new_size);
        self.start = bucket.ptr.as_ptr();
        self.current = bucket.ptr.as_ptr().add(bucket.len);
        self.buckets.push(bucket);
    }
}

struct Pop(*mut u8);

struct Bucket {
    ptr: NonNull<u8>,
    len: usize,
}

unsafe impl Send for Bucket {}
unsafe impl Sync for Bucket {}

impl Bucket {
    pub fn new(len: usize) -> Self {
        let layout = unsafe { Layout::from_size_align_unchecked(len, 1) };
        Self {
            ptr: Global.allocate(layout).unwrap().to_raw_parts().0.cast(),
            len,
        }
    }
}

impl Drop for Bucket {
    fn drop(&mut self) {
        unsafe { Global.deallocate(self.ptr, Layout::from_size_align_unchecked(self.len, 1)) }
    }
}
