use std::{
    alloc::{Allocator, Global},
    default::default,
    mem,
    ptr::{self, NonNull},
};

use crate::field;

use super::{ArcVec, FragVec};

use {
    super::{ArcVecInner, FragVecView},
    crate::{FragAddr, FragRef},
    core::slice,
    std::alloc::Layout,
    trusted::TrustedObjectRef,
};

mod trusted {
    use super::*;

    pub(super) struct TrustedObjectRef(FragRef<ObjectHeader>);

    impl TrustedObjectRef {
        pub fn addr(&self) -> FragAddr {
            self.0.addr()
        }
    }
}

struct LocalRelPtr {
    location: u32,
    offset: u32,
}

struct GlobalRelPtr {
    object: TrustedObjectRef,
    location: u32,
    offset: u32,
}

pub struct ObjectHeader {
    base: usize,
    locals: usize,
    globals: usize,
    end: usize,
}

struct Object<'a> {
    data: *mut u8,
    locals: &'a [LocalRelPtr],
    globals: &'a [GlobalRelPtr],
}

impl<'a> Object<'a> {
    fn new(data: &'a ThreadData, header: &ObjectHeader) -> Self {
        let base = data.base();
        Self {
            data: unsafe { base.add(header.base) },
            locals: unsafe {
                let ptr = base.add(header.locals).cast();
                let len = header.globals - header.locals;
                slice::from_raw_parts(ptr, len)
            },
            globals: unsafe {
                let ptr = base.add(header.globals).cast();
                let len = header.end - header.globals;
                slice::from_raw_parts(ptr, len)
            },
        }
    }

    fn from_builder(builder: &'a ObjectBuilder) -> Self {
        Self {
            data: builder.memory,
            locals: &builder.locals,
            globals: &builder.globals,
        }
    }

    unsafe fn relocate(&self, base: &'a ObjectFragBase) {
        for rel in self.locals {
            self.data
                .add(rel.location as usize)
                .cast::<*mut u8>()
                .write(self.data.add(rel.offset as usize))
        }

        for rel in self.globals {
            let (thread, other) = base.get_header(&rel.object);
            let ptr = ArcVecInner::get_item(thread.memory.inner.0, 0)
                .add(other.base)
                .add(rel.offset as usize);
            self.data
                .add(rel.location as usize)
                .cast::<*mut u8>()
                .write(ptr);
        }
    }
}

struct ThreadData {
    headers: FragVec<ObjectHeader>,
    memory: FragVecView<u8>,
    layout: Layout,
    offset: usize,
    biggest_align_offset: usize,
    thread: u8,
}

impl ThreadData {
    unsafe fn allocate(&mut self, layout: Layout, base: &ObjectFragBase) -> (*mut u8, usize) {
        let max_req_size = layout.size() + layout.align();
        let mut s = self.memory.inner.0;
        let cap = field!(s => cap);
        let len = field!(s => len);

        if len + max_req_size > cap {
            s = self.grow(len, max_req_size, base);
        }

        let (new_layout, offset) = self.layout.extend(layout).unwrap();
        if layout.align() > self.layout.align() {
            self.biggest_align_offset = offset
        }
        field!(s => mut len) = new_layout.size();
        self.layout = new_layout;

        (self.base().add(offset), offset)
    }

    #[cold]
    #[inline(never)]
    unsafe fn grow(
        &mut self,
        len: usize,
        pushed: usize,
        base: &ObjectFragBase,
    ) -> NonNull<ArcVecInner<u8>> {
        let len = (len + pushed).next_power_of_two();
        let new = ArcVecInner::<u8, Global>::with_capacity(len, Global);
        let offset =
            ArcVecInner::get_item(new, self.biggest_align_offset).align_offset(self.layout.align());

        ptr::copy_nonoverlapping(
            self.base(),
            ArcVecInner::get_item(new, offset),
            len - self.offset,
        );

        self.memory.inner = ArcVec(new);
        self.offset = offset;

        new
    }

    fn base(&self) -> *mut u8 {
        let base = unsafe { ArcVecInner::get_item(self.memory.inner.0, 0) };
        unsafe { base.add(self.offset) }
    }
}

pub struct ObjectFragBase {
    data: Box<[ThreadData]>,
}

impl ObjectFragBase {
    fn get_header(&self, frag: &TrustedObjectRef) -> (&ThreadData, &ObjectHeader) {
        let FragAddr { index, thread, .. } = frag.addr();
        let thread = unsafe { self.data.get_unchecked(thread as usize) };
        (thread, unsafe {
            &*ArcVecInner::get_item(thread.headers.view.inner.0, index as usize)
        })
    }
}

pub struct ObjectBuilder {
    memory: *mut u8,
    memory_layout: Layout,
    locals: Vec<LocalRelPtr>,
    globals: Vec<GlobalRelPtr>,
    layout: Layout,
}

impl ObjectBuilder {
    pub fn new() -> Self {
        Self {
            memory: ptr::null_mut(),
            memory_layout: Layout::new::<()>(),
            locals: default(),
            globals: default(),
            layout: Layout::new::<()>(),
        }
    }

    fn layout(&self) -> (Layout, usize, usize) {
        let (layout, locals) = self
            .layout
            .extend(unsafe {
                Layout::from_size_align_unchecked(
                    mem::size_of::<LocalRelPtr>() * self.locals.len(),
                    mem::align_of::<LocalRelPtr>(),
                )
            })
            .unwrap();

        let (layout, globals) = layout
            .extend(unsafe {
                Layout::from_size_align_unchecked(
                    mem::size_of::<GlobalRelPtr>() * self.globals.len(),
                    mem::align_of::<GlobalRelPtr>(),
                )
            })
            .unwrap();
        (layout, locals, globals)
    }

    pub fn allocate(&mut self, owner_offset: u32, layout: Layout) -> *mut u8 {
        let (new_layout, offset) = self.layout.extend(layout).unwrap();
        if new_layout.size() > self.memory_layout.size() {
            self.grow(new_layout);
        }
        let rel_ptr = LocalRelPtr {
            location: owner_offset,
            offset: offset as u32 - owner_offset,
        };
        self.locals.push(rel_ptr);

        unsafe { self.memory.add(offset) }
    }

    #[cold]
    #[inline(never)]
    fn grow(&mut self, layout: Layout) {
        let new_layout = unsafe {
            Layout::from_size_align_unchecked(layout.size().next_power_of_two(), layout.align())
        };

        let allocation = match NonNull::new(self.memory) {
            Some(ptr) => unsafe { Global.grow(ptr, self.layout, new_layout) },
            None => Global.allocate(new_layout),
        }
        .unwrap();

        let (ptr, ..) = allocation.to_raw_parts();
        self.memory = ptr.as_ptr().cast();
        self.memory_layout = new_layout;
    }

    fn clear(&mut self) {
        self.layout = unsafe { Layout::from_size_align_unchecked(0, self.memory_layout.align()) };
        self.locals.clear();
        self.globals.clear();
    }
}

pub struct ObjectFragMap {
    base: ObjectFragBase,
    local: ThreadData,
}

impl ObjectFragMap {
    pub fn allocate(&mut self, builder: &mut ObjectBuilder) -> FragRef<ObjectHeader> {
        let (layout, locals, globals) = builder.layout();
        let offset = unsafe {
            let (addr, offset) = self.local.allocate(layout, todo!());
            ptr::copy_nonoverlapping(builder.memory, addr, builder.memory_layout.size());
            ptr::copy_nonoverlapping(
                builder.locals.as_ptr(),
                addr.add(locals).cast(),
                builder.locals.len(),
            );
            ptr::copy_nonoverlapping(
                builder.globals.as_ptr(),
                addr.add(globals).cast(),
                builder.globals.len(),
            );
            offset
        };

        let header = ObjectHeader {
            base: offset,
            locals: offset + locals,
            globals: offset + globals,
            end: offset + layout.size(),
        };

        let (header, ..) = self.local.headers.push(header);

        builder.clear();

        header
    }
}
