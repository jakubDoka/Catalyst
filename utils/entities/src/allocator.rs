use std::{
    alloc::Layout,
    default::default,
    ops::Range,
    ptr::{self, NonNull},
};

pub mod arena;
pub mod code;

/// A simple bump allocator that allocates memory into fixed size chunks.
pub struct Allocator {
    free: Vec<Chunk>,
    chunks: Vec<Chunk>,
    current: *mut u8,
    start: *mut u8,
    chunk_size: usize,
}

impl Default for Allocator {
    fn default() -> Self {
        Self::new(Self::DEFAULT_CHUNK_SIZE)
    }
}

impl Allocator {
    /// Something with linux page size
    pub const DEFAULT_CHUNK_SIZE: usize = 1024 * 1024 * 2;

    /// Create a new allocator with a chunk size of `chunk_size`.
    /// The constructor by it self does not allocate anything.
    pub fn new(chunk_size: usize) -> Self {
        Self {
            free: default(),
            chunks: default(),
            current: NonNull::dangling().as_ptr(),
            start: NonNull::dangling().as_ptr(),
            chunk_size,
        }
    }

    /// Allocate a new chunk of memory. This rarely causes system calls if the layout size is small
    /// compared to the chunk size. Lifetime of the returned memory is tied to the lifetime of the
    /// allocator.
    pub fn alloc(&mut self, layout: Layout, write_padding: bool) -> NonNull<[u8]> {
        let padding = Self::compute_padding(self.current, layout);
        let size = layout.size() + padding;
        if (self.current as usize - self.start as usize) < size {
            self.alloc_new(layout, write_padding)
        } else {
            // SAFETY: We just checked that the current chunk has enough space
            if write_padding {
                Self::write_padding(self.current, padding);
            }
            let new = unsafe { self.current.sub(size) };
            self.current = new;
            // SAFETY: Allocation with address range range 0..n should never happen
            unsafe { NonNull::new_unchecked(ptr::slice_from_raw_parts_mut(new, size)) }
        }
    }

    fn write_padding(current: *mut u8, value: usize) {
        let addr = unsafe { current.sub(value) };
        match value {
            0 => {}
            1..=255 => unsafe { ptr::write(addr, value as u8) },
            _ => unsafe { ptr::write_unaligned(addr as *mut usize, value) },
        }
    }

    fn read_padding(current: *mut u8, layout: Layout) -> usize {
        let addr = unsafe { current.sub(layout.size()) };
        match addr as usize % layout.align() {
            0 => 0,
            1..=255 => unsafe { ptr::read(addr) as usize },
            _ => unsafe { ptr::read_unaligned(addr as *const usize) },
        }
    }

    fn compute_padding(current: *const u8, layout: Layout) -> usize {
        current.align_offset(layout.align())
    }

    #[cold]
    #[inline(never)]
    fn alloc_new(&mut self, layout: Layout, write_padding: bool) -> NonNull<[u8]> {
        // we need a reserve in case the allocation is not aligned
        // for the layout
        let min_size = layout.pad_to_align().size() + layout.align();

        let reuse = self
            .free
            .last_mut()
            .map_or(false, |chunk| chunk.len() >= layout.size());
        let mut new_chunk = if reuse {
            // SAFETY: Branch implies that garbage contains something.
            unsafe { self.free.pop().unwrap_unchecked() }
        } else {
            Chunk::new(
                self.chunk_size.max(min_size),
                region::Protection::READ_WRITE,
            )
        };

        // SAFETY: we are the unique owner of the chunk
        let Range { start, end } = new_chunk.range_mut();
        let padding = Self::compute_padding(end, layout);
        if write_padding {
            Self::write_padding(end, padding);
        }
        let size = layout.size() + padding;
        // SAFETY: former code ensures that there is enough space
        let new = unsafe { end.sub(size) };
        self.current = new;
        self.start = start;

        self.chunks.push(new_chunk);

        // SAFETY: Allocation with address range range 0..n should never happen
        unsafe { NonNull::new_unchecked(ptr::slice_from_raw_parts_mut(new, size)) }
    }

    /// Declares all memory freed. Using the pointers after this call is UB.
    pub unsafe fn clear(&mut self) {
        self.free.append(&mut self.chunks);
        self.current = NonNull::dangling().as_ptr();
        self.start = NonNull::dangling().as_ptr();
    }
}

struct AllocatorFrame {
    previous_start: *mut u8,
    previous_current: *mut u8,
    previous_chunk_size: usize,
}

impl AllocatorFrame {
    fn new(allocator: &Allocator) -> Self {
        let previous_start = allocator.start;
        let previous_current = allocator.current;
        let previous_chunk_size = allocator.chunks.len();

        Self {
            previous_start,
            previous_current,
            previous_chunk_size,
        }
    }

    /// Safety: This cannot be called after earlier created frames called this on the allocator.
    /// Allocator also must be the same whith which the frame was created.
    unsafe fn revert(&self, allocator: &mut Allocator) {
        allocator
            .chunks
            .drain(self.previous_chunk_size..)
            .collect_into(&mut allocator.free);
        allocator.current = self.previous_current;
        allocator.start = self.previous_start;
    }
}

impl Allocator {
    /// # Safety
    /// `previous_size` must be the size of the allocation at `ptr`.
    /// puadding needs to be written for top allocation
    pub unsafe fn grow(
        &mut self,
        ptr: NonNull<u8>,
        previous_layout: Layout,
        new_layout: Layout,
    ) -> NonNull<[u8]> {
        let reused = self.try_free(ptr, previous_layout);

        let mut new = self.alloc(new_layout, true);

        let copy_method = if reused {
            ptr::copy
        } else {
            ptr::copy_nonoverlapping
        };

        copy_method(
            ptr.as_ptr(),
            new.as_mut().as_mut_ptr(),
            previous_layout.size(),
        );

        new
    }

    /// try to free the memory at `ptr` if it is the last allocation in the current chunk
    /// # Safety
    /// `size` mush match the size of the allocation at `ptr`.
    pub unsafe fn try_free(&mut self, ptr: NonNull<u8>, layout: Layout) -> bool {
        let current = self.current;
        if current != ptr.as_ptr() {
            return false;
        }

        let padding = Self::read_padding(current, layout);
        let restored = current.add(layout.size() + padding);
        self.current = restored;

        true
    }
}

use chunk::Chunk;

#[cfg(not(miri))]
mod chunk {
    use std::{default::default, ops::Range, ptr, slice};

    use rkyv::{
        out_field,
        ser::{ScratchSpace, Serializer},
        vec::{ArchivedVec, VecResolver},
        with::DeserializeWith,
        Archive, Archived, Deserialize, Fallible, Serialize,
    };

    transmute_arkive!(ArchivedProtection(region::Protection => u64));

    pub struct Chunk {
        data: region::Allocation,
        protection: region::Protection,
    }

    impl Chunk {
        pub fn new(cap: usize, protection: region::Protection) -> Self {
            Chunk {
                data: region::alloc(cap, protection).expect("Failed to allocate memory"),
                protection,
            }
        }

        pub fn len(&self) -> usize {
            self.data.len()
        }

        pub fn range_mut(&mut self) -> Range<*mut u8> {
            self.data.as_mut_ptr_range()
        }

        pub fn range(&self) -> Range<*const u8> {
            self.data.as_ptr_range()
        }
    }

    pub struct ArchivedChunk {
        data: ArchivedVec<u8>,
        protection: Archived<u64>,
    }

    impl Archive for Chunk {
        type Archived = ArchivedChunk;

        type Resolver = VecResolver;

        unsafe fn resolve(&self, pos: usize, resolver: Self::Resolver, out: *mut Self::Archived) {
            let (o, f) = out_field!(out.data);
            ArchivedVec::<u8>::resolve_from_len(self.len(), pos + o, resolver, f);
            let (o, f) = out_field!(out.protection);
            <Archived<u64>>::resolve(&(self.protection.bits() as u64), pos + o, default(), f);
        }
    }

    impl<S: Serializer + ScratchSpace> Serialize<S> for Chunk
    where
        u8: Serialize<S>,
    {
        fn serialize(
            &self,
            serializer: &mut S,
        ) -> Result<Self::Resolver, <S as rkyv::Fallible>::Error> {
            let slice = unsafe { slice::from_ptr_range(self.data.as_ptr_range::<u8>()) };
            ArchivedVec::<u8>::serialize_from_slice(slice, serializer)
        }
    }

    impl<D: Fallible> Deserialize<Chunk, D> for ArchivedChunk {
        fn deserialize(&self, deserializer: &mut D) -> Result<Chunk, <D as Fallible>::Error> {
            let slice = self.data.as_ptr() as *const u8;
            let mut region = Chunk::new(
                self.data.len(),
                ArchivedProtection::deserialize_with(&self.protection, deserializer)?,
            );
            unsafe { ptr::copy_nonoverlapping(slice, region.data.as_mut_ptr(), self.data.len()) }
            Ok(region)
        }
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn test_execute_write() {
            let mut chunk = Chunk::new(1, region::Protection::READ_WRITE_EXECUTE);
            let range = chunk.range_mut();
            let ptr = range.start;
            unsafe {
                *ptr = 0xc3; // ret
                let f: fn() = std::mem::transmute(ptr);
                f();
            }
        }
    }
}

#[cfg(miri)]
mod chunk {
    use std::{
        alloc::{Allocator, Global, Layout},
        ops::Range,
        ptr::{self, NonNull},
    };

    pub struct Chunk {
        data: NonNull<[u8]>,
    }

    impl Chunk {
        const ALIGN: usize = 1 << 10;
        pub fn new(cap: usize, _protection: region::Protection) -> Self {
            Chunk {
                data: Global
                    .allocate(Layout::from_size_align(cap, Self::ALIGN).unwrap())
                    .unwrap(),
            }
        }

        pub fn len(&self) -> usize {
            self.data.len()
        }

        pub fn range(&mut self) -> Range<*mut u8> {
            let (start, len) = self.data.to_raw_parts();
            unsafe { start.as_ptr() as _..start.as_ptr().cast::<u8>().add(len) }
        }

        pub fn set_memory_protection(&mut self, protection: region::Protection) {
            unimplemented!();
        }
    }

    impl Drop for Chunk {
        fn drop(&mut self) {
            let (start, cap) = self.data.to_raw_parts();
            let layout = Layout::from_size_align(cap, Self::ALIGN).unwrap();
            unsafe { Global.deallocate(start.cast(), layout) }
        }
    }
}
