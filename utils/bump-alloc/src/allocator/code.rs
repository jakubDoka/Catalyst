use std::{
    default::default,
    mem::ManuallyDrop,
    ops::Range,
    ptr::{self, NonNull},
    slice,
    sync::atomic::AtomicU8,
};

use super::chunk::Chunk;
use crate::{frag_map::relocator::Unified, Allocator};
use region::Protection;
use rkyv::{
    ser::{ScratchSpace, Serializer},
    Archive, Deserialize, Fallible, Serialize,
};

#[derive(Default)]
pub struct CodeRelocator {
    used: Vec<Relocation>,
}

impl CodeRelocator {
    pub fn mark(&mut self, code: &Code) {
        self.used.push(Relocation {
            chunk: 0,
            offset: 0,
            code: unsafe { ptr::read(code) },
            most_aligned: code.start,
        });
    }

    pub fn relocate(&mut self, allocator: &mut CodeAllocator) {
        Unified::unify_vec(&mut self.used);

        let mut current_cunk = 0;
        let mut chunk_offset = 0;

        for reloc in &mut self.used {
            let to_move = allocator.data_ptr(&reloc.code);
            let len = reloc.code.len() as usize;

            let Range { start, end } = allocator.chunks[current_cunk].range_mut();

            if end as usize - start as usize - chunk_offset < len {
                current_cunk += 1;
                chunk_offset = 0;
                if end as usize - start as usize >= len {
                    todo!()
                }
            }

            let relative_most_aligned = reloc.most_aligned - reloc.code.start;
            let padding = unsafe {
                start
                    .add(chunk_offset)
                    .add(relative_most_aligned as usize)
                    .align_offset(reloc.code.align.value())
            };

            unsafe {
                ptr::copy(to_move, start.add(chunk_offset + padding), len);
            }

            reloc.chunk = current_cunk as u32;
            reloc.offset = chunk_offset as u32 + padding as u32;
            reloc.code.padding = padding as u8;

            chunk_offset += len + padding;
        }

        allocator.chunks.truncate(current_cunk + 1);
    }

    pub fn project(&self, code: &mut Code, codes: &mut CodeAllocator) -> Option<()> {
        let reloc = self
            .used
            .binary_search_by(|reloc| reloc.code.cmp(code))
            .map(Some)
            .unwrap_or_else(|i| i.checked_sub(1))?;

        let reloc = self.used.get(reloc)?;
        if reloc.code.chunk != code.chunk || code.end > reloc.code.end {
            return None;
        }

        if code.start == reloc.code.start {
            code.padding = reloc.code.padding;
        }

        code.start = code.start - reloc.code.start + reloc.offset;
        code.chunk = reloc.chunk;

        codes.reset(code);

        Some(())
    }

    pub fn clear(&mut self) {
        self.used.clear();
    }
}

struct Relocation {
    chunk: u32,
    offset: u32,
    code: Code,
    most_aligned: u32,
}

impl Default for Relocation {
    fn default() -> Self {
        unsafe { std::mem::zeroed() }
    }
}

impl Unified for Relocation {
    fn union(&self, other: &Self) -> Option<Self> {
        if self.code.end != other.code.start - other.code.padding as u32
            || self.code.chunk != other.code.chunk
        {
            return None;
        }

        Some(Self {
            code: Code {
                end: other.code.end,
                align: self.code.align.max(other.code.align),
                ..self.code
            },
            chunk: 0,
            offset: 0,
            most_aligned: if self.code.align > other.code.align {
                self.most_aligned
            } else {
                other.most_aligned
            },
        })
    }

    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.code.cmp(&other.code)
    }
}

#[derive(Clone, Copy, Archive, Serialize, Deserialize)]
pub struct Code {
    chunk: u32,
    start: u32,
    end: u32,
    thread: u8,
    padding: u8,
    align: Align,
}

impl PartialEq for Code {
    fn eq(&self, _other: &Self) -> bool {
        unimplemented!()
    }
}

impl Eq for Code {}

impl PartialOrd for Code {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(
            self.chunk
                .cmp(&other.chunk)
                .then(self.start.cmp(&other.start)),
        )
    }
}

impl Ord for Code {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl Code {
    pub fn len(&self) -> u32 {
        self.end - self.start
    }

    pub fn thread(&self) -> u8 {
        self.thread
    }

    pub fn align(&self) -> Align {
        self.align
    }
}

#[derive(Archive, Serialize, Deserialize)]
pub struct ArchivableCodeAllocator {
    chunks: ManuallyDrop<Vec<Chunk>>,
    progress: usize,
}

pub struct CodeAllocator {
    chunks: Vec<Chunk>,
    base: *mut u8,
    cursor: *mut u8,
    end: *mut u8,
}

unsafe impl Send for CodeAllocator {}
unsafe impl Sync for CodeAllocator {}

impl Default for CodeAllocator {
    fn default() -> Self {
        Self::new()
    }
}

impl CodeAllocator {
    const PROTECTION: Protection = Protection::READ_WRITE_EXECUTE;
    const CHUNK_SIZE: usize = Allocator::DEFAULT_CHUNK_SIZE;

    pub fn new() -> Self {
        Self {
            chunks: default(),
            base: NonNull::dangling().as_ptr(),
            cursor: NonNull::dangling().as_ptr(),
            end: NonNull::dangling().as_ptr(),
        }
    }

    pub fn data_ptr(&self, code: &Code) -> *const u8 {
        let chunk = &self.chunks[code.chunk as usize];
        let len = code.len() as usize;
        let offset = code.start as usize;
        assert!(offset + len <= chunk.len());
        unsafe { chunk.range().start.add(offset) }
    }

    pub fn data<'a>(&'a self, code: &Code, finishing: bool) -> CodeGuard<'a> {
        let addr = self.data_ptr(code);
        let lock = unsafe {
            addr.sub(code.padding as usize)
                .cast::<CodeLock>()
                .as_ref()
                .unwrap()
        };

        let len = code.len() as usize;

        CodeGuard {
            code: unsafe {
                if lock.lock() {
                    Ok(slice::from_raw_parts_mut(addr as _, len))
                } else {
                    Err(slice::from_raw_parts(addr, len))
                }
            },
            finish: finishing,
            lock,
        }
    }

    pub fn alloc(&mut self, data: &[u8], align: Align, thread: u8) -> Code {
        let size = data.len();
        let offset = self.align_offset(align);
        let taken = offset + size;

        if (self.end as usize - self.cursor as usize) < taken {
            self.grow(taken);
        }

        let code = self.code(offset, size, align, thread);

        unsafe {
            (self.cursor as *mut CodeLock).write(CodeLock::new());
            let ptr = self.cursor.add(offset);
            ptr::copy_nonoverlapping(data.as_ptr(), ptr, size);
            self.cursor = self.cursor.add(taken);
        }

        code
    }

    fn code(&self, padding: usize, len: usize, align: Align, thread: u8) -> Code {
        let start = padding + (self.cursor as usize - self.base as usize);
        Code {
            chunk: self.chunks.len() as u32 - 1,
            start: start as u32,
            end: (start + len) as u32,
            padding: padding as u8,
            thread,
            align,
        }
    }

    fn grow(&mut self, min_size: usize) {
        let mut chunk = Self::chunk(min_size);
        let range = chunk.range_mut();
        self.chunks.push(chunk);
        self.base = range.start;
        self.cursor = range.start;
        self.end = range.end;
    }

    fn align_offset(&mut self, align: Align) -> usize {
        if self.cursor == self.end {
            return 1;
        }

        let lock_space = 1;
        let next = unsafe { self.cursor.add(lock_space) };
        next.align_offset(align.value()) + lock_space
    }

    fn chunk(min_size: usize) -> Chunk {
        let size = Self::CHUNK_SIZE.max(min_size);
        Chunk::new(size, Self::PROTECTION)
    }

    unsafe fn as_archivable(&self) -> ArchivableCodeAllocator {
        ArchivableCodeAllocator {
            chunks: ManuallyDrop::new(ptr::read(&self.chunks)),
            progress: self.cursor as usize - self.base as usize,
        }
    }

    fn reset(&self, code: &Code) {
        unsafe {
            (self.data_ptr(&code) as *mut u8 as *mut CodeLock).write(CodeLock::new());
        }
    }
}

pub struct CodeGuard<'a> {
    code: Result<&'a mut [u8], &'a [u8]>,
    finish: bool,
    lock: &'a CodeLock,
}

impl<'a> CodeGuard<'a> {
    pub fn data(&self) -> &[u8] {
        match &self.code {
            Ok(o) => &*o,
            Err(e) => e,
        }
    }

    pub fn try_data_mut(&mut self) -> Result<&mut [u8], &[u8]> {
        match &mut self.code {
            Ok(o) => Ok(&mut **o),
            Err(e) => Err(e),
        }
    }
}

impl<'a> Drop for CodeGuard<'a> {
    fn drop(&mut self) {
        if self.finish {
            self.lock.finish();
        } else {
            self.lock.unlock();
        }
    }
}

pub struct CodeLock(AtomicU8);

impl CodeLock {
    const UNLOCKED: u8 = 0;
    const FINISHED: u8 = 1;
    const LOCKED: u8 = 2;

    fn new() -> Self {
        Self(AtomicU8::new(Self::UNLOCKED))
    }

    fn lock(&self) -> bool {
        if self.is_finished() {
            return false;
        }
        loop {
            match self.0.compare_exchange_weak(
                Self::UNLOCKED,
                Self::LOCKED,
                std::sync::atomic::Ordering::Relaxed,
                std::sync::atomic::Ordering::Relaxed,
            ) {
                Ok(Self::UNLOCKED) => break true,
                Err(Self::FINISHED) => break false,
                _ => {}
            }
        }
    }

    fn is_finished(&self) -> bool {
        self.0.load(std::sync::atomic::Ordering::Relaxed) == Self::FINISHED
    }

    fn finish(&self) {
        self.0
            .store(Self::FINISHED, std::sync::atomic::Ordering::Relaxed);
    }

    fn unlock(&self) {
        if self.is_finished() {
            return;
        }

        self.0
            .store(Self::UNLOCKED, std::sync::atomic::Ordering::Relaxed);
    }
}

impl Archive for CodeAllocator {
    type Archived = ArchivedArchivableCodeAllocator;

    type Resolver = ArchivableCodeAllocatorResolver;

    unsafe fn resolve(&self, pos: usize, resolver: Self::Resolver, out: *mut Self::Archived) {
        self.as_archivable().resolve(pos, resolver, out)
    }
}

impl<S: Serializer + ScratchSpace> Serialize<S> for CodeAllocator {
    fn serialize(
        &self,
        serializer: &mut S,
    ) -> Result<Self::Resolver, <S as rkyv::Fallible>::Error> {
        unsafe { self.as_archivable() }.serialize(serializer)
    }
}

impl<D: Fallible> Deserialize<CodeAllocator, D> for ArchivedArchivableCodeAllocator
where
    Self: Deserialize<ArchivableCodeAllocator, D>,
{
    fn deserialize(&self, deserializer: &mut D) -> Result<CodeAllocator, <D as Fallible>::Error> {
        let mut arch = Deserialize::<ArchivableCodeAllocator, D>::deserialize(self, deserializer)?;
        let (base, cursor, end) = match arch.chunks.last_mut() {
            Some(c) => {
                let range = c.range_mut();
                (
                    range.start,
                    unsafe { range.start.add(arch.progress) },
                    range.end,
                )
            }
            None => {
                let dang = NonNull::<u8>::dangling().as_ptr();
                (dang, dang, dang)
            }
        };
        Ok(CodeAllocator {
            chunks: ManuallyDrop::into_inner(arch.chunks),
            base,
            cursor,
            end,
        })
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Serialize, Deserialize, Archive)]
pub struct Align {
    power: u8,
}

impl Align {
    pub const fn new(power: u8) -> Self {
        Self { power }
    }

    pub const fn project(value: u64) -> Self {
        Self {
            power: value.ilog2() as u8,
        }
    }

    pub const fn power(self) -> u8 {
        self.power
    }

    pub const fn mask(self) -> usize {
        self.value() - 1
    }

    pub const fn value(self) -> usize {
        1 << self.power
    }
}
