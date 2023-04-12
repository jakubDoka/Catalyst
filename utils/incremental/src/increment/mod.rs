mod std_type_impls;

/// Implements Increment for any type that implements [`Copy`] and [`IncrementSafe`].
#[macro_export]
macro_rules! impl_default_increment {
    ($($type:ty),*) => {
        $(
            impl $crate::Increment for $type where Self: $crate::IncrementSafe + Copy {
                const DEFAULT: bool = true;

                fn serialize(&self, serializer: &mut $crate::Serializer) {
                    serializer.write(self);
                }

                unsafe fn deserialize(deserializer: &mut $crate::Deserializer) -> Self {
                    deserializer.read()
                }

                fn size(&self) -> usize {
                    std::mem::size_of::<Self>()
                }
            }
        )*
    };
}

/// Shorthand shortcircuiting for when we can take the short route of just writing the data.
#[macro_export]
macro_rules! increment_shortcut {
    (write $self:ident, $serializer:ident, $type:ty) => {
        if <$type as Increment>::DEFAULT {
            return $serializer.write($self);
        }
    };

    (read $deserializer:ident, $type:ty) => {
        if <$type as Increment>::DEFAULT {
            return $deserializer.read();
        }
    };

    (len $self:ty, $type:ty) => {
        if <$type as Increment>::DEFAULT {
            return std::mem::size_of::<$self>();
        }
    };
}

/// A trait that is implemented for all types that can be serialized and deserialized.
pub trait Increment: 'static {
    /// WHether this is implemented by [`impl_default_increment!`]. This is usefull when deciding
    /// wether we can take the short route of just writing the data.
    const DEFAULT: bool = false;

    /// Increment the data into the serializer.
    fn serialize(&self, serializer: &mut Serializer);
    /// Deserialize the data from the deserializer.
    unsafe fn deserialize(deserializer: &mut Deserializer) -> Self;
    /// The size of the data. This must be correct of [`Serializer::debug_assert_fits`] will panic
    /// at debug builds. Overestimates are not checked.
    fn size(&self) -> usize;
}

impl_default_increment! {
    bool,
    u8, u16, u32, u64, u128,
    i8, i16, i32, i64, i128,
    f32, f64,
    char,
    usize, isize
}

/// Struct used for serializing Increment data.
#[derive(Default)]
pub struct Serializer {
    data: Vec<u8>,
}

impl Serializer {
    /// Serializes data and seed into a byte slice. Seed can be later checked with [`Deserializer::deserialize`].
    pub fn serialize<T: Increment, S: Increment>(&mut self, seed: &S, data: &T) -> &[u8] {
        self.data.clear();
        self.data.reserve(data.size() + seed.size());
        seed.serialize(self);
        data.serialize(self);
        &self.data
    }

    /// Writes the T to buffer as is, (bitwise copy)
    pub fn write<T>(&mut self, data: &T) {
        let size = std::mem::size_of::<T>();
        self.debug_assert_fits(size);
        unsafe {
            let ptr = self.data.as_mut_ptr().add(self.data.len());
            ptr.copy_from_nonoverlapping(data as *const T as _, size);
            self.data.set_len(self.data.len() + size);
        }
    }

    /// Writes the slice of T to buffer as is, (bitwise copy)
    pub fn write_slice<T>(&mut self, data: &[T]) {
        let size = std::mem::size_of::<T>() * data.len();
        self.debug_assert_fits(size);
        unsafe {
            let ptr = self.data.as_mut_ptr().add(self.data.len());
            ptr.copy_from_nonoverlapping(data.as_ptr() as _, size);
            self.data.set_len(self.data.len() + size);
        }
    }

    fn debug_assert_fits(&self, size: usize) {
        debug_assert!(self.data.capacity() - self.data.len() >= size);
    }
}

pub struct Deserializer<'a> {
    data: &'a [u8],
}

impl<'a> Deserializer<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        Deserializer { data }
    }

    /// # Safety
    ///
    /// This function attempts to grans some safety by checking all it can. If seed does not fit in
    /// or does not match the data, it will return an error.
    pub unsafe fn deserialize<T: Increment, S: Increment + Eq>(
        &mut self,
        seed: &S,
    ) -> Result<T, Option<S>> {
        if self.data.len() < seed.size() {
            return Err(None);
        }

        let present = S::deserialize(self);

        if present == *seed {
            Ok(T::deserialize(self))
        } else {
            Err(Some(present))
        }
    }

    /// # Safety
    ///
    /// This function simply takes a leading pointer in the data, reads the T from it, and returns it
    /// while also advancing the data pointer. It is up to the caller to ensure that the data read
    /// does not overflow.
    pub unsafe fn read<T: 'static>(&mut self) -> T {
        let size = std::mem::size_of::<T>();
        let ptr = self.data.as_ptr() as *const T;
        self.bump(size);
        ptr.read_unaligned()
    }

    /// # Safety
    ///
    /// This function simply takes a leading pointer in the data, and consides the next len * size_of::<T>()
    /// bytes as a slice of T. It is up to the caller to ensure that the data read does not overflow, and
    /// that the slice contains valid data.
    pub unsafe fn read_slice<T: 'static>(&mut self, len: usize, dest: *mut T) {
        let size = std::mem::size_of::<T>() * len;
        let ptr = self.data.as_ptr();
        self.bump(size);
        ptr.copy_to_nonoverlapping(dest as _, size);
    }

    unsafe fn bump(&mut self, size: usize) {
        debug_assert!(self.data.len() >= size);
        self.data = self.data.get_unchecked(size..);
    }
}

pub auto trait IncrementSafe {}

impl<T> !IncrementSafe for &T {}
impl<T> !IncrementSafe for &mut T {}
impl<T> !IncrementSafe for *const T {}
impl<T> !IncrementSafe for *mut T {}
