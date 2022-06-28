use cranelift_codegen::{ir::Signature, isa::CallConv};

pub auto trait BitSafe {}

impl<T> !BitSafe for &T {}
impl<T> !BitSafe for &mut T {}
impl<T> !BitSafe for *const T {}
impl<T> !BitSafe for *mut T {}

pub trait BitSerde: Sized {
    fn write(&self, buffer: &mut Vec<u8>);
    fn read(cursor: &mut usize, buffer: &[u8]) -> Result<Self, String>;
    fn size() -> usize {
        std::mem::size_of::<Self>()
    }
}

impl BitSerde for Signature {
    fn write(&self, buffer: &mut Vec<u8>) {
        self.call_conv.write(buffer);
        self.params.write(buffer);
        self.returns.write(buffer);
    }

    fn read(cursor: &mut usize, buffer: &[u8]) -> Result<Self, String> {
        Ok(Self {
            call_conv: CallConv::read(cursor, buffer)?,
            params: Vec::read(cursor, buffer)?,
            returns: Vec::read(cursor, buffer)?,
        })
    }
}

impl BitSerde for String {
    fn write(&self, buffer: &mut Vec<u8>) {
        self.len().write(buffer);
        buffer.extend_from_slice(self.as_bytes());
    }

    fn read(cursor: &mut usize, buffer: &[u8]) -> Result<Self, String> {
        let len = usize::read(cursor, buffer)?;

        if len > buffer.len() {
            return Err(format!(
                "String length {} exceeds buffer length {}",
                len,
                buffer.len()
            ));
        }

        let slice = &buffer[*cursor..*cursor + len];
        *cursor += len;

        String::from_utf8(slice.to_vec()).map_err(|e| e.to_string())
    }

    fn size() -> usize {
        std::mem::size_of::<usize>()
    }
}

impl<T: BitSerde> BitSerde for Vec<T> {
    fn write(&self, buffer: &mut Vec<u8>) {
        self.len().write(buffer);
        for item in self {
            item.write(buffer);
        }
    }

    fn read(cursor: &mut usize, buffer: &[u8]) -> Result<Self, String> {
        let len = usize::read(cursor, buffer)?;

        if len.checked_mul(T::size()).is_none() || len * T::size() > buffer.len() {
            return Err(format!(
                "Vec length {} * {} exceeds buffer length {}",
                len,
                T::size(),
                buffer.len()
            ));
        }
        let mut vec = Vec::with_capacity(len);
        for _ in 0..len {
            vec.push(T::read(cursor, buffer)?);
        }
        Ok(vec)
    }

    fn size() -> usize {
        std::mem::size_of::<usize>()
    }
}

impl<T: BitSafe + Sized + Copy> BitSerde for T {
    fn write(&self, buffer: &mut Vec<u8>) {
        let size = std::mem::size_of::<Self>();

        if buffer.capacity() < buffer.len() + size {
            buffer.reserve(size);
        }

        unsafe {
            std::ptr::write(
                buffer.as_mut_ptr().offset(buffer.len() as isize) as *mut Self,
                *self,
            );
            buffer.set_len(buffer.len() + size);
        }
    }

    fn read(cursor: &mut usize, buffer: &[u8]) -> Result<Self, String> {
        let size = std::mem::size_of::<Self>();

        if *cursor + size > buffer.len() {
            return Err(format!(
                "T::read: cursor({}) + size({}) exceeds buffer length {}",
                *cursor,
                size,
                buffer.len()
            ));
        }

        let result =
            unsafe { std::ptr::read(buffer.as_ptr().offset(*cursor as isize) as *const Self) };
        *cursor += size;
        Ok(result)
    }
}
