use super::*;

impl<T: Increment> Increment for Option<T> {
    const DEFAULT: bool = T::DEFAULT;

    fn serialize(&self, serializer: &mut Serializer) {
        crate::increment_shortcut!(write self, serializer, T);

        match self {
            Some(value) => {
                serializer.write(&true);
                Increment::serialize(value, serializer);
            }
            None => {
                serializer.write(&false);
            }
        }
    }

    unsafe fn deserialize(deserializer: &mut Deserializer) -> Self {
        crate::increment_shortcut!(read deserializer, T);

        if deserializer.read() {
            Some(Increment::deserialize(deserializer))
        } else {
            None
        }
    }

    fn size(&self) -> usize {
        crate::increment_shortcut!(len Self, T);

        std::mem::size_of::<bool>()
            + match self {
                Some(value) => Increment::size(value),
                None => 0,
            }
    }
}

impl<T: Increment> Increment for Vec<T> {
    fn serialize(&self, serializer: &mut Serializer) {
        serializer.write(&self.len());

        if T::DEFAULT {
            return serializer.write_slice(self);
        } else {
            for value in self {
                Increment::serialize(value, serializer);
            }
        }
    }

    unsafe fn deserialize(deserializer: &mut Deserializer) -> Self {
        let len = deserializer.read();

        let mut vec = Vec::with_capacity(len);

        if T::DEFAULT {
            deserializer.read_slice(len, vec.as_mut_ptr());
            vec.set_len(len);
        } else {
            for _ in 0..len {
                vec.push(Increment::deserialize(deserializer));
            }
        }

        vec
    }

    fn size(&self) -> usize {
        std::mem::size_of::<usize>()
            + if T::DEFAULT {
                std::mem::size_of::<T>() * self.len()
            } else {
                self.iter().map(Increment::size).sum()
            }
    }
}
