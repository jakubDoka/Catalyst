use std::ops::{RangeInclusive, Range};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Border {
    JustBefore(u128),
    AfterMax,
}

impl Border {
    pub fn next(self) -> Border {
        match self {
            Border::JustBefore(value) if value != u128::MAX => Border::JustBefore(value + 1),
            _ => Border::AfterMax,
        }
    }

    pub fn prev(self) -> Border {
        match self {
            Border::JustBefore(value) => Border::JustBefore(value - 1),
            Border::AfterMax => Border::JustBefore(u128::max_value() - 1),
        }
    }
}

pub trait RangeSerde {
    fn encode(&self) -> u128;
    fn decode(encoded: u128) -> Self;
}

pub fn convert_range(range: RangeInclusive<impl RangeSerde>) -> RangeInclusive<u128> {
    let (start, end) = range.into_inner();
    start.encode()..=end.encode()
}

macro_rules! impl_analyzable_range_for_int {
    ($($t:ty),*) => {
        $(
            impl RangeSerde for $t {
                fn encode(&self) -> u128 {
                    (*self as u128).wrapping_sub(i128::MIN as u128)
                }

                fn decode(encoded: u128) -> Self {
                    encoded.wrapping_sub(i128::MIN as u128) as Self
                }
            }
        )*
    };
}

impl_analyzable_range_for_int!(i8, i16, i32, i64, i128, isize);

macro_rules! impl_analyzable_range_for_uint {
    ($($t:ty),*) => {
        $(
            impl RangeSerde for $t {
                fn encode(&self) -> u128 {
                    *self as u128
                }

                fn decode(encoded: u128) -> Self {
                    encoded as Self
                }
            }
        )*
    };
}

impl_analyzable_range_for_uint!(u8, u16, u32, u64, u128, usize);

impl RangeSerde for bool {
    fn encode(&self) -> u128 {
        *self as u128
    }

    fn decode(encoded: u128) -> Self {
        encoded != 0
    }
}

impl RangeSerde for char {
    fn encode(&self) -> u128 {
        *self as u128
    }

    fn decode(encoded: u128) -> Self {
        char::from_u32(encoded as u32).unwrap()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct PatternRange {
    pub start: u128,
    pub end: u128,
}

impl PatternRange {
    pub fn is_one(&self) -> bool {
        self.start == self.end
    }
    
    pub fn new(range: Range<impl RangeSerde>) -> Self {
        Self {
            start: range.start.encode(),
            end: range.end.encode(),
        }
    }

    pub fn decode<T: RangeSerde>(&self) -> (T, T) {
        (T::decode(self.start), T::decode(self.end))
    }

    pub fn intersect(&self, other: &Self) -> Option<Self> {

        let start = self.start.max(other.start);
        let end = self.end.min(other.end);

        if start <= end {
            Some(Self {
                start,
                end,
            })
        } else {
            None
        }
    }

    pub fn into_borders(self) -> (Border, Border) {
        (
            Border::JustBefore(self.start),
            Border::JustBefore(self.end).next(),
        )
    }

    pub fn from_borders(start: Border, end: Border) -> PatternRange {
        match (start, end) {
            (Border::JustBefore(start), Border::JustBefore(end)) => {
                Self::new(start..end - 1)
            }
            (Border::JustBefore(start), Border::AfterMax) => {
                Self::new(start..u128::max_value())
            }
            _ => unreachable!(),
        }
    }

    pub fn start(&self) -> Border {
        Border::JustBefore(self.start)
    }

    pub fn end(&self) -> Border {
        Border::JustBefore(self.end).next()
    }

    pub fn set_start(&mut self, start: Border) {
        *self = Self::from_borders(start, self.end());
    }

    pub fn set_end(&mut self, end: Border) {
        *self = Self::from_borders(self.start(), end);
    }

    pub fn placeholder() -> Self {
        Self { 
            start: u128::MIN, 
            end: u128::MAX, 
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_encode_decode() {
        for i in i8::MIN..=i8::MAX {
            assert_eq!(i as i64, i64::decode(i.encode()));
        }
    }
}
