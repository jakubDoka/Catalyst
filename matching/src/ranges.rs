use std::ops::RangeInclusive;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IntBorder {
    JustBefore(u128),
    AfterMax,
}

impl IntBorder {
    pub fn next(self) -> IntBorder {
        match self {
            IntBorder::JustBefore(value) if value != u128::MAX => IntBorder::JustBefore(value + 1),
            _ => IntBorder::AfterMax,
        }
    }

    pub fn prev(self) -> IntBorder {
        match self {
            IntBorder::JustBefore(value) => IntBorder::JustBefore(value - 1),
            IntBorder::AfterMax => IntBorder::JustBefore(u128::max_value() - 1),
        }
    }
}

pub trait AnalyzableRange {
    fn encode(&self) -> u128;
    fn decode(encoded: u128) -> Self;
}

pub fn convert_range(range: RangeInclusive<impl AnalyzableRange>) -> RangeInclusive<u128> {
    let (start, end) = range.into_inner();
    start.encode()..=end.encode()
}

macro_rules! impl_analyzable_range_for_int {
    ($($t:ty),*) => {
        $(
            impl AnalyzableRange for $t {
                fn encode(&self) -> u128 {
                    (*self as u128).wrapping_sub(Self::MIN as u128)
                }

                fn decode(encoded: u128) -> Self {
                    encoded.wrapping_sub(Self::MIN as u128) as Self 
                }
            }
        )*
    };
}

impl_analyzable_range_for_int!(i8, i16, i32, i64, i128, isize);

macro_rules! impl_analyzable_range_for_uint {
    ($($t:ty),*) => {
        $(
            impl AnalyzableRange for $t {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct IntRange {
    pub start: u128,
    pub end: u128,
    pub bias: u128,
}

impl IntRange {
    pub fn new(range: RangeInclusive<impl AnalyzableRange>, bias: u128) -> Self {
        Self {
            start: range.start().encode(),
            end: range.end().encode(),
            bias,
        }
    }

    pub fn decode<T: AnalyzableRange>(&self) -> (T, T) {
        (T::decode(self.start), T::decode(self.end))
    }

    pub fn intersect(&self, other: &Self) -> Option<Self> {
        assert!(self.bias == other.bias);
        
        let start = self.start.max(other.start);
        let end = self.end.min(other.end);

        if start <= end {
            Some(Self {
                start,
                end,
                bias: self.bias,
            })
        } else {
            None
        }
    }

    pub fn into_borders(self) -> (IntBorder, IntBorder) {
        (IntBorder::JustBefore(self.start), IntBorder::JustBefore(self.end).next())
    }

    pub fn from_borders(start: IntBorder, end: IntBorder, bias: u128) -> IntRange {
        match (start, end) {
            (IntBorder::JustBefore(start), IntBorder::JustBefore(end)) => Self::new(start..=end - 1, bias),
            (IntBorder::JustBefore(start), IntBorder::AfterMax) => Self::new(start..=u128::max_value(), bias),
            _ => unreachable!(),
        }
    }

    pub fn start(&self) -> IntBorder {
        IntBorder::JustBefore(self.start)
    }

    pub fn end(&self) -> IntBorder {
        IntBorder::JustBefore(self.end).next()
    }

    pub fn set_start(&mut self, start: IntBorder) {
        *self = Self::from_borders(start, self.end(), self.bias);
    }

    pub fn set_end(&mut self, end: IntBorder) {
        *self = Self::from_borders(self.start(), end, self.bias);
    }

    pub fn new_bounds(range: &RangeInclusive<i32>) -> IntRange {
        let start = range.start().encode();
        let end = range.end().encode();
        let bias = end - start;
        Self { start, end, bias }
    }

    pub fn coverage(&self, coverage: &RangeInclusive<i32>) -> IntRange {
        let start = coverage.start().encode();
        let end = coverage.end().encode();
        Self { start, end, bias: self.bias }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_encode_decode() {
        for i in i8::MIN..=i8::MAX {
            assert_eq!(i, i8::decode(i.encode()));
        }
    }
}