use crate::Clear;

/// Holds a Vector of `T` that is split into frames and makes it easy
/// to modify and view them.
pub struct Frames<T> {
    data: Vec<T>,
    indices: Vec<u32>,
}

impl<T> Frames<T> {
    /// Does not allocate.
    pub fn new() -> Self {
        Frames {
            data: Vec::new(),
            indices: Vec::new(),
        }
    }

    /// Returns the number of frames.
    pub fn len(&self) -> usize {
        self.indices.len()
    }

    /// Pops the top frame and returns iterator over its elements.
    pub fn pop(&mut self) -> impl Iterator<Item = T> + '_ {
        let check = self.indices.pop().expect("marked frame");
        self.data.drain(check as usize..)
    }

    /// Pushes value to current frame
    pub fn push(&mut self, value: T) {
        self.data.push(value);
    }

    /// Extends frame by multiple values.
    pub fn extend(&mut self, values: impl IntoIterator<Item = T>) {
        self.data.extend(values);
    }

    /// Marks the end of current frame and beginning of new one.
    pub fn mark(&mut self) {
        self.indices.push(self.data.len() as u32);
    }

    /// Returns contents of the top frame.
    pub fn top(&self) -> &[T] {
        self.nth_from_top(0)
    }

    /// Returns contents of the nth frame from the top.
    pub fn nth_from_top(&self, n: usize) -> &[T] {
        let inv = self.indices.len() - n - 1;
        let start = self.indices[inv];
        let end = *self
            .indices
            .get(inv + 1)
            .unwrap_or(&(self.data.len() as u32));

        &self.data[start as usize..end as usize]
    }

    /// Reports whether there is nothing inside this structure.
    pub fn is_empty(&self) -> bool {
        self.indices.is_empty() && self.data.is_empty()
    }

    /// Joins tow two frames into one.
    pub fn join_frames(&mut self) {
        self.indices.pop().unwrap();
    }

    /// Splits the top frame into two such that `top_frame_length`
    /// matches the `self.top().len()` afterwards.
    ///
    /// # Panics
    ///
    /// Panics if `top_frame_length` is greater than the length of the current top frame.
    pub fn split_at(&mut self, top_frame_length: usize) -> &T {
        assert!(self.top().len() >= top_frame_length);
        self.indices
            .push(self.data.len() as u32 - top_frame_length as u32);
        self.top().iter().rev().nth(top_frame_length - 1).unwrap()
    }
}

impl<T> Default for Frames<T> {
    fn default() -> Self {
        Frames::new()
    }
}

impl<T> Clear for Frames<T> {
    fn clear(&mut self) {
        self.data.clear();
        self.indices.clear();
    }
}
