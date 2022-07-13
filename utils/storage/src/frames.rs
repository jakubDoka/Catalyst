pub struct Frames<T> {
    data: Vec<T>,
    indices: Vec<u32>,
}

impl<T> Frames<T> {
    pub fn new() -> Self {
        Frames {
            data: Vec::new(),
            indices: Vec::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.indices.len()
    }

    pub fn pop(&mut self) -> impl Iterator<Item = T> + '_ {
        let check = self.indices.pop().expect("marked frame");
        self.data.drain(check as usize..)
    }

    pub fn push(&mut self, value: T) {
        self.data.push(value);
    }

    pub fn extend(&mut self, values: impl IntoIterator<Item = T>) {
        self.data.extend(values);
    }

    pub fn mark(&mut self) {
        self.indices.push(self.data.len() as u32);
    }

    pub fn top(&self) -> &[T] {
        self.nth_from_top(0)
    }

    pub fn nth_from_top(&self, n: usize) -> &[T] {
        let inv = self.indices.len() - n - 1;
        let start = self.indices[inv];
        let end = *self
            .indices
            .get(inv + 1)
            .unwrap_or(&(self.data.len() as u32));

        &self.data[start as usize..end as usize]
    }
}

impl<T> Default for Frames<T> {
    fn default() -> Self {
        Frames::new()
    }
}
