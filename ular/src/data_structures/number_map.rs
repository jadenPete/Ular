use std::{
    cell::UnsafeCell,
    fmt::{Debug, Formatter},
};

pub struct NumberMap<A> {
    pub offset: usize,
    values: UnsafeCell<Vec<Option<A>>>,
}

impl<A> NumberMap<A> {
    fn ensure_defined(&self, key: usize) {
        if key < self.offset {
            panic!(
                "Attempted to write to the key {} in a map with offset {}.",
                key, self.offset
            );
        }

        /*
         * SAFETY: Logically, this is immutable, because although we're modifying `self.values`,
         * we're only inserting *new* elements. Those new elements are `None`, and an element not
         * existing at index `key` and one existing whose value is `None` both imply that the key
         * `key` doesn't exist.
         */
        let mut_self = unsafe { &mut *self.values.get() };

        for _ in mut_self.len()..=key - self.offset {
            mut_self.push(None);
        }
    }

    fn get_values(&self) -> &Vec<Option<A>> {
        /*
         * SAFETY: Getting an immutable reference to `self.values`, given an immutable reference
         * `self`, was always safe.
         */
        unsafe { &*self.values.get() }
    }

    fn get_values_mut(&mut self) -> &mut Vec<Option<A>> {
        /*
         * SAFETY: Getting a mutable reference to `self.values`, given a mutable reference `self`,
         * was always safe.
         */
        unsafe { &mut *self.values.get() }
    }

    pub fn contains_key(&self, key: usize) -> bool {
        let values = self.get_values();

        key - self.offset < values.len() && values[key - self.offset].is_some()
    }

    pub fn get(&self, key: usize) -> Option<&A> {
        self.get_values()
            .get(key - self.offset)
            .and_then(|value| value.as_ref())
    }

    pub fn get_mut(&mut self, key: usize) -> Option<&mut A> {
        let offset = self.offset;

        self.get_values_mut()
            .get_mut(key - offset)
            .and_then(|value| value.as_mut())
    }

    pub fn get_or_insert_with<F: FnOnce() -> A>(&mut self, key: usize, default: F) -> &mut A {
        let offset = self.offset;

        self.ensure_defined(key);
        self.get_values_mut()[key - offset].get_or_insert_with(default)
    }

    pub fn insert(&mut self, key: usize, value: A) -> &mut A {
        let offset = self.offset;

        self.ensure_defined(key);

        let values = self.get_values_mut();
        let i = key - offset;

        values[i] = Some(value);
        values[i].as_mut().unwrap()
    }

    /// Returns a consuming iterator of each entry of the map. [NumberMap] doesn't implement
    /// [IntoIterator] because Rust doesn't yet support `impl` types in type aliases, which we'd need
    /// in order to set `IntoIter` to `impl Iterator<Item = (usize, A)>`.
    ///
    /// See [the tracking issue](https://github.com/rust-lang/rust/issues/63063) for more information.
    pub fn into_iter(self) -> impl Iterator<Item = (usize, A)> {
        let offset = self.offset;

        self.into_values()
            .into_iter()
            .enumerate()
            .flat_map(move |(i, value)| value.map(|value| (i + offset, value)))
    }

    pub fn into_contiguous_values(
        self,
        expected_length: usize,
    ) -> Result<Vec<A>, IndexUndefinedError> {
        let mut result = Vec::with_capacity(expected_length);

        for (i, (j, value)) in self.into_iter().enumerate() {
            if j > i {
                return Err(IndexUndefinedError { index: i });
            }

            result.push(value);
        }

        if result.len() < expected_length {
            return Err(IndexUndefinedError {
                index: result.len(),
            });
        }

        Ok(result)
    }

    fn into_values(self) -> Vec<Option<A>> {
        self.values.into_inner()
    }

    pub fn iter(&self) -> impl Iterator<Item = (usize, &A)> {
        self.get_values()
            .iter()
            .enumerate()
            .flat_map(|(i, value)| value.as_ref().map(|value| (i + self.offset, value)))
    }

    pub fn new(offset: usize) -> Self {
        Self {
            offset,
            values: UnsafeCell::new(Vec::new()),
        }
    }
}

impl<A: Debug> Debug for NumberMap<A> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        formatter.debug_map().entries(self.iter()).finish()
    }
}

pub struct IndexUndefinedError {
    pub index: usize,
}
