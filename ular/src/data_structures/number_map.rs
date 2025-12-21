use std::fmt::{Debug, Formatter};

pub(crate) struct NumberMap<A> {
    offset: usize,
    values: Vec<Option<A>>,
}

impl<A> NumberMap<A> {
    fn ensure_defined(&mut self, key: usize) {
        if key < self.offset {
            panic!(
                "Attempted to write to the key {} in a map with offset {}.",
                key, self.offset
            );
        }

        for _ in self.values.len()..=key - self.offset {
            self.values.push(None);
        }
    }

    pub(crate) fn contains_key(&self, key: usize) -> bool {
        key - self.offset < self.values.len() && self.values[key - self.offset].is_some()
    }

    pub(crate) fn get(&self, key: usize) -> Option<&A> {
        self.values.get(key.checked_sub(self.offset)?)?.as_ref()
    }

    pub(crate) fn get_mut(&mut self, key: usize) -> Option<&mut A> {
        let offset = self.offset;

        self.values.get_mut(key.checked_sub(offset)?)?.as_mut()
    }

    pub(crate) fn get_or_insert_with<F: FnOnce() -> A>(
        &mut self,
        key: usize,
        default: F,
    ) -> &mut A {
        let offset = self.offset;

        self.ensure_defined(key);
        self.values[key - offset].get_or_insert_with(default)
    }

    pub(crate) fn insert(&mut self, key: usize, value: A) -> &mut A {
        let offset = self.offset;

        self.ensure_defined(key);

        let i = key - offset;

        self.values[i] = Some(value);
        self.values[i].as_mut().unwrap()
    }

    /// Returns a consuming iterator of each entry of the map. [NumberMap] doesn't implement
    /// [IntoIterator] because Rust doesn't yet support `impl` types in type aliases, which we'd need
    /// in order to set `IntoIter` to `impl Iterator<Item = (usize, A)>`.
    ///
    /// See [the tracking issue](https://github.com/rust-lang/rust/issues/63063) for more information.
    pub(crate) fn into_iter(self) -> impl Iterator<Item = (usize, A)> {
        let offset = self.offset;

        self.values
            .into_iter()
            .enumerate()
            .flat_map(move |(i, value)| value.map(|value| (i + offset, value)))
    }

    pub(crate) fn into_contiguous_values(
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

    pub(crate) fn iter(&self) -> impl Iterator<Item = (usize, &A)> {
        self.values
            .iter()
            .enumerate()
            .flat_map(|(i, value)| value.as_ref().map(|value| (i + self.offset, value)))
    }

    pub(crate) fn new(offset: usize) -> Self {
        Self {
            offset,
            values: Vec::new(),
        }
    }

    pub(crate) fn values(&self) -> impl Iterator<Item = &A> {
        self.values.iter().flatten()
    }
}

impl<A: Debug> Debug for NumberMap<A> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        formatter.debug_map().entries(self.iter()).finish()
    }
}

pub(crate) struct IndexUndefinedError {
    pub(crate) index: usize,
}
