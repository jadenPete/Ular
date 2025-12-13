use std::{cell::RefCell, collections::HashMap, hash::Hash};

pub(crate) struct Cache<Key, Value> {
    // It's ok to use `RefCell` here because within each function in which we borrow `cached`, the
    // immutable and mutably references' lifetimes are non-overlapping, and they're dropped before the
    // function returns
    cached: RefCell<HashMap<Key, Value>>,
}

impl<Key: Hash + Eq, Value: Copy> Cache<Key, Value> {
    pub(crate) fn get_or_compute<A: FnOnce() -> Value>(&self, key: Key, compute: A) -> Value {
        let cached_reference = self.cached.borrow();

        if let Some(&result) = cached_reference.get(&key) {
            return result;
        }

        drop(cached_reference);

        let value = compute();

        *self
            .cached
            .borrow_mut()
            .entry(key)
            .insert_entry(value)
            .get()
    }

    pub(crate) fn len(&self) -> usize {
        self.cached.borrow().len()
    }

    pub(crate) fn new() -> Self {
        Self {
            cached: RefCell::new(HashMap::new()),
        }
    }
}
