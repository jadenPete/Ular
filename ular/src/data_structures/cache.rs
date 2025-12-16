use hashbrown::Equivalent;
use std::{borrow::Borrow, cell::RefCell, hash::Hash};

/// A `Cache` is a hash map that supports a "get or compute if not present" operation.
///
/// What differentiates [get_or_compute] the equivalent operation on [HashMap] is that the former
/// doesn't require a mutable reference to the cache. Instead, it uses interior mutability to comply
/// with borrow rules.
pub(crate) struct Cache<Key, Value> {
    // It's ok to use `RefCell` here because within each function in which we borrow `cached`, the
    // immutable and mutably references' lifetimes are non-overlapping, and they're dropped before the
    // function returns
    cached: RefCell<hashbrown::HashMap<Key, Value>>,
}

impl<Key: Hash + Eq, Value: Copy> Cache<Key, Value> {
    pub(crate) fn get_or_compute<
        'a,
        KeyRef: Eq + Equivalent<Key> + FlexibleToOwned<'a, Owned = Key> + Hash,
        Compute: FnOnce() -> Value,
    >(
        &self,
        key: &'a KeyRef,
        compute: Compute,
    ) -> Value {
        let cached_reference = self.cached.borrow();

        if let Some(&result) = cached_reference.get(key) {
            return result;
        }

        drop(cached_reference);

        let value = compute();

        *self
            .cached
            .borrow_mut()
            .entry(key.to_flexible_owned())
            .insert(value)
            .get()
    }

    pub(crate) fn len(&self) -> usize {
        self.cached.borrow().len()
    }

    pub(crate) fn new() -> Self {
        Self {
            cached: RefCell::new(hashbrown::HashMap::new()),
        }
    }
}

/// A generalization of [Borrow], allowing any borrowed type, not just references.
///
/// `FlexibleBorrow` was built to solve the following problem. Suppose you have a struct like this:
/// ```
/// struct Person {
///     first_name: String,
///     last_name: String,
/// }
/// ```
///
/// in most circumstances, you'd borrow it by creating a `&Person`. However, what if you want to
/// borrow it using a struct like the following?
/// ```
/// struct PersonRef<'a> {
///     first_name: &'a str,
///     last_name: &'a str,
/// }
/// ```
///
/// This may be desirable in a number of circumstances. For example, suppose you're using `Person` as
/// the key to a hash map, and you don't have a `Person` in scope to reference when looking up a value.
/// You could create a new `Person` by cloning your `first_name` and `last_name` references and refer
/// to that, but that would be wasteful if the created `Person` is immediately dropped after the
/// lookup.
///
/// `FlexibleBorrow` accommodates this use case by assuming nothing about the borrowed type.
pub(crate) trait FlexibleBorrow<'a, Borrowed>
where
    Borrowed: ?Sized,
{
    fn flexible_borrow(&'a self) -> Borrowed;
}

impl<'a, Borrowed: ?Sized, Value: Borrow<Borrowed>> FlexibleBorrow<'a, &'a Borrowed> for Value {
    fn flexible_borrow(&'a self) -> &'a Borrowed {
        self.borrow()
    }
}

/// An alternative to [ToOwned] that expects the owned type to implement [FlexibleBorrow] instead of
/// [Borrow].
pub(crate) trait FlexibleToOwned<'a> {
    type Owned;

    fn to_flexible_owned(&'a self) -> Self::Owned;
}

impl<'a, A: ToOwned + ?Sized> FlexibleToOwned<'a> for A {
    type Owned = A::Owned;

    fn to_flexible_owned(&'a self) -> Self::Owned {
        self.to_owned()
    }
}
