pub(crate) mod graph;
pub(crate) mod number_map;

pub(crate) trait IteratorExtension: Iterator {
    fn at_least<A: FnMut(&Self::Item) -> bool>(self, condition: A, expected: usize) -> bool;
}

impl<A: Iterator> IteratorExtension for A {
    fn at_least<B: FnMut(&Self::Item) -> bool>(self, condition: B, expected: usize) -> bool {
        self.filter(condition).take(expected).count() == expected
    }
}
