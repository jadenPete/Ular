#[repr(C)]
pub enum FfiOption<A> {
    None,
    Some(A),
}

impl<A> FfiOption<A> {
    #[inline]
    pub fn into_option(self) -> Option<A> {
        match self {
            FfiOption::None => None,
            FfiOption::Some(value) => Some(value),
        }
    }
}
