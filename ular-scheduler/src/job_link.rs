use intrusive_collections::{DefaultLinkOps, linked_list::LinkOps};
use std::{cell::Cell, ptr::NonNull};

// Use a special value to indicate an unlinked node
const UNLINKED_MARKER: Option<NonNull<JobLink>> =
    unsafe { Some(NonNull::new_unchecked(1 as *mut JobLink)) };

/// Basically just a `repr(C)` version of [intrusive_collections::linked_list::Link]
#[repr(C)]
pub struct JobLink {
    next: Cell<Option<NonNull<JobLink>>>,
    prev: Cell<Option<NonNull<JobLink>>>,
}

impl DefaultLinkOps for JobLink {
    type Ops = LinkOps;

    const NEW: Self::Ops = LinkOps;
}

impl JobLink {
    #[inline]
    pub fn is_linked(&self) -> bool {
        self.next.get() != UNLINKED_MARKER
    }

    #[inline]
    pub fn new() -> Self {
        Self {
            next: Cell::new(UNLINKED_MARKER),
            prev: Cell::new(UNLINKED_MARKER),
        }
    }
}

unsafe impl Send for JobLink {}
