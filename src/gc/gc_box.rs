use std::cell::Cell;

use super::Trace;

/// A garbage collected allocation.
#[derive(Debug)]
pub(crate) struct GcBox<T: Trace + ?Sized> {
    pub(crate) marked: Cell<bool>,
    pub(crate) is_error: Cell<bool>,
    pub(crate) data: T,
}

impl<T: Trace> GcBox<T> {
    #[inline]
    pub(crate) fn new(value: T) -> Self {
        GcBox {
            data: value,
            marked: Cell::new(false),
            is_error: Cell::new(false),
        }
    }
}

impl<T: Trace + ?Sized> GcBox<T> {
    #[inline]
    pub(crate) unsafe fn unmark(&self) {
        self.marked.set(false);
    }
}

unsafe impl<T: Trace + ?Sized> Trace for GcBox<T> {
    #[inline]
    unsafe fn trace(&self) {
        self.marked.set(true);
        self.data.trace();
    }

    #[inline]
    unsafe fn marked(&self) -> bool {
        self.marked.get() || self.data.marked()
    }
}
