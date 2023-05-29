use std::fmt::Debug;
use std::ptr::NonNull;

use super::{Gc, GcBox, Trace};

// A Heap. All Gc<T> should allocate on a Heap.
pub(crate) struct Heap(Vec<NonNull<GcBox<dyn Trace>>>);

impl Heap {
    #[inline]
    pub(crate) fn new() -> Self {
        Self(Vec::new())
    }

    #[inline]
    pub(crate) fn new_gc_object<T: Trace + 'static>(&mut self, value: T) -> Gc<T> {
        let t = Gc::new(value);
        self.0.push(t.ptr);
        t
    }

    #[inline]
    pub(crate) unsafe fn unmark(&mut self) {
        for ptr in &self.0 {
            ptr.as_ref().unmark()
        }
    }

    #[inline]
    pub(crate) unsafe fn sweep(&mut self) {
        let mut new_heap = Vec::new();
        for ptr in &self.0 {
            if ptr.as_ref().marked.get() {
                new_heap.push(*ptr);
            } else {
                drop(Box::from_raw(ptr.as_ptr()));
            }
        }
        self.0 = new_heap;
    }

    #[inline]
    pub(crate) fn len(&self) -> usize {
        self.0.len()
    }
}

impl Drop for Heap {
    fn drop(&mut self) {
        unsafe { self.sweep() }
    }
}

impl Default for Heap {
    fn default() -> Self {
        Self::new()
    }
}

impl Debug for Heap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(self.0.iter().map(|p| p.as_ptr()))
            .finish()
    }
}
