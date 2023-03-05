use std::alloc::{dealloc, Layout};
use std::fmt::Debug;
use std::ptr::NonNull;

use super::{Gc, GcBox, Trace};

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
    pub(crate) unsafe fn sweep(&mut self) {
        let mut new_heap = Vec::new();
        for ptr in &self.0 {
            if ptr.as_ref().marked.get() {
                new_heap.push(*ptr);
            } else {
                ptr.as_ptr().drop_in_place();
                dealloc(
                    ptr.as_ptr() as *mut u8,
                    Layout::for_value::<GcBox<_>>(ptr.as_ref()),
                );
            }
        }
        self.0 = new_heap;
    }

    #[inline]
    pub(crate) fn len(&self) -> usize {
        self.0.len()
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