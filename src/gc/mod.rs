#![allow(clippy::missing_safety_doc)]

pub mod cell;
mod gc_box;
mod heap;
mod trace;

use std::borrow::Borrow;
use std::convert::AsRef;
use std::fmt::{Debug, Display, Pointer};
use std::hash::Hash;
use std::mem;
use std::num::NonZeroUsize;
use std::ops::Deref;
use std::ptr::NonNull;

pub use cell::*;
pub(crate) use gc_box::GcBox;
pub(crate) use heap::Heap;
pub use trace::Trace;

pub struct Gc<T: Trace + ?Sized> {
    pub(crate) ptr: NonNull<GcBox<T>>,
}

impl<T: Trace> Gc<T> {
    #[inline]
    pub(crate) fn new(value: T) -> Self {
        unsafe {
            Gc {
                ptr: NonNull::new_unchecked(Box::into_raw(Box::new(GcBox::new(value)))),
            }
        }
    }

    #[inline]
    pub fn addr(&self) -> NonZeroUsize {
        unsafe { NonZeroUsize::new_unchecked(mem::transmute(self.ptr)) }
    }
}

impl<T: Trace + ?Sized> Gc<T> {
    #[inline]
    fn inner(&self) -> &GcBox<T> {
        unsafe { self.ptr.as_ref() }
    }

    #[inline]
    pub fn is_error(&self) -> bool {
        self.inner().is_error.get()
    }

    #[inline]
    pub fn set_error(&self) {
        self.inner().is_error.set(true)
    }
}

unsafe impl<T: Trace + ?Sized> Trace for Gc<T> {
    unsafe fn trace(&self) {
        self.inner().marked.set(true);
        self.inner().trace()
    }

    unsafe fn marked(&self) -> bool {
        self.inner().marked()
    }
}

impl<T: Trace + ?Sized> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner().data
    }
}

impl<T: Trace + ?Sized> Borrow<T> for Gc<T> {
    fn borrow(&self) -> &T {
        self
    }
}

impl<T: Trace + ?Sized> AsRef<T> for Gc<T> {
    fn as_ref(&self) -> &T {
        self
    }
}

impl<T: Trace> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Self { ptr: self.ptr }
    }
}

impl<T: Trace> Copy for Gc<T> {}

impl<T: Trace + ?Sized> Pointer for Gc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Pointer::fmt(&self.inner(), f)
    }
}

impl<T: Trace + Debug> Debug for Gc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.inner().data)
    }
}

impl<T: Trace + Display> Display for Gc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner().data)
    }
}

impl<T: Trace + PartialEq> PartialEq for Gc<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.inner().data.eq(&other.inner().data)
    }
}

impl<T: Trace> Hash for Gc<T> {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ptr.hash(state);
    }
}

impl<T: Trace + Eq> Eq for Gc<T> {}

impl<T: Trace + PartialOrd> PartialOrd for Gc<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.inner().data.partial_cmp(&other.inner().data)
    }
}

impl<T: Trace + Ord> Ord for Gc<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.inner().data.cmp(&other.inner().data)
    }
}
