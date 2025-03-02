//! The index types used in the compiler.

use std::{fmt, num::NonZeroU32};

use index_vec::Idx;

macro_rules! define_index {
    ($($name:ident),*) => {$(
        #[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
        pub struct $name(NonZeroU32);

        impl $name {
            pub const fn new(idx: u32) -> Self {
                assert!(idx != u32::MAX);
                unsafe { Self::new_unchecked(idx) }
            }

            #[allow(clippy::missing_safety_doc)]
            pub const unsafe fn new_unchecked(idx: u32) -> Self {
                Self(unsafe {NonZeroU32::new_unchecked(idx ^ u32::MAX)})
            }

            pub const fn get(&self) -> u32 {
                self.0.get() ^ u32::MAX
            }
        }

        impl Idx for $name {
            fn from_usize(idx: usize) -> Self {
                Self::new(idx as u32)
            }

            fn index(self) -> usize {
                self.get() as usize
            }
        }

        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}", self.get())
            }
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}", self.get())
            }
        }

        impl Default for $name {
            fn default() -> Self {
                unsafe{ Self::new_unchecked(0) }
            }
        }
    )*};
}

define_index!(FunctionId, ScopeId, SymbolId, ReferenceId);
