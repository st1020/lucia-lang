//! The string interner.

use core::fmt;
use std::{borrow::Borrow, collections::HashSet, hash, rc::Rc};

use rustc_hash::FxBuildHasher;

/// The string interner.
pub trait StringInterner {
    type String: AsRef<str> + Clone;

    fn intern(&mut self, s: &str) -> Self::String;
}

impl<S: StringInterner> StringInterner for &mut S {
    type String = S::String;

    fn intern(&mut self, s: &str) -> Self::String {
        S::intern(self, s)
    }
}

/// An interned string.
///
/// Usually used in a interned string must be used as a key in a hash map.
#[derive(Debug, Clone, Copy)]
pub struct InternedString<S>(pub S);

impl<S: AsRef<str>> PartialEq for InternedString<S> {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ref() == other.0.as_ref()
    }
}

impl<S: AsRef<str>> Eq for InternedString<S> {}

impl<S: AsRef<str>> PartialOrd for InternedString<S> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<S: AsRef<str>> Ord for InternedString<S> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.as_ref().cmp(other.0.as_ref())
    }
}

impl<S: AsRef<str>> hash::Hash for InternedString<S> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.0.as_ref().hash(state);
    }
}

impl<S> From<S> for InternedString<S> {
    fn from(value: S) -> Self {
        InternedString(value)
    }
}

impl<S: AsRef<str>> AsRef<str> for InternedString<S> {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl<S: AsRef<str>> Borrow<str> for InternedString<S> {
    fn borrow(&self) -> &str {
        self.as_ref()
    }
}

impl<S: AsRef<str>> fmt::Display for InternedString<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0.as_ref())
    }
}

/// A basic string interner.
#[derive(Debug, Default)]
pub struct BasicInterner(HashSet<Rc<str>, FxBuildHasher>);

impl StringInterner for BasicInterner {
    type String = Rc<str>;

    fn intern(&mut self, s: &str) -> Self::String {
        if let Some(s) = self.0.get(s) {
            s.clone()
        } else {
            let s = Rc::from(Box::from(s));
            self.0.insert(Rc::clone(&s));
            s
        }
    }
}
