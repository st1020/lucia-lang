//! The string interner.

use std::{borrow::Borrow, collections::HashSet, fmt, hash, ops};

use bumpalo::{collections::String, Bump};
use rustc_hash::FxBuildHasher;

/// The string interner.
///
/// The interned string must implement `Copy` because a interned string is used in bumpalo's bump
/// arena. If the interned string is `Arc<str>`, when bump arena freed, the `Drop` will not be
/// called, which will cause the memory leak.
pub trait StringInterner {
    type String: AsRef<str> + Copy;

    fn intern(&mut self, s: &str) -> Self::String;
}

impl<'a, S: StringInterner> StringInterner for &'a mut S {
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

/// An interned atom string, a string pointer in bumpalo's bump arena.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Atom<'a>(&'a String<'a>);

impl<'a> Atom<'a> {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl ops::Deref for Atom<'_> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl AsRef<str> for Atom<'_> {
    fn as_ref(&self) -> &str {
        self.0.as_str()
    }
}

impl Borrow<str> for Atom<'_> {
    fn borrow(&self) -> &str {
        self.0.borrow()
    }
}

impl fmt::Display for Atom<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

/// The bump string interner.
pub struct BumpInterner<'a> {
    allocator: &'a Bump,
    interner: HashSet<Atom<'a>, FxBuildHasher>,
}

impl<'a> BumpInterner<'a> {
    pub fn new(allocator: &'a Bump) -> Self {
        Self {
            allocator,
            interner: HashSet::with_hasher(FxBuildHasher),
        }
    }
}

impl<'a> StringInterner for BumpInterner<'a> {
    type String = Atom<'a>;

    fn intern(&mut self, s: &str) -> Self::String {
        if let Some(s) = self.interner.get(s) {
            *s
        } else {
            let s = Atom(self.allocator.alloc(String::from_str_in(s, self.allocator)));
            self.interner.insert(s);
            s
        }
    }
}
