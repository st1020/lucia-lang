//! The string interner.

use std::{borrow::Borrow, collections::HashSet, fmt::Display, hash::Hash, rc::Rc};

use compact_str::CompactString;
use rustc_hash::FxBuildHasher;

/// The string interner.
pub trait StringInterner {
    type String: Clone + Eq + Hash + Display;

    fn intern(&mut self, s: &str) -> Self::String;
}

impl<S: StringInterner> StringInterner for &mut S {
    type String = S::String;

    fn intern(&mut self, s: &str) -> Self::String {
        S::intern(self, s)
    }
}

/// No-op string interner.
#[derive(Debug, Default)]
pub struct NoopInterner;

impl StringInterner for NoopInterner {
    type String = String;

    fn intern(&mut self, s: &str) -> Self::String {
        s.to_owned()
    }
}

/// A basic string interner.
#[derive(Debug, Default)]
pub struct BasicInterner(HashSet<Rc<str>, FxBuildHasher>);

impl StringInterner for BasicInterner {
    type String = Rc<str>;

    fn intern(&mut self, s: &str) -> Self::String {
        if let Some(s) = self.0.get(s) {
            Rc::clone(s)
        } else {
            let s = Rc::from(Box::from(s));
            self.0.insert(Rc::clone(&s));
            s
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct RcCompactStringWrapper(Rc<CompactString>);

impl Borrow<str> for RcCompactStringWrapper {
    fn borrow(&self) -> &str {
        self.0.as_str()
    }
}

/// A CompactString interner.
#[derive(Debug, Default)]
pub struct CompactInterner(HashSet<RcCompactStringWrapper, FxBuildHasher>);

impl StringInterner for CompactInterner {
    type String = Rc<CompactString>;

    fn intern(&mut self, s: &str) -> Self::String {
        if let Some(s) = self.0.get(s) {
            s.clone().0
        } else {
            let s = Rc::new(CompactString::from(s));
            self.0.insert(RcCompactStringWrapper(Rc::clone(&s)));
            s
        }
    }
}
