use std::fmt;

use text_size::TextRange;

use crate::utils::{Float, escape_str};

use super::*;

/// A literal.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lit<S> {
    pub kind: LitKind<S>,
    pub range: TextRange,
}

impl_locatable!(Lit);
impl_kind_display!(Lit);

/// Kind of literal.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LitKind<S> {
    /// "null"
    Null,
    /// "true", "false"
    Bool(bool),
    /// "12", "0o100", "0b110"
    Int(i64),
    /// "12.34", "0b100.100"
    Float(Float),
    /// ""abc"", ""abc"
    Str(S),
}

impl<S: AsRef<str>> fmt::Display for LitKind<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LitKind::Null => write!(f, "null"),
            LitKind::Bool(v) => write!(f, "{v}"),
            LitKind::Int(v) => write!(f, "{v}"),
            LitKind::Float(v) => write!(f, "{v:?}"),
            LitKind::Str(v) => write!(f, "\"{}\"", escape_str(v.as_ref(), false)),
        }
    }
}
