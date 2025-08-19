use std::fmt;

use itertools::Itertools;
use text_size::TextRange;

use super::*;

/// Kind of import statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImportKind<S> {
    /// `import path::xxx as xxx`
    Simple(Option<Box<Ident<S>>>),
    /// `import path::{...}`
    Nested(Vec<ImportItem<S>>),
    /// `import path::*`
    Glob,
}

impl_locatable!(ImportItem);

impl<S: AsRef<str>> fmt::Display for ImportKind<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ImportKind::Simple(alias) => {
                if let Some(alias) = alias {
                    write!(f, " as {alias}")
                } else {
                    write!(f, "")
                }
            }
            ImportKind::Nested(v) => write!(f, "::{{{}}}", v.iter().join(", ")),
            ImportKind::Glob => write!(f, "::*"),
        }
    }
}

/// An import item.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportItem<S> {
    pub name: Ident<S>,
    pub alias: Option<Ident<S>>,
    pub range: TextRange,
}

impl<S: AsRef<str>> fmt::Display for ImportItem<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.alias {
            Some(alias) => write!(f, "{} as {}", self.name, alias),
            None => write!(f, "{}", self.name),
        }
    }
}
