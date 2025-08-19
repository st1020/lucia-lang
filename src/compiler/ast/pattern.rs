use std::fmt;

use itertools::Itertools;
use text_size::TextRange;

use super::*;

/// A pattern.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pattern<S> {
    pub kind: PatternKind<S>,
    pub range: TextRange,
}

impl_locatable!(Pattern);

impl<S: AsRef<str>> fmt::Display for Pattern<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

/// Kind of pattern.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatternKind<S> {
    Lit(Box<Lit<S>>),
    Ident(Box<Ident<S>>),
    Table {
        pairs: Vec<TablePatternPair<S>>,
        others: Option<TextRange>,
    },
    List {
        items: Vec<Pattern<S>>,
        others: Option<TextRange>,
    },
}

impl<S: AsRef<str>> fmt::Display for PatternKind<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PatternKind::Lit(lit) => write!(f, "{lit}"),
            PatternKind::Ident(ident) => write!(f, "{ident}"),
            PatternKind::Table { pairs, others } => write!(
                f,
                "{{{}{}}}",
                pairs.iter().join(", "),
                if others.is_some() { ", ..." } else { "" }
            ),
            PatternKind::List { items, others } => write!(
                f,
                "[{}{}]",
                items.iter().join(", "),
                if others.is_some() { ", ..." } else { "" }
            ),
        }
    }
}

/// A table pattern pair.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TablePatternPair<S> {
    pub key: Lit<S>,
    pub value: Pattern<S>,
    pub range: TextRange,
}

impl_locatable!(TablePatternPair);

impl<S: AsRef<str>> fmt::Display for TablePatternPair<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.key, self.value)
    }
}
