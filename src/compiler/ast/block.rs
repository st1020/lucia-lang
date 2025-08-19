use std::{fmt, sync::OnceLock};

use text_size::TextRange;

use crate::{compiler::index::ScopeId, utils::Indent};

use super::*;

/// A block.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block<S> {
    pub body: Vec<Expr<S>>,
    pub range: TextRange,
    pub scope_id: OnceLock<ScopeId>,
}

impl_locatable!(Block);

impl<S: AsRef<str>> fmt::Display for Block<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{{")?;
        for stmt in &self.body {
            writeln!(f, "{}", stmt.indent(4))?;
        }
        write!(f, "}}")
    }
}
