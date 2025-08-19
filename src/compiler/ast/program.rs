use std::fmt;

use super::*;

/// The root AST node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program<S> {
    pub function: Box<Function<S>>,
}

impl<S: AsRef<str>> fmt::Display for Program<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.function.body)
    }
}
