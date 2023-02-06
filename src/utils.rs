use std::fmt::{Debug, Display};

/// Location of token in the code.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Location {
    pub lineno: u32,
    pub column: u32,
    pub offset: u32,
}

impl Debug for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}({})", self.lineno, self.column, self.offset)
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}
