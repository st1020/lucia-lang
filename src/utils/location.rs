use std::fmt;

/// Location of token in the code.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Location {
    /// The lineno.
    pub lineno: u32,
    /// The column.
    pub column: u32,
    /// The character offset, counting from 0.
    pub offset: u32,
}

impl fmt::Debug for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}({})", self.lineno, self.column, self.offset)
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

impl Default for Location {
    fn default() -> Self {
        Self {
            lineno: 1,
            column: 1,
            offset: 0,
        }
    }
}

pub trait Locatable {
    fn start(&self) -> Location;

    fn end(&self) -> Location;
}
