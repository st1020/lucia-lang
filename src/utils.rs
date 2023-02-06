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

pub fn escape_str(value: &str, ascii_only: bool) -> String {
    let mut ans = String::new();
    for c in value.chars() {
        match c {
            '\0' => ans.push_str("\\0"),
            '\t' => ans.push_str("\\t"),
            '\r' => ans.push_str("\\t"),
            '\n' => ans.push_str("\\n"),
            '\\' => ans.push_str("\\\\"),
            '"' => ans.push_str("\\\""),
            '\'' => ans.push_str("\\\'"),
            '\x20'..='\x7e' if ascii_only => ans.push(c),
            _ if ascii_only => ans.push_str(&c.escape_default().to_string()),
            _ => ans.push(c),
        }
    }
    ans
}
