use std::fmt::{Debug, Display, Write};

use crate::objects::Value;

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
            '\r' => ans.push_str("\\r"),
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

pub(crate) trait Join<Item: Display>: Iterator<Item = Item> {
    fn join(&mut self, sep: &str) -> String {
        if let Some(first) = self.next() {
            let (lb, _) = self.size_hint();
            let mut result = String::with_capacity(sep.len() * lb);
            write!(&mut result, "{}", first).unwrap();
            self.for_each(|i| {
                result.push_str(sep);
                write!(&mut result, "{}", i).unwrap();
            });
            result
        } else {
            String::new()
        }
    }
}

impl<T: ?Sized, Item: Display> Join<Item> for T where T: Iterator<Item = Item> {}

/// A wrap of `&Value`, which `fmt::Debug` is same as `fmt::Display`.
#[derive(Clone)]
pub(crate) struct ValueDebug<'a>(pub &'a Value);

impl Debug for ValueDebug<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A wrap of `&Vec<Value>`, avoid `fmt::Debug` infinite recursion when circular references occur.
#[derive(Clone)]
pub(crate) struct ValueVecDebug<'a>(pub &'a Vec<Value>);

impl Debug for ValueVecDebug<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(self.0.iter().map(ValueDebug))
            .finish()
    }
}
