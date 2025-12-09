use std::fmt::Display;

use itertools::Itertools;

pub(crate) trait Indent: Display {
    fn indent(&self, indent: usize) -> String {
        format!("{self}")
            .split('\n')
            .map(|x| format!("{}{}", " ".repeat(indent), x))
            .join("\n")
    }
}

impl<T: Display> Indent for T {}
