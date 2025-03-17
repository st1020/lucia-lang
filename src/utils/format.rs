use std::fmt::{Display, Write};

pub trait Join<Item: Display>: Iterator<Item = Item> {
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

pub trait Indent: Display {
    fn indent(&self, indent: usize) -> String {
        format!("{}", self)
            .split('\n')
            .map(|x| format!("{}{}", " ".repeat(indent), x))
            .join("\n")
    }
}

impl<T: Display> Indent for T {}
