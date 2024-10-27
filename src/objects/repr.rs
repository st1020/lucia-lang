use std::collections::HashSet;

use compact_str::{format_compact, CompactString, ToCompactString};
use gc_arena::Gc;

use crate::{
    objects::{Callback, Closure, Function, Str, Table, UserData, Value},
    utils::{escape_str, Float},
};

pub trait Repr {
    fn repr(&self) -> CompactString;
}

macro_rules! impl_display_repr {
    ($($t:ty),* $(,)?) => {
        $(
            impl Repr for $t {
                fn repr(&self) -> CompactString {
                    self.to_compact_string()
                }
            }
        )*
    };
}
impl_display_repr!(
    bool,
    i64,
    Float,
    Closure<'_>,
    Callback<'_>,
    Function<'_>,
    UserData<'_>,
);

impl Repr for Str<'_> {
    fn repr(&self) -> CompactString {
        format_compact!("\"{}\"", escape_str(self, false))
    }
}

impl Repr for Table<'_> {
    fn repr(&self) -> CompactString {
        enum ValueOrStr<'gc> {
            Value(Value<'gc>),
            Str(&'static str),
        }
        let mut result = CompactString::new("");
        let mut visited = HashSet::new();
        let mut stack = vec![ValueOrStr::Value(Value::Table(*self))];
        while let Some(current) = stack.pop() {
            match current {
                ValueOrStr::Value(Value::Table(t)) => {
                    let id = Gc::as_ptr(t.into_inner()) as *const ();
                    if visited.contains(&id) {
                        result.push_str("<table>");
                        continue;
                    }
                    visited.insert(id);
                    stack.push(ValueOrStr::Str("}"));
                    for i in (0..t.len()).rev() {
                        let Some((k, v)) = self.get_index(i) else {
                            continue;
                        };
                        if i != t.len() - 1 {
                            stack.push(ValueOrStr::Str(", "));
                        }
                        stack.push(ValueOrStr::Value(v));
                        stack.push(ValueOrStr::Str(": "));
                        stack.push(ValueOrStr::Value(k));
                    }
                    stack.push(ValueOrStr::Str("{"));
                }
                ValueOrStr::Value(v) => result.push_str(&v.repr()),
                ValueOrStr::Str(s) => result.push_str(s),
            }
        }
        result
    }
}

impl Repr for Value<'_> {
    fn repr(&self) -> CompactString {
        match self {
            Value::Null => CompactString::const_new("null"),
            Value::Bool(v) => v.repr(),
            Value::Int(v) => v.repr(),
            Value::Float(v) => v.repr(),
            Value::Str(v) => v.repr(),
            Value::Table(v) => v.repr(),
            Value::Function(v) => v.repr(),
            Value::UserData(v) => v.repr(),
        }
    }
}
