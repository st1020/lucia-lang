use compact_str::ToCompactString;

use crate::objects::{CallbackInner, Str, Table, TableEntries, TableInner, Value};

pub fn string_lib() -> Table {
    let mut t = TableInner::new();
    t.set(
        "get",
        CallbackInner::from_fn(|s: Str, i: usize| s.chars().nth(i).map(|x| x.to_compact_string())),
    );
    t.set(
        "chars",
        CallbackInner::from_fn(|s: Str| {
            s.chars()
                .map(|x| Value::from(x.to_compact_string()))
                .collect::<TableEntries>()
        }),
    );
    t.set(
        "lines",
        CallbackInner::from_fn(|s: Str| s.lines().map(Value::from).collect::<TableEntries>()),
    );
    t.set(
        "contains",
        CallbackInner::from_fn(|s1: Str, s2: Str| s1.contains(s2.as_str())),
    );
    t.set(
        "starts_with",
        CallbackInner::from_fn(|s1: Str, s2: Str| s1.starts_with(s2.as_str())),
    );
    t.set(
        "ends_with",
        CallbackInner::from_fn(|s1: Str, s2: Str| s1.ends_with(s2.as_str())),
    );
    t.set(
        "find",
        CallbackInner::from_fn(|s1: Str, s2: Str| (s1.find(s2.as_str())).map(Value::from)),
    );
    t.set(
        "split",
        CallbackInner::from_fn(|s: Str, pat: Str| {
            s.split(pat.as_str())
                .map(Value::from)
                .collect::<TableEntries>()
        }),
    );
    t.set(
        "splitn",
        CallbackInner::from_fn(|s: Str, pat: Str, count: usize| {
            s.splitn(count, pat.as_str())
                .map(Value::from)
                .collect::<TableEntries>()
        }),
    );
    t.set(
        "trim",
        CallbackInner::from_fn(|s: Str| s.trim().to_compact_string()),
    );
    t.set(
        "trim_start",
        CallbackInner::from_fn(|s: Str| s.trim_start().to_compact_string()),
    );
    t.set(
        "trim_end",
        CallbackInner::from_fn(|s: Str| s.trim_end().to_compact_string()),
    );
    t.set(
        "strip_prefix",
        CallbackInner::from_fn(|s1: Str, s2: Str| {
            s1.strip_prefix(s2.as_str()).map(|x| x.to_compact_string())
        }),
    );
    t.set(
        "strip_suffix",
        CallbackInner::from_fn(|s1: Str, s2: Str| {
            s1.strip_suffix(s2.as_str()).map(|x| x.to_compact_string())
        }),
    );
    t.set("is_ascii", CallbackInner::from_fn(|s: Str| s.is_ascii()));
    t.set(
        "replace",
        CallbackInner::from_fn(|s: Str, from: Str, to: Str| s.replace(from.as_str(), to.as_str())),
    );
    t.set(
        "replacen",
        CallbackInner::from_fn(|s: Str, from: Str, to: Str, count: usize| {
            s.replacen(from.as_str(), to.as_str(), count)
        }),
    );
    t.set(
        "to_lowercase",
        CallbackInner::from_fn(|s: Str| s.to_lowercase()),
    );
    t.set(
        "to_uppercase",
        CallbackInner::from_fn(|s: Str| s.to_uppercase()),
    );
    t.set(
        "to_ascii_lowercase",
        CallbackInner::from_fn(|s: Str| s.to_ascii_lowercase()),
    );
    t.set(
        "to_ascii_uppercase",
        CallbackInner::from_fn(|s: Str| s.to_ascii_uppercase()),
    );
    t.set(
        "repeat",
        CallbackInner::from_fn(|s: Str, i: usize| s.repeat(i)),
    );
    t.into()
}
