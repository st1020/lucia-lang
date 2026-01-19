use compact_str::ToCompactString;

use crate::objects::{Callback, RcStr, Table, TableEntries, Value};

pub fn string_lib() -> Table {
    let mut t = Table::new();
    t.set(
        "get",
        Callback::from_fn(|s: RcStr, i: usize| s.chars().nth(i).map(|x| x.to_compact_string())),
    );
    t.set(
        "chars",
        Callback::from_fn(|s: RcStr| {
            s.chars()
                .map(|x| Value::from(x.to_compact_string()))
                .collect::<TableEntries>()
        }),
    );
    t.set(
        "lines",
        Callback::from_fn(|s: RcStr| s.lines().map(Value::from).collect::<TableEntries>()),
    );
    t.set(
        "contains",
        Callback::from_fn(|s1: RcStr, s2: RcStr| s1.contains(s2.as_str())),
    );
    t.set(
        "starts_with",
        Callback::from_fn(|s1: RcStr, s2: RcStr| s1.starts_with(s2.as_str())),
    );
    t.set(
        "ends_with",
        Callback::from_fn(|s1: RcStr, s2: RcStr| s1.ends_with(s2.as_str())),
    );
    t.set(
        "find",
        Callback::from_fn(|s1: RcStr, s2: RcStr| (s1.find(s2.as_str())).map(Value::from)),
    );
    t.set(
        "split",
        Callback::from_fn(|s: RcStr, pat: RcStr| {
            s.split(pat.as_str())
                .map(Value::from)
                .collect::<TableEntries>()
        }),
    );
    t.set(
        "splitn",
        Callback::from_fn(|s: RcStr, pat: RcStr, count: usize| {
            s.splitn(count, pat.as_str())
                .map(Value::from)
                .collect::<TableEntries>()
        }),
    );
    t.set(
        "trim",
        Callback::from_fn(|s: RcStr| s.trim().to_compact_string()),
    );
    t.set(
        "trim_start",
        Callback::from_fn(|s: RcStr| s.trim_start().to_compact_string()),
    );
    t.set(
        "trim_end",
        Callback::from_fn(|s: RcStr| s.trim_end().to_compact_string()),
    );
    t.set(
        "strip_prefix",
        Callback::from_fn(|s1: RcStr, s2: RcStr| {
            s1.strip_prefix(s2.as_str()).map(|x| x.to_compact_string())
        }),
    );
    t.set(
        "strip_suffix",
        Callback::from_fn(|s1: RcStr, s2: RcStr| {
            s1.strip_suffix(s2.as_str()).map(|x| x.to_compact_string())
        }),
    );
    t.set("is_ascii", Callback::from_fn(|s: RcStr| s.is_ascii()));
    t.set(
        "replace",
        Callback::from_fn(|s: RcStr, from: RcStr, to: RcStr| s.replace(from.as_str(), to.as_str())),
    );
    t.set(
        "replacen",
        Callback::from_fn(|s: RcStr, from: RcStr, to: RcStr, count: usize| {
            s.replacen(from.as_str(), to.as_str(), count)
        }),
    );
    t.set(
        "to_lowercase",
        Callback::from_fn(|s: RcStr| s.to_lowercase()),
    );
    t.set(
        "to_uppercase",
        Callback::from_fn(|s: RcStr| s.to_uppercase()),
    );
    t.set(
        "to_ascii_lowercase",
        Callback::from_fn(|s: RcStr| s.to_ascii_lowercase()),
    );
    t.set(
        "to_ascii_uppercase",
        Callback::from_fn(|s: RcStr| s.to_ascii_uppercase()),
    );
    t.set(
        "repeat",
        Callback::from_fn(|s: RcStr, i: usize| s.repeat(i)),
    );
    t
}
