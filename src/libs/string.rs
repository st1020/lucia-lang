use compact_str::ToCompactString;

use crate::{
    Context,
    objects::{Callback, IntoValue, Str, Table, TableEntries, Value},
};

pub fn string_lib<'gc>(ctx: Context<'gc>) -> Table<'gc> {
    let t = Table::new(&ctx);
    t.set(
        ctx,
        "get",
        Callback::from(&ctx, |s: Str<'gc>, i: usize| {
            s.chars().nth(i).map(|x| x.to_compact_string())
        }),
    );
    t.set(
        ctx,
        "chars",
        Callback::from(&ctx, |ctx: Context<'gc>, s: Str<'gc>| {
            TableEntries::from_iter(s.chars().map(|x| x.to_compact_string().into_value(ctx)))
        }),
    );
    t.set(
        ctx,
        "lines",
        Callback::from(&ctx, |ctx: Context<'gc>, s: Str<'gc>| {
            TableEntries::from_iter(s.lines().map(|x| x.to_compact_string().into_value(ctx)))
        }),
    );
    t.set(
        ctx,
        "contains",
        Callback::from(&ctx, |s1: Str<'gc>, s2: Str<'gc>| s1.contains(s2.as_str())),
    );
    t.set(
        ctx,
        "starts_with",
        Callback::from(&ctx, |s1: Str<'gc>, s2: Str<'gc>| {
            s1.starts_with(s2.as_str())
        }),
    );
    t.set(
        ctx,
        "ends_with",
        Callback::from(&ctx, |s1: Str<'gc>, s2: Str<'gc>| s1.ends_with(s2.as_str())),
    );
    t.set(
        ctx,
        "find",
        Callback::from(&ctx, |s1: Str<'gc>, s2: Str<'gc>| {
            (s1.find(s2.as_str())).map(|x| Value::Int(x.try_into().unwrap()))
        }),
    );
    t.set(
        ctx,
        "split",
        Callback::from(&ctx, |ctx: Context<'gc>, s: Str<'gc>, pat: Str<'gc>| {
            TableEntries::from_iter(
                s.split(pat.as_str())
                    .map(|x| x.to_compact_string().into_value(ctx)),
            )
        }),
    );
    t.set(
        ctx,
        "splitn",
        Callback::from(
            &ctx,
            |ctx: Context<'gc>, s: Str<'gc>, pat: Str<'gc>, count: usize| {
                TableEntries::from_iter(
                    s.splitn(count, pat.as_str())
                        .map(|x| x.to_compact_string().into_value(ctx)),
                )
            },
        ),
    );
    t.set(
        ctx,
        "trim",
        Callback::from(&ctx, |s: Str<'gc>| s.trim().to_compact_string()),
    );
    t.set(
        ctx,
        "trim_start",
        Callback::from(&ctx, |s: Str<'gc>| s.trim_start().to_compact_string()),
    );
    t.set(
        ctx,
        "trim_end",
        Callback::from(&ctx, |s: Str<'gc>| s.trim_end().to_compact_string()),
    );
    t.set(
        ctx,
        "strip_prefix",
        Callback::from(&ctx, |s1: Str<'gc>, s2: Str<'gc>| {
            s1.strip_prefix(s2.as_str()).map(|x| x.to_compact_string())
        }),
    );
    t.set(
        ctx,
        "strip_suffix",
        Callback::from(&ctx, |s1: Str<'gc>, s2: Str<'gc>| {
            s1.strip_suffix(s2.as_str()).map(|x| x.to_compact_string())
        }),
    );
    t.set(
        ctx,
        "is_ascii",
        Callback::from(&ctx, |s: Str<'gc>| s.is_ascii()),
    );
    t.set(
        ctx,
        "replace",
        Callback::from(&ctx, |s: Str<'gc>, from: Str<'gc>, to: Str<'gc>| {
            s.replace(from.as_str(), to.as_str()).to_compact_string()
        }),
    );
    t.set(
        ctx,
        "replace",
        Callback::from(
            &ctx,
            |s: Str<'gc>, from: Str<'gc>, to: Str<'gc>, count: usize| {
                s.replacen(from.as_str(), to.as_str(), count)
                    .to_compact_string()
            },
        ),
    );
    t.set(
        ctx,
        "to_lowercase",
        Callback::from(&ctx, |s: Str<'gc>| s.to_lowercase().to_compact_string()),
    );
    t.set(
        ctx,
        "to_uppercase",
        Callback::from(&ctx, |s: Str<'gc>| s.to_uppercase().to_compact_string()),
    );
    t.set(
        ctx,
        "to_ascii_lowercase",
        Callback::from(&ctx, |s: Str<'gc>| {
            s.to_ascii_lowercase().to_compact_string()
        }),
    );
    t.set(
        ctx,
        "to_ascii_uppercase",
        Callback::from(&ctx, |s: Str<'gc>| {
            s.to_ascii_uppercase().to_compact_string()
        }),
    );
    t.set(
        ctx,
        "repeat",
        Callback::from(&ctx, |s: Str<'gc>, i: usize| {
            s.repeat(i).to_compact_string()
        }),
    );
    t
}
