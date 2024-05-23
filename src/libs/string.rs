use smol_str::{StrExt, ToSmolStr};

use crate::{
    check_args,
    objects::{Callback, CallbackReturn, IntoValue, Table, TableEntries, Value},
    Context,
};

pub fn string_lib(ctx: Context<'_>) -> Table<'_> {
    let t = Table::new(&ctx);
    t.set(
        ctx,
        "get",
        Callback::from_fn(&ctx, |ctx, args| {
            let (s, i) = check_args!(args, Str, Int);
            Ok(CallbackReturn::Return(
                s.chars()
                    .nth(i.try_into().unwrap())
                    .map_or(Value::Null, |x| x.to_smolstr().into_value(ctx)),
            ))
        }),
    );
    t.set(
        ctx,
        "chars",
        Callback::from_fn(&ctx, |ctx, args| {
            let (s,) = check_args!(args, Str);
            Ok(CallbackReturn::Return(
                TableEntries::from_iter(s.chars().map(|x| x.to_smolstr().into_value(ctx)))
                    .into_value(ctx),
            ))
        }),
    );
    t.set(
        ctx,
        "lines",
        Callback::from_fn(&ctx, |ctx, args| {
            let (s,) = check_args!(args, Str);
            Ok(CallbackReturn::Return(
                TableEntries::from_iter(s.lines().map(|x| x.to_smolstr().into_value(ctx)))
                    .into_value(ctx),
            ))
        }),
    );
    t.set(
        ctx,
        "contains",
        Callback::from_fn(&ctx, |_ctx, args| {
            let (s1, s2) = check_args!(args, Str, Str);
            Ok(CallbackReturn::Return((s1.contains(s2.as_ref())).into()))
        }),
    );
    t.set(
        ctx,
        "starts_with",
        Callback::from_fn(&ctx, |_ctx, args| {
            let (s1, s2) = check_args!(args, Str, Str);
            Ok(CallbackReturn::Return((s1.starts_with(s2.as_ref())).into()))
        }),
    );
    t.set(
        ctx,
        "ends_with",
        Callback::from_fn(&ctx, |_ctx, args| {
            let (s1, s2) = check_args!(args, Str, Str);
            Ok(CallbackReturn::Return((s1.ends_with(s2.as_ref())).into()))
        }),
    );
    t.set(
        ctx,
        "find",
        Callback::from_fn(&ctx, |_ctx, args| {
            let (s1, s2) = check_args!(args, Str, Str);
            Ok(CallbackReturn::Return(
                (s1.find(s2.as_ref())).map_or(Value::Null, |x| Value::Int(x.try_into().unwrap())),
            ))
        }),
    );
    t.set(
        ctx,
        "split",
        Callback::from_fn(&ctx, |ctx, args| {
            let (s, pat, count) = check_args!(args, Str, Str | Int);
            let pat = pat.as_ref();
            Ok(CallbackReturn::Return(if let Some(count) = count {
                TableEntries::from_iter(
                    s.splitn(count.try_into().unwrap(), pat)
                        .map(|x| x.to_smolstr().into_value(ctx)),
                )
                .into_value(ctx)
            } else {
                TableEntries::from_iter(s.split(pat).map(|x| x.to_smolstr().into_value(ctx)))
                    .into_value(ctx)
            }))
        }),
    );
    t.set(
        ctx,
        "trim",
        Callback::from_fn(&ctx, |ctx, args| {
            let (s,) = check_args!(args, Str);
            Ok(CallbackReturn::Return(
                s.trim().to_smolstr().into_value(ctx),
            ))
        }),
    );
    t.set(
        ctx,
        "trim_start",
        Callback::from_fn(&ctx, |ctx, args| {
            let (s,) = check_args!(args, Str);
            Ok(CallbackReturn::Return(
                s.trim_start().to_smolstr().into_value(ctx),
            ))
        }),
    );
    t.set(
        ctx,
        "trim_end",
        Callback::from_fn(&ctx, |ctx, args| {
            let (s,) = check_args!(args, Str);
            Ok(CallbackReturn::Return(
                s.trim_end().to_smolstr().into_value(ctx),
            ))
        }),
    );
    t.set(
        ctx,
        "strip_prefix",
        Callback::from_fn(&ctx, |ctx, args| {
            let (s1, s2) = check_args!(args, Str, Str);
            Ok(CallbackReturn::Return(
                s1.strip_prefix(s2.as_ref())
                    .map_or(s1.into_value(ctx), |x| x.to_smolstr().into_value(ctx)),
            ))
        }),
    );
    t.set(
        ctx,
        "strip_suffix",
        Callback::from_fn(&ctx, |ctx, args| {
            let (s1, s2) = check_args!(args, Str, Str);
            Ok(CallbackReturn::Return(
                s1.strip_suffix(s2.as_ref())
                    .map_or(s1.into_value(ctx), |x| x.to_smolstr().into_value(ctx)),
            ))
        }),
    );
    t.set(
        ctx,
        "is_ascii",
        Callback::from_fn(&ctx, |_ctx, args| {
            let (s,) = check_args!(args, Str);
            Ok(CallbackReturn::Return(s.is_ascii().into()))
        }),
    );
    t.set(
        ctx,
        "replace",
        Callback::from_fn(&ctx, |ctx, args| {
            let (s, from, to, count) = check_args!(args, Str, Str, Str | Int);
            Ok(CallbackReturn::Return(if let Some(count) = count {
                s.replacen_smolstr(from.as_ref(), to.as_ref(), count.try_into().unwrap())
                    .into_value(ctx)
            } else {
                s.replace_smolstr(from.as_ref(), to.as_ref())
                    .into_value(ctx)
            }))
        }),
    );
    t.set(
        ctx,
        "to_lowercase",
        Callback::from_fn(&ctx, |ctx, args| {
            let (s,) = check_args!(args, Str);
            Ok(CallbackReturn::Return(
                s.to_lowercase_smolstr().into_value(ctx),
            ))
        }),
    );
    t.set(
        ctx,
        "to_uppercase",
        Callback::from_fn(&ctx, |ctx, args| {
            let (s,) = check_args!(args, Str);
            Ok(CallbackReturn::Return(
                s.to_uppercase_smolstr().into_value(ctx),
            ))
        }),
    );
    t.set(
        ctx,
        "to_ascii_lowercase",
        Callback::from_fn(&ctx, |ctx, args| {
            let (s,) = check_args!(args, Str);
            Ok(CallbackReturn::Return(
                s.to_ascii_lowercase_smolstr().into_value(ctx),
            ))
        }),
    );
    t.set(
        ctx,
        "to_ascii_uppercase",
        Callback::from_fn(&ctx, |ctx, args| {
            let (s,) = check_args!(args, Str);
            Ok(CallbackReturn::Return(
                s.to_ascii_uppercase_smolstr().into_value(ctx),
            ))
        }),
    );
    t.set(
        ctx,
        "repeat",
        Callback::from_fn(&ctx, |ctx, args| {
            let (s, i) = check_args!(args, Str, Int);
            Ok(CallbackReturn::Return(
                s.repeat(i.try_into().unwrap()).to_smolstr().into_value(ctx),
            ))
        }),
    );
    t
}
