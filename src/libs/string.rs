use crate::{
    check_args,
    objects::{AnyCallback, CallbackReturn, IntoValue, Table, TableEntries, Value},
    Context,
};

pub fn string_lib(ctx: Context<'_>) -> Table<'_> {
    let t = Table::new(&ctx);
    t.set(
        ctx,
        "get",
        AnyCallback::from_fn(&ctx, |ctx, args| {
            let (s, i) = check_args!(args, Str, Int);
            Ok(CallbackReturn::Return(
                s.chars()
                    .nth(i.try_into().unwrap())
                    .map_or(Value::Null, |x| x.to_string().into_value(ctx)),
            ))
        }),
    );
    t.set(
        ctx,
        "chars",
        AnyCallback::from_fn(&ctx, |ctx, args| {
            let (s,) = check_args!(args, Str);
            Ok(CallbackReturn::Return(
                TableEntries::from_iter(s.chars().map(|x| x.to_string().into_value(ctx)))
                    .into_value(ctx),
            ))
        }),
    );
    t.set(
        ctx,
        "lines",
        AnyCallback::from_fn(&ctx, |ctx, args| {
            let (s,) = check_args!(args, Str);
            Ok(CallbackReturn::Return(
                TableEntries::from_iter(s.lines().map(|x| x.to_string().into_value(ctx)))
                    .into_value(ctx),
            ))
        }),
    );
    t.set(
        ctx,
        "contains",
        AnyCallback::from_fn(&ctx, |_ctx, args| {
            let (s1, s2) = check_args!(args, Str, Str);
            Ok(CallbackReturn::Return(
                (s1.contains(&s2.to_string())).into(),
            ))
        }),
    );
    t.set(
        ctx,
        "starts_with",
        AnyCallback::from_fn(&ctx, |_ctx, args| {
            let (s1, s2) = check_args!(args, Str, Str);
            Ok(CallbackReturn::Return(
                (s1.starts_with(&s2.to_string())).into(),
            ))
        }),
    );
    t.set(
        ctx,
        "ends_with",
        AnyCallback::from_fn(&ctx, |_ctx, args| {
            let (s1, s2) = check_args!(args, Str, Str);
            Ok(CallbackReturn::Return(
                (s1.ends_with(&s2.to_string())).into(),
            ))
        }),
    );
    t.set(
        ctx,
        "find",
        AnyCallback::from_fn(&ctx, |_ctx, args| {
            let (s1, s2) = check_args!(args, Str, Str);
            Ok(CallbackReturn::Return(
                (s1.find(&s2.to_string()))
                    .map_or(Value::Null, |x| Value::Int(x.try_into().unwrap())),
            ))
        }),
    );
    t.set(
        ctx,
        "split",
        AnyCallback::from_fn(&ctx, |ctx, args| {
            let (s, pat, count) = check_args!(args, Str, Str | Int);
            let pat = &pat.to_string();
            Ok(CallbackReturn::Return(if let Some(count) = count {
                TableEntries::from_iter(
                    s.splitn(count.try_into().unwrap(), pat)
                        .map(|x| x.to_string().into_value(ctx)),
                )
                .into_value(ctx)
            } else {
                TableEntries::from_iter(s.split(pat).map(|x| x.to_string().into_value(ctx)))
                    .into_value(ctx)
            }))
        }),
    );
    t.set(
        ctx,
        "trim",
        AnyCallback::from_fn(&ctx, |ctx, args| {
            let (s,) = check_args!(args, Str);
            Ok(CallbackReturn::Return(s.trim().to_string().into_value(ctx)))
        }),
    );
    t.set(
        ctx,
        "trim_start",
        AnyCallback::from_fn(&ctx, |ctx, args| {
            let (s,) = check_args!(args, Str);
            Ok(CallbackReturn::Return(
                s.trim_start().to_string().into_value(ctx),
            ))
        }),
    );
    t.set(
        ctx,
        "trim_end",
        AnyCallback::from_fn(&ctx, |ctx, args| {
            let (s,) = check_args!(args, Str);
            Ok(CallbackReturn::Return(
                s.trim_end().to_string().into_value(ctx),
            ))
        }),
    );
    t.set(
        ctx,
        "strip_prefix",
        AnyCallback::from_fn(&ctx, |ctx, args| {
            let (s1, s2) = check_args!(args, Str, Str);
            Ok(CallbackReturn::Return(
                s1.strip_prefix(&s2.to_string())
                    .map_or(s1.into_value(ctx), |x| x.to_string().into_value(ctx)),
            ))
        }),
    );
    t.set(
        ctx,
        "strip_suffix",
        AnyCallback::from_fn(&ctx, |ctx, args| {
            let (s1, s2) = check_args!(args, Str, Str);
            Ok(CallbackReturn::Return(
                s1.strip_suffix(&s2.to_string())
                    .map_or(s1.into_value(ctx), |x| x.to_string().into_value(ctx)),
            ))
        }),
    );
    t.set(
        ctx,
        "is_ascii",
        AnyCallback::from_fn(&ctx, |_ctx, args| {
            let (s,) = check_args!(args, Str);
            Ok(CallbackReturn::Return(s.is_ascii().into()))
        }),
    );
    t.set(
        ctx,
        "replace",
        AnyCallback::from_fn(&ctx, |ctx, args| {
            let (s, from, to, count) = check_args!(args, Str, Str, Str | Int);
            Ok(CallbackReturn::Return(if let Some(count) = count {
                s.replacen(
                    &from.to_string(),
                    &to.to_string(),
                    count.try_into().unwrap(),
                )
                .into_value(ctx)
            } else {
                s.replace(&from.to_string(), &to.to_string())
                    .into_value(ctx)
            }))
        }),
    );
    t.set(
        ctx,
        "to_lowercase",
        AnyCallback::from_fn(&ctx, |ctx, args| {
            let (s,) = check_args!(args, Str);
            Ok(CallbackReturn::Return(s.to_lowercase().into_value(ctx)))
        }),
    );
    t.set(
        ctx,
        "to_uppercase",
        AnyCallback::from_fn(&ctx, |ctx, args| {
            let (s,) = check_args!(args, Str);
            Ok(CallbackReturn::Return(s.to_uppercase().into_value(ctx)))
        }),
    );
    t.set(
        ctx,
        "to_ascii_lowercase",
        AnyCallback::from_fn(&ctx, |ctx, args| {
            let (s,) = check_args!(args, Str);
            Ok(CallbackReturn::Return(
                s.to_ascii_lowercase().into_value(ctx),
            ))
        }),
    );
    t.set(
        ctx,
        "to_ascii_uppercase",
        AnyCallback::from_fn(&ctx, |ctx, args| {
            let (s,) = check_args!(args, Str);
            Ok(CallbackReturn::Return(
                s.to_ascii_uppercase().into_value(ctx),
            ))
        }),
    );
    t.set(
        ctx,
        "repeat",
        AnyCallback::from_fn(&ctx, |ctx, args| {
            let (s, i) = check_args!(args, Str, Int);
            Ok(CallbackReturn::Return(
                s.repeat(i.try_into().unwrap()).into_value(ctx),
            ))
        }),
    );
    t
}
