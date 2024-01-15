use crate::{
    check_args,
    errors::{Error, ErrorKind},
    meta_ops,
    objects::{Callback, CallbackReturn, IntoValue, Value},
    Context,
};

pub fn load_builtin(ctx: Context<'_>) {
    let builtins = ctx.state.builtins;
    builtins.set(
        ctx,
        "id",
        Callback::from_fn(&ctx, |_ctx, args| {
            let (v,) = check_args!(args, Value);
            Ok(CallbackReturn::Return(v.id().map_or(Value::Null, |x| {
                Value::Int(usize::from(x).try_into().unwrap())
            })))
        }),
    );
    builtins.set(
        ctx,
        "type",
        Callback::from_fn(&ctx, |ctx, args| {
            let (v,) = check_args!(args, Value);
            Ok(CallbackReturn::Return(
                v.value_type().name().into_value(ctx),
            ))
        }),
    );
    builtins.set(
        ctx,
        "assert",
        Callback::from_fn(&ctx, |_ctx, args| {
            let (v, msg) = check_args!(args, Value | Value);
            if !(bool::from(v)) {
                Err(Error::new(ErrorKind::AssertError(
                    msg.unwrap_or(Value::Null),
                )))
            } else {
                Ok(CallbackReturn::Return(v))
            }
        }),
    );
    builtins.set(
        ctx,
        "len",
        Callback::from_fn(&ctx, |ctx, args| {
            let (v,) = check_args!(args, Value);
            Ok(meta_ops::len(ctx, v)?.into())
        }),
    );
    builtins.set(
        ctx,
        "bool",
        Callback::from_fn(&ctx, |ctx, args| {
            let (v,) = check_args!(args, Value);
            Ok(meta_ops::bool(ctx, v)?.into())
        }),
    );
    builtins.set(
        ctx,
        "int",
        Callback::from_fn(&ctx, |ctx, args| {
            let (v,) = check_args!(args, Value);
            Ok(meta_ops::int(ctx, v)?.into())
        }),
    );
    builtins.set(
        ctx,
        "float",
        Callback::from_fn(&ctx, |ctx, args| {
            let (v,) = check_args!(args, Value);
            Ok(meta_ops::float(ctx, v)?.into())
        }),
    );
    builtins.set(
        ctx,
        "str",
        Callback::from_fn(&ctx, |ctx, args| {
            let (v,) = check_args!(args, Value);
            Ok(meta_ops::str(ctx, v)?.into())
        }),
    );
    builtins.set(
        ctx,
        "repr",
        Callback::from_fn(&ctx, |ctx, args| {
            let (v,) = check_args!(args, Value);
            Ok(meta_ops::repr(ctx, v)?.into())
        }),
    );
}
