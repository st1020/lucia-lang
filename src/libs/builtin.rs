use gc_arena::{lock::RefLock, Collect, Gc};

use crate::{
    errors::{Error, LuciaError},
    meta_ops,
    objects::{Callback, CallbackReturn, Value},
    Context,
};

pub fn load_builtin<'gc>(ctx: Context<'gc>) {
    let builtins = ctx.state.builtins;
    builtins.set(
        ctx,
        "id",
        Callback::from(&ctx, |v: Value<'gc>| {
            v.id().map(|x| i64::try_from(usize::from(x)).unwrap())
        }),
    );
    builtins.set(
        ctx,
        "type",
        Callback::from(&ctx, |v: Value<'gc>| v.value_type().name()),
    );
    builtins.set(
        ctx,
        "assert",
        Callback::from(&ctx, |v: bool, msg: &[Value<'gc>]| {
            if !v {
                Err(Error::new(LuciaError::Assert(
                    msg.first().cloned().unwrap_or(Value::Null),
                )))
            } else {
                Ok(v)
            }
        }),
    );
    builtins.set(ctx, "len", Callback::from(&ctx, meta_ops::len));
    builtins.set(ctx, "bool", Callback::from(&ctx, meta_ops::bool));
    builtins.set(ctx, "int", Callback::from(&ctx, meta_ops::int));
    builtins.set(ctx, "float", Callback::from(&ctx, meta_ops::float));
    builtins.set(ctx, "str", Callback::from(&ctx, meta_ops::str));
    builtins.set(ctx, "repr", Callback::from(&ctx, meta_ops::repr));
    builtins.set(
        ctx,
        "range",
        Callback::from(&ctx, |ctx: Context<'gc>, start: i64, end: i64| {
            #[derive(Collect)]
            #[collect[no_drop]]
            struct RangeIter {
                value: i64,
                end: i64,
            }

            Ok(CallbackReturn::Return(
                Callback::from_fn_with(
                    &ctx,
                    Gc::new(&ctx, RefLock::new(RangeIter { value: start, end })),
                    |range, ctx, _args| {
                        let mut range = range.borrow_mut(&ctx);
                        let value = range.value;
                        Ok(CallbackReturn::Return(if value == range.end {
                            Value::Null
                        } else {
                            range.value += 1;
                            Value::Int(value)
                        }))
                    },
                )
                .into(),
            ))
        }),
    );
}
