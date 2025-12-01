use gc_arena::{Collect, Gc, lock::RefLock};

use crate::{
    Context,
    compiler::value::MetaMethod,
    errors::{Error, LuciaError},
    objects::{Callback, CallbackReturn, Value},
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
            if v {
                Ok(v)
            } else {
                Err(Error::new(LuciaError::Assert(
                    msg.first().copied().unwrap_or(Value::Null),
                )))
            }
        }),
    );
    builtins.set(
        ctx,
        "len",
        Callback::from(&ctx, |ctx, value: Value<'gc>| value.meta_len(ctx)),
    );
    builtins.set(
        ctx,
        "bool",
        Callback::from(&ctx, |ctx, value: Value<'gc>| value.meta_bool(ctx)),
    );
    builtins.set(
        ctx,
        "int",
        Callback::from(&ctx, |ctx, value: Value<'gc>| value.meta_int(ctx)),
    );
    builtins.set(
        ctx,
        "float",
        Callback::from(&ctx, |ctx, value: Value<'gc>| value.meta_float(ctx)),
    );
    builtins.set(
        ctx,
        "str",
        Callback::from(&ctx, |ctx, value: Value<'gc>| value.meta_str(ctx)),
    );
    builtins.set(
        ctx,
        "repr",
        Callback::from(&ctx, |ctx, value: Value<'gc>| value.meta_repr(ctx)),
    );
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
