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
            if !v {
                Err(Error::new(LuciaError::Assert(
                    msg.first().cloned().unwrap_or(Value::Null),
                )))
            } else {
                Ok(v)
            }
        }),
    );
    builtins.set(
        ctx,
        "len",
        Callback::from(&ctx, |ctx: Context<'gc>, value| ctx.len(value)),
    );
    builtins.set(
        ctx,
        "bool",
        Callback::from(&ctx, |ctx: Context<'gc>, value| ctx.bool(value)),
    );
    builtins.set(
        ctx,
        "int",
        Callback::from(&ctx, |ctx: Context<'gc>, value| ctx.int(value)),
    );
    builtins.set(
        ctx,
        "float",
        Callback::from(&ctx, |ctx: Context<'gc>, value| ctx.float(value)),
    );
    builtins.set(
        ctx,
        "str",
        Callback::from(&ctx, |ctx: Context<'gc>, value| ctx.str(value)),
    );
    builtins.set(
        ctx,
        "repr",
        Callback::from(&ctx, |ctx: Context<'gc>, value| ctx.repr(value)),
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
