use gc_arena::{Gc, lock::RefLock};

use crate::{
    Context,
    objects::{Callback, CallbackReturn, Table, Value},
};

pub fn table_lib<'gc>(ctx: Context<'gc>) -> Table<'gc> {
    let t = Table::new(&ctx);
    t.set(
        ctx,
        "keys",
        Callback::from(&ctx, |ctx: Context<'gc>, table: Table<'gc>| {
            Callback::from_fn_with(
                &ctx,
                Gc::new(&ctx, RefLock::new(table.iter())),
                |iter, ctx, _args| {
                    Ok(CallbackReturn::Return(
                        iter.borrow_mut(&ctx).next().map_or(Value::Null, |(k, _)| k),
                    ))
                },
            )
        }),
    );
    t.set(
        ctx,
        "values",
        Callback::from(&ctx, |ctx: Context<'gc>, table: Table<'gc>| {
            Callback::from_fn_with(
                &ctx,
                Gc::new(&ctx, RefLock::new(table.iter())),
                |iter, ctx, _args| {
                    Ok(CallbackReturn::Return(
                        iter.borrow_mut(&ctx).next().map_or(Value::Null, |(_, v)| v),
                    ))
                },
            )
        }),
    );
    t.set(
        ctx,
        "raw_len",
        Callback::from(&ctx, |table: Table<'gc>| {
            i64::try_from(table.len()).unwrap()
        }),
    );
    t.set(
        ctx,
        "raw_get",
        Callback::from(
            &ctx,
            |ctx: Context<'gc>, table: Table<'gc>, key: Value<'gc>| table.get(ctx, key),
        ),
    );
    t.set(
        ctx,
        "raw_set",
        Callback::from(
            &ctx,
            |ctx: Context<'gc>, table: Table<'gc>, key: Value<'gc>, value: Value<'gc>| {
                table.set(ctx, key, value);
            },
        ),
    );
    t.set(ctx, "raw_iter", t.iter_callback(ctx));
    t
}
