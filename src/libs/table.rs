use gc_arena::{lock::RefLock, Gc};

use crate::{
    meta_ops::raw_iter,
    objects::{Callback, CallbackReturn, Table, Value},
    Context,
};

pub fn table_lib<'gc>(ctx: Context<'gc>) -> Table<'gc> {
    let t = Table::new(&ctx);
    t.set(
        ctx,
        "keys",
        Callback::from_fn(&ctx, |ctx: Context<'gc>, t: Table<'gc>| {
            Callback::from_fn_with(
                &ctx,
                (t, Gc::new(&ctx, RefLock::new(0usize))),
                |(t, i), ctx, _args| {
                    let mut i = i.borrow_mut(&ctx);
                    *i += 1;
                    Ok(CallbackReturn::Return(
                        t.get_index(*i - 1).map_or(Value::Null, |(k, _)| k),
                    ))
                },
            )
        }),
    );
    t.set(
        ctx,
        "values",
        Callback::from_fn(&ctx, |ctx: Context<'gc>, t: Table<'gc>| {
            Callback::from_fn_with(
                &ctx,
                (t, Gc::new(&ctx, RefLock::new(0usize))),
                |(t, i), ctx, _args| {
                    let mut i = i.borrow_mut(&ctx);
                    *i += 1;
                    Ok(CallbackReturn::Return(
                        t.get_index(*i - 1).map_or(Value::Null, |(_, v)| v),
                    ))
                },
            )
        }),
    );
    t.set(
        ctx,
        "raw_len",
        Callback::from_fn(&ctx, |table: Table<'gc>| {
            i64::try_from(table.len()).unwrap()
        }),
    );
    t.set(
        ctx,
        "raw_get",
        Callback::from_fn(
            &ctx,
            |ctx: Context<'gc>, table: Table<'gc>, key: Value<'gc>| table.get(ctx, key),
        ),
    );
    t.set(
        ctx,
        "raw_set",
        Callback::from_fn(
            &ctx,
            |ctx: Context<'gc>, table: Table<'gc>, key: Value<'gc>, value: Value<'gc>| {
                table.set(ctx, key, value);
            },
        ),
    );
    t.set(ctx, "raw_iter", Callback::from_fn(&ctx, raw_iter));
    t
}
