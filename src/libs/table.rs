use gc_arena::{lock::RefLock, Gc};

use crate::{
    check_args,
    meta_ops::raw_iter,
    objects::{Callback, CallbackReturn, IntoValue, Table, Value},
    Context,
};

pub fn table_lib(ctx: Context<'_>) -> Table<'_> {
    let t = Table::new(&ctx);
    t.set(
        ctx,
        "keys",
        Callback::from_fn(&ctx, |ctx, args| {
            let (t,) = check_args!(args, Table);
            Ok(CallbackReturn::Return(
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
                .into(),
            ))
        }),
    );
    t.set(
        ctx,
        "values",
        Callback::from_fn(&ctx, |ctx, args| {
            let (t,) = check_args!(args, Table);
            Ok(CallbackReturn::Return(
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
                .into(),
            ))
        }),
    );
    t.set(
        ctx,
        "raw_len",
        Callback::from_fn(&ctx, |_ctx, args| {
            let (table,) = check_args!(args, Table);
            Ok(CallbackReturn::Return(Value::Int(
                table.len().try_into().unwrap(),
            )))
        }),
    );
    t.set(
        ctx,
        "raw_get",
        Callback::from_fn(&ctx, |ctx, args| {
            let (table, key) = check_args!(args, Table, Value);
            Ok(CallbackReturn::Return(table.get(ctx, key)))
        }),
    );
    t.set(
        ctx,
        "raw_set",
        Callback::from_fn(&ctx, |ctx, args| {
            let (table, key, value) = check_args!(args, Table, Value, Value);
            table.set(ctx, key, value);
            Ok(CallbackReturn::Return(Value::Null))
        }),
    );
    t.set(
        ctx,
        "raw_iter",
        Callback::from_fn(&ctx, |ctx, args| {
            let (table,) = check_args!(args, Table,);
            Ok(CallbackReturn::Return(raw_iter(ctx, table).into_value(ctx)))
        }),
    );
    t
}
