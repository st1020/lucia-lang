use std::rc::Rc;

use crate::objects::{Callback, RcTable, Table, Value};

pub fn table_lib() -> Table {
    let mut t = Table::new();
    // t.set(
    //     "keys",
    //     NativeFn::from(&ctx, |ctx: Context, table: Table| {
    //         NativeFn::from_fn_with(
    //             &ctx,
    //             Gc::new(&ctx, RefLock::new(table.iter())),
    //             |iter, ctx, _args| {
    //                 Ok(NativeFnReturn::Return(
    //                     iter.borrow_mut(&ctx).next().map_or(Value::Null, |(k, _)| k),
    //                 ))
    //             },
    //         )
    //     }),
    // );
    // t.set(
    //     "values",
    //     NativeFn::from(&ctx, |ctx: Context, table: Table| {
    //         NativeFn::from_fn_with(
    //             &ctx,
    //             Gc::new(&ctx, RefLock::new(table.iter())),
    //             |iter, ctx, _args| {
    //                 Ok(NativeFnReturn::Return(
    //                     iter.borrow_mut(&ctx).next().map_or(Value::Null, |(_, v)| v),
    //                 ))
    //             },
    //         )
    //     }),
    // );
    t.set("raw_len", Callback::from_fn(|table: RcTable| table.len()));
    t.set(
        "raw_get",
        Callback::from_fn(|table: RcTable, key: Value| table.get(key)),
    );
    t.set(
        "raw_set",
        Callback::from_fn(|table: RcTable, key: Value, value: Value| {
            let mut table = Rc::unwrap_or_clone(table);
            table.set(key, value);
            table
        }),
    );
    // t.set("raw_iter", t.iter_callback(ctx));
    t
}
