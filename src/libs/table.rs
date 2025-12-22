use std::rc::Rc;

use crate::objects::{CallbackInner, Table, TableInner, Value};

pub fn table_lib() -> Table {
    let mut t = TableInner::new();
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
    t.set(
        "raw_len",
        CallbackInner::from_fn(|table: Table| table.len()),
    );
    t.set(
        "raw_get",
        CallbackInner::from_fn(|table: Table, key: Value| table.get(key)),
    );
    t.set(
        "raw_set",
        CallbackInner::from_fn(|table: Table, key: Value, value: Value| {
            let mut table = Rc::unwrap_or_clone(table);
            table.set(key, value);
            table
        }),
    );
    // t.set("raw_iter", t.iter_callback(ctx));
    t.into()
}
