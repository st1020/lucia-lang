use gc_arena::Collect;

use crate::{
    check_args, meta_ops,
    objects::{Callback, CallbackFn, CallbackReturn, IntoValue, Table, Value},
    Context,
};

#[derive(Collect)]
#[collect[no_drop]]
struct TableKeys<'gc> {
    table: Table<'gc>,
    index: usize,
}

impl<'gc> CallbackFn<'gc> for TableKeys<'gc> {
    fn call(
        &mut self,
        _ctx: Context<'gc>,
        _args: Vec<Value<'gc>>,
    ) -> Result<CallbackReturn<'gc>, crate::errors::Error<'gc>> {
        let t = Ok(CallbackReturn::Return(
            self.table
                .get_index(self.index)
                .map_or(Value::Null, |(k, _)| k),
        ));
        self.index += 1;
        t
    }
}

#[derive(Collect)]
#[collect[no_drop]]
struct TableValues<'gc> {
    table: Table<'gc>,
    index: usize,
}

impl<'gc> CallbackFn<'gc> for TableValues<'gc> {
    fn call(
        &mut self,
        _ctx: Context<'gc>,
        _args: Vec<Value<'gc>>,
    ) -> Result<CallbackReturn<'gc>, crate::errors::Error<'gc>> {
        let t = Ok(CallbackReturn::Return(
            self.table
                .get_index(self.index)
                .map_or(Value::Null, |(_, v)| v),
        ));
        self.index += 1;
        t
    }
}

pub fn table_lib(ctx: Context<'_>) -> Table<'_> {
    let t = Table::new(&ctx);
    t.set(
        ctx,
        "keys",
        Callback::from_fn(&ctx, |ctx, args| {
            let (t,) = check_args!(args, Table);
            Ok(CallbackReturn::Return(
                Callback::new(&ctx, TableKeys { table: t, index: 0 }).into(),
            ))
        }),
    );
    t.set(
        ctx,
        "values",
        Callback::from_fn(&ctx, |ctx, args| {
            let (t,) = check_args!(args, Table);
            Ok(CallbackReturn::Return(
                Callback::new(&ctx, TableValues { table: t, index: 0 }).into(),
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
            Ok(CallbackReturn::Return(
                Callback::new(&ctx, meta_ops::IterTable(table, 0)).into_value(ctx),
            ))
        }),
    );
    t
}
