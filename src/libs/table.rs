use gc_arena::Collect;

use crate::{
    check_args, meta_ops,
    objects::{AnyCallback, Callback, CallbackReturn, IntoValue, Table, Value},
    Context,
};

#[derive(Collect)]
#[collect[no_drop]]
struct TableKeys<'gc>(Table<'gc>, usize);

impl<'gc> Callback<'gc> for TableKeys<'gc> {
    fn call(
        &mut self,
        _ctx: Context<'gc>,
        _args: Vec<Value<'gc>>,
    ) -> Result<CallbackReturn<'gc>, crate::errors::Error<'gc>> {
        let t = Ok(CallbackReturn::Return(
            self.0.get_index(self.1).map_or(Value::Null, |(k, _)| k),
        ));
        self.1 += 1;
        t
    }
}

#[derive(Collect)]
#[collect[no_drop]]
struct TableValues<'gc>(Table<'gc>, usize);

impl<'gc> Callback<'gc> for TableValues<'gc> {
    fn call(
        &mut self,
        _ctx: Context<'gc>,
        _args: Vec<Value<'gc>>,
    ) -> Result<CallbackReturn<'gc>, crate::errors::Error<'gc>> {
        let t = Ok(CallbackReturn::Return(
            self.0.get_index(self.1).map_or(Value::Null, |(_, v)| v),
        ));
        self.1 += 1;
        t
    }
}

pub fn table_lib(ctx: Context<'_>) -> Table<'_> {
    let t = Table::new(&ctx);
    t.set(
        ctx,
        "keys",
        AnyCallback::from_fn(&ctx, |ctx, args| {
            let (t,) = check_args!(args, Table);
            Ok(CallbackReturn::Return(
                AnyCallback::new(&ctx, TableKeys(t, 0)).into(),
            ))
        }),
    );
    t.set(
        ctx,
        "values",
        AnyCallback::from_fn(&ctx, |ctx, args| {
            let (t,) = check_args!(args, Table);
            Ok(CallbackReturn::Return(
                AnyCallback::new(&ctx, TableValues(t, 0)).into(),
            ))
        }),
    );
    t.set(
        ctx,
        "raw_len",
        AnyCallback::from_fn(&ctx, |_ctx, args| {
            let (table,) = check_args!(args, Table);
            Ok(CallbackReturn::Return(Value::Int(
                table.len().try_into().unwrap(),
            )))
        }),
    );
    t.set(
        ctx,
        "raw_get",
        AnyCallback::from_fn(&ctx, |ctx, args| {
            let (table, key) = check_args!(args, Table, Value);
            Ok(CallbackReturn::Return(table.get(ctx, key)))
        }),
    );
    t.set(
        ctx,
        "raw_set",
        AnyCallback::from_fn(&ctx, |ctx, args| {
            let (table, key, value) = check_args!(args, Table, Value, Value);
            table.set(ctx, key, value);
            Ok(CallbackReturn::Return(Value::Null))
        }),
    );
    t.set(
        ctx,
        "raw_iter",
        AnyCallback::from_fn(&ctx, |ctx, args| {
            let (table,) = check_args!(args, Table,);
            Ok(CallbackReturn::Return(
                AnyCallback::new(&ctx, meta_ops::IterTable(table, 0)).into_value(ctx),
            ))
        }),
    );
    t
}
