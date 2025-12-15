use crate::{
    Context,
    compiler::value::MetaMethod,
    errors::Error,
    objects::{CallbackInner, Value},
};

pub fn load_builtin(context: &mut Context) {
    let builtins = &mut context.builtins;
    builtins.set(
        "id",
        CallbackInner::from(|v: Value| v.id().map(usize::from)),
    );
    builtins.set(
        "type",
        CallbackInner::from(|v: Value| v.value_type().name()),
    );
    builtins.set(
        "assert",
        CallbackInner::from(|v: bool, msg: &[Value]| {
            if v {
                Ok(v)
            } else {
                Err(Error::LuciaAssert(
                    msg.first().cloned().unwrap_or(Value::Null),
                ))
            }
        }),
    );
    builtins.set(
        "len",
        CallbackInner::from(|ctx: &Context, value: Value| value.meta_len(ctx)),
    );
    builtins.set(
        "bool",
        CallbackInner::from(|ctx: &Context, value: Value| value.meta_bool(ctx)),
    );
    builtins.set(
        "int",
        CallbackInner::from(|ctx: &Context, value: Value| value.meta_int(ctx)),
    );
    builtins.set(
        "float",
        CallbackInner::from(|ctx: &Context, value: Value| value.meta_float(ctx)),
    );
    builtins.set(
        "str",
        CallbackInner::from(|ctx: &Context, value: Value| value.meta_str(ctx)),
    );
    builtins.set(
        "repr",
        CallbackInner::from(|ctx: &Context, value: Value| value.meta_repr(ctx)),
    );
    // builtins.set(
    //     "range",
    //     Callback::from(&|ctx: Context, start: i64, end: i64| {
    //         #[derive(Collect)]
    //         #[collect[no_drop]]
    //         struct RangeIter {
    //             value: i64,
    //             end: i64,
    //         }

    //         Ok(CallbackReturn::Return(
    //             Callback::from_fn_with(
    //                 Gc::new(&RefLock::new(RangeIter { value: start, end })),
    //                 |range, _args| {
    //                     let mut range = range.borrow_mut(&ctx);
    //                     let value = range.value;
    //                     Ok(CallbackReturn::Return(if value == range.end {
    //                         Value::Null
    //                     } else {
    //                         range.value += 1;
    //                         Value::Int(value)
    //                     }))
    //                 },
    //             )
    //             .into(),
    //         ))
    //     }),
    // );
}
