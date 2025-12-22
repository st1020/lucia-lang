use crate::{
    Context,
    compiler::value::MetaMethod,
    errors::Error,
    objects::{BuiltinEffect, CallbackInner, Value},
};

pub fn load_builtin(context: &mut Context) {
    let builtins = &mut context.builtins;
    builtins.set(BuiltinEffect::Yield.name(), BuiltinEffect::Yield);
    builtins.set(BuiltinEffect::Error.name(), BuiltinEffect::Error);
    builtins.set(BuiltinEffect::Panic.name(), BuiltinEffect::Panic);
    builtins.set(BuiltinEffect::Assert.name(), BuiltinEffect::Assert);
    builtins.set(
        "id",
        CallbackInner::from_fn(|v: Value| v.id().map(usize::from)),
    );
    builtins.set(
        "type",
        CallbackInner::from_fn(|v: Value| v.value_type().name()),
    );
    builtins.set(
        "assert",
        CallbackInner::from_fn(|v: bool, msg: &[Value]| {
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
        CallbackInner::from_fn(|ctx: &Context, value: Value| value.meta_len(ctx)),
    );
    builtins.set(
        "bool",
        CallbackInner::from_fn(|ctx: &Context, value: Value| value.meta_bool(ctx)),
    );
    builtins.set(
        "int",
        CallbackInner::from_fn(|ctx: &Context, value: Value| value.meta_int(ctx)),
    );
    builtins.set(
        "float",
        CallbackInner::from_fn(|ctx: &Context, value: Value| value.meta_float(ctx)),
    );
    builtins.set(
        "str",
        CallbackInner::from_fn(|ctx: &Context, value: Value| value.meta_str(ctx)),
    );
    builtins.set(
        "repr",
        CallbackInner::from_fn(|ctx: &Context, value: Value| value.meta_repr(ctx)),
    );
    // builtins.set(
    //     "range",
    //     CallbackInner::from_fn(|ctx: Context, start: i64, end: i64| {
    //         #[derive(Collect)]
    //         #[collect[no_drop]]
    //         struct RangeIter {
    //             value: i64,
    //             end: i64,
    //         }

    //         Ok(NativeFnReturn::Return(
    //             CallbackInner::from_fn_with(
    //                 Gc::new(&RefLock::new(RangeIter { value: start, end })),
    //                 |range, _args| {
    //                     let mut range = range.borrow_mut(&ctx);
    //                     let value = range.value;
    //                     Ok(NativeFnReturn::Return(if value == range.end {
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
