use crate::{
    Context,
    compiler::value::MetaMethod,
    errors::Error,
    objects::{BuiltinEffect, Callback, CallbackFn, CallbackResult, CallbackReturn, Effect, Value},
};

pub fn load_builtin(context: &mut Context) {
    let builtins = &mut context.builtins;
    builtins.set(BuiltinEffect::Yield.name(), BuiltinEffect::Yield);
    builtins.set(BuiltinEffect::Error.name(), BuiltinEffect::Error);
    builtins.set(BuiltinEffect::Panic.name(), BuiltinEffect::Panic);
    builtins.set(BuiltinEffect::Assert.name(), BuiltinEffect::Assert);
    builtins.set("id", Callback::from_fn(|v: Value| v.id().map(usize::from)));
    builtins.set("type", Callback::from_fn(|v: Value| v.value_type().name()));
    builtins.set(
        "assert",
        Callback::from_fn(|v: bool, msg: &[Value]| {
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
        Callback::from_fn(|ctx: &Context, value: Value| value.meta_len(ctx)),
    );
    builtins.set(
        "bool",
        Callback::from_fn(|ctx: &Context, value: Value| value.meta_bool(ctx)),
    );
    builtins.set(
        "int",
        Callback::from_fn(|ctx: &Context, value: Value| value.meta_int(ctx)),
    );
    builtins.set(
        "float",
        Callback::from_fn(|ctx: &Context, value: Value| value.meta_float(ctx)),
    );
    builtins.set(
        "str",
        Callback::from_fn(|ctx: &Context, value: Value| value.meta_str(ctx)),
    );
    builtins.set(
        "repr",
        Callback::from_fn(|ctx: &Context, value: Value| value.meta_repr(ctx)),
    );
    builtins.set(
        "range",
        Callback::from_fn(|start: i64, end: i64| {
            #[derive(Clone)]
            struct RangeIter {
                value: i64,
                end: i64,
            }

            impl CallbackFn for RangeIter {
                fn call(&mut self, _ctx: &Context, _args: &[Value]) -> CallbackResult {
                    self.value += 1;
                    if self.value <= self.end {
                        Ok(CallbackReturn::Perform {
                            effect: Effect::Builtin(BuiltinEffect::Yield).into(),
                            args: vec![Value::Int(self.value - 1)],
                        })
                    } else {
                        Ok(CallbackReturn::ReturnValue { value: Value::Null })
                    }
                }
            }

            Callback::new(RangeIter { value: start, end })
        }),
    );
}
