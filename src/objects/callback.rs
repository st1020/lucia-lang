use std::ops;

use gc_arena::{Collect, Gc, Mutation};

use crate::{
    errors::{Error, RuntimeError},
    meta_ops::MetaResult,
    objects::{
        conversion::{FromValue, IntoValue},
        define_object, ArgumentRange, Function, Value,
    },
    Context,
};

pub type CallbackResult<'gc> = Result<CallbackReturn<'gc>, Error<'gc>>;

#[derive(Debug, PartialEq, Eq, Collect)]
#[collect(no_drop)]
pub enum CallbackReturn<'gc> {
    Return(Value<'gc>),
    TailCall(Function<'gc>, Vec<Value<'gc>>),
}

impl<'gc, const N: usize> From<MetaResult<'gc, N>> for CallbackReturn<'gc> {
    fn from(value: MetaResult<'gc, N>) -> Self {
        match value {
            MetaResult::Value(v) => CallbackReturn::Return(v),
            MetaResult::Call(f, args) => CallbackReturn::TailCall(f, Vec::from(args)),
        }
    }
}

pub trait IntoCallbackReturn<'gc> {
    fn into_callback_return(self, ctx: Context<'gc>) -> CallbackReturn<'gc>;
}

impl<'gc> IntoCallbackReturn<'gc> for CallbackReturn<'gc> {
    fn into_callback_return(self, _ctx: Context<'gc>) -> CallbackReturn<'gc> {
        self
    }
}

impl<'gc, const N: usize> IntoCallbackReturn<'gc> for MetaResult<'gc, N> {
    fn into_callback_return(self, _ctx: Context<'gc>) -> CallbackReturn<'gc> {
        match self {
            MetaResult::Value(v) => CallbackReturn::Return(v),
            MetaResult::Call(f, args) => CallbackReturn::TailCall(f, Vec::from(args)),
        }
    }
}

impl<'gc, T: IntoValue<'gc>> IntoCallbackReturn<'gc> for T {
    fn into_callback_return(self, ctx: Context<'gc>) -> CallbackReturn<'gc> {
        CallbackReturn::Return(self.into_value(ctx))
    }
}

pub trait IntoCallbackResult<'gc> {
    fn into_callback_result(self, ctx: Context<'gc>) -> CallbackResult<'gc>;
}

impl<'gc, T: IntoCallbackReturn<'gc>> IntoCallbackResult<'gc> for T {
    fn into_callback_result(self, ctx: Context<'gc>) -> CallbackResult<'gc> {
        Ok(self.into_callback_return(ctx))
    }
}

impl<'gc, T: IntoCallbackReturn<'gc>> IntoCallbackResult<'gc> for Result<T, Error<'gc>> {
    fn into_callback_result(self, ctx: Context<'gc>) -> CallbackResult<'gc> {
        self.map(|x| x.into_callback_return(ctx))
    }
}

pub trait CallbackFn<'gc>: Collect {
    fn call(&self, ctx: Context<'gc>, args: Vec<Value<'gc>>) -> CallbackResult<'gc>;
}

define_object!(Callback, CallbackInner<'gc>, ptr_eq);

#[derive(Debug)]
pub struct CallbackInner<'gc> {
    call:
        unsafe fn(*const CallbackInner<'gc>, Context<'gc>, Vec<Value<'gc>>) -> CallbackResult<'gc>,
}

impl<'gc> Callback<'gc> {
    pub fn new<C: CallbackFn<'gc> + 'gc>(mc: &Mutation<'gc>, callback: C) -> Self {
        #[repr(C)]
        struct HeaderCallback<'gc, C> {
            header: CallbackInner<'gc>,
            callback: C,
        }

        // SAFETY: We can't auto-implement `Collect` due to the function pointer lifetimes, but
        // function pointers can't hold any data.
        unsafe impl<'gc, C: Collect> Collect for HeaderCallback<'gc, C> {
            fn needs_trace() -> bool
            where
                Self: Sized,
            {
                C::needs_trace()
            }

            fn trace(&self, cc: &gc_arena::Collection) {
                self.callback.trace(cc)
            }
        }

        let hc = Gc::new(
            mc,
            HeaderCallback {
                header: CallbackInner {
                    call: |ptr, ctx, args| unsafe {
                        let hc = ptr as *const HeaderCallback<C>;
                        ((*hc).callback).call(ctx, args)
                    },
                },
                callback,
            },
        );

        Self(unsafe { Gc::cast::<CallbackInner>(hc) })
    }

    pub fn from<F, T>(mc: &Mutation<'gc>, call: F) -> Callback<'gc>
    where
        F: 'static + IntoCallback<'gc, T>,
    {
        Self::from_fn(mc, move |ctx, args| call.call(ctx, args))
    }

    /// Create a callback from a Rust function.
    ///
    /// The function must be `'static` because Rust closures cannot implement `Collect`. If you need
    /// to associate GC data with this function, use [`Callback::from_fn_with`].
    pub fn from_fn<F>(mc: &Mutation<'gc>, call: F) -> Callback<'gc>
    where
        F: 'static + Fn(Context<'gc>, Vec<Value<'gc>>) -> CallbackResult<'gc>,
    {
        Self::from_fn_with(mc, (), move |_, ctx, args| call(ctx, args))
    }

    /// Create a callback from a Rust function together with a GC object.
    pub fn from_fn_with<R, F>(mc: &Mutation<'gc>, root: R, call: F) -> Callback<'gc>
    where
        R: 'gc + Collect,
        F: 'static + Fn(&R, Context<'gc>, Vec<Value<'gc>>) -> CallbackResult<'gc>,
    {
        #[derive(Collect)]
        #[collect(no_drop)]
        struct RootCallback<R, F> {
            root: R,
            #[collect(require_static)]
            call: F,
        }

        impl<'gc, R, F> CallbackFn<'gc> for RootCallback<R, F>
        where
            R: 'gc + Collect,
            F: 'static + Fn(&R, Context<'gc>, Vec<Value<'gc>>) -> CallbackResult<'gc>,
        {
            fn call(&self, ctx: Context<'gc>, args: Vec<Value<'gc>>) -> CallbackResult<'gc> {
                (self.call)(&self.root, ctx, args)
            }
        }

        Callback::new(mc, RootCallback { root, call })
    }

    pub fn call(self, ctx: Context<'gc>, args: Vec<Value<'gc>>) -> CallbackResult<'gc> {
        unsafe { (self.0.call)(Gc::as_ptr(self.0), ctx, args) }
    }
}

#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub struct Varargs<'gc>(Vec<Value<'gc>>);

impl<'gc> ops::Deref for Varargs<'gc> {
    type Target = Vec<Value<'gc>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'gc> ops::DerefMut for Varargs<'gc> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub trait IntoCallback<'gc, Marker> {
    fn call(&self, ctx: Context<'gc>, args: Vec<Value<'gc>>) -> CallbackResult<'gc>;
}

macro_rules! impl_into_callback {
    ($len:literal, $($idx:literal $t:ident),*) => {
        impl<'gc, Func, Ret, $($t,)*> IntoCallback<'gc, fn($($t,)*) -> Ret> for Func
        where
            Func: Fn($($t,)*) -> Ret,
            Ret: IntoCallbackResult<'gc>,
            $($t: FromValue<'gc>,)*
        {
            fn call(&self, ctx: Context<'gc>, args: Vec<Value<'gc>>) -> CallbackResult<'gc> {
                let args_len = args.len();
                let required = ArgumentRange::from($len);
                if !required.contains(args_len) {
                    return Err(Error::new(
                        RuntimeError::CallArguments {
                            required,
                            given: args_len,
                        },
                    ));
                }
                self($($t::from_value(args[$idx])?,)*).into_callback_result(ctx)
            }
        }

        impl<'gc, Func, Ret, $($t,)*> IntoCallback<'gc, fn($($t,)* Varargs<'gc>) -> Ret> for Func
        where
            Func: Fn($($t,)* Varargs<'gc>) -> Ret,
            Ret: IntoCallbackResult<'gc>,
            $($t: FromValue<'gc>,)*
        {
            fn call(&self, ctx: Context<'gc>, args: Vec<Value<'gc>>) -> CallbackResult<'gc> {
                let args_len = args.len();
                let required = ArgumentRange::from(($len, None));
                if !required.contains(args_len) {
                    return Err(Error::new(
                        RuntimeError::CallArguments {
                            required,
                            given: args_len
                        },
                    ));
                }
                self($($t::from_value(args[$idx])?,)* Varargs(args[$len..].to_vec())).into_callback_result(ctx)
            }
        }

        impl<'gc, Func, Ret, $($t,)*> IntoCallback<'gc, fn(Context<'gc>, $($t,)*) -> Ret> for Func
        where
            Func: Fn(Context<'gc>, $($t,)*) -> Ret,
            Ret: IntoCallbackResult<'gc>,
            $($t: FromValue<'gc>,)*
        {
            fn call(&self, ctx: Context<'gc>, args: Vec<Value<'gc>>) -> CallbackResult<'gc> {
                let args_len = args.len();
                let required = ArgumentRange::from($len);
                if !required.contains(args_len) {
                    return Err(Error::new(
                        RuntimeError::CallArguments {
                            required,
                            given: args_len,
                        },
                    ));
                }
                self(ctx, $($t::from_value(args[$idx])?,)*).into_callback_result(ctx)
            }
        }

        impl<'gc, Func, Ret, $($t,)*> IntoCallback<'gc, fn(Context<'gc>, $($t,)* Varargs<'gc>) -> Ret> for Func
        where
            Func: Fn(Context<'gc>, $($t,)* Varargs<'gc>) -> Ret,
            Ret: IntoCallbackResult<'gc>,
            $($t: FromValue<'gc>,)*
        {
            fn call(&self, ctx: Context<'gc>, args: Vec<Value<'gc>>) -> CallbackResult<'gc> {
                let args_len = args.len();
                let required = ArgumentRange::from(($len, None));
                if !required.contains(args_len) {
                    return Err(Error::new(
                        RuntimeError::CallArguments {
                            required,
                            given: args_len,
                        },
                    ));
                }
                self(ctx, $($t::from_value(args[$idx])?,)* Varargs(args[$len..].to_vec())).into_callback_result(ctx)
            }
        }
    };
}

impl_into_callback!(0,);
impl_into_callback!( 1, 0 A);
impl_into_callback!( 2, 0 A, 1 B);
impl_into_callback!( 3, 0 A, 1 B, 2 C);
impl_into_callback!( 4, 0 A, 1 B, 2 C, 3 D);
impl_into_callback!( 5, 0 A, 1 B, 2 C, 3 D, 4 E);
impl_into_callback!( 6, 0 A, 1 B, 2 C, 3 D, 4 E, 5 F);
impl_into_callback!( 7, 0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G);
impl_into_callback!( 8, 0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H);
impl_into_callback!( 9, 0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H, 8 I);
impl_into_callback!(10, 0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H, 8 I, 9 J);
impl_into_callback!(11, 0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H, 8 I, 9 J, 10 K);
impl_into_callback!(12, 0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H, 8 I, 9 J, 10 K, 11 L);
impl_into_callback!(13, 0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H, 8 I, 9 J, 10 K, 11 L, 12 M);
impl_into_callback!(14, 0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H, 8 I, 9 J, 10 K, 11 L, 12 M, 13 N);
impl_into_callback!(15, 0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H, 8 I, 9 J, 10 K, 11 L, 12 M, 13 N, 14 O);
impl_into_callback!(16, 0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H, 8 I, 9 J, 10 K, 11 L, 12 M, 13 N, 14 O, 15 P);

#[cfg(test)]
mod tests {
    use gc_arena::{Arena, Rootable};

    use crate::{context::State, objects::CallbackReturn};

    use super::*;

    #[test]
    fn test_dyn_callback() {
        #[derive(Collect)]
        #[collect(require_static)]
        struct CB();

        impl<'gc> CallbackFn<'gc> for CB {
            fn call(&self, _ctx: Context<'gc>, _args: Vec<Value<'gc>>) -> CallbackResult<'gc> {
                Ok(CallbackReturn::Return(Value::Int(42)))
            }
        }

        #[allow(clippy::redundant_closure)]
        let arena = Arena::<Rootable![State<'_>]>::new(|mc| State::new(mc));
        arena.mutate(|mc, state| {
            let ctx = state.ctx(mc);
            let dyn_callback = Callback::new(mc, CB());
            assert_eq!(
                dyn_callback.call(ctx, Vec::new()),
                Ok(CallbackReturn::Return(Value::Int(42)))
            );
        });
    }
}
