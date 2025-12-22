#![allow(clippy::arbitrary_source_item_ordering)]

use std::{hash, marker::PhantomData, ops::RangeBounds, ptr, rc::Rc};

use derive_more::{Debug, Display};

use crate::{
    Context,
    errors::Error,
    objects::{ArgumentRange, Effect, FromValue, Function, MetaResult, Value},
};

pub type CallbackResult = Result<CallbackReturn, Error>;

#[derive(Debug, PartialEq, Eq)]
pub enum CallbackReturn {
    Return(Value),
    TailCall(Function, Vec<Value>),
    TailEffect(Effect, Vec<Value>),
}

impl<T: Into<Value>> From<T> for CallbackReturn {
    fn from(value: T) -> Self {
        CallbackReturn::Return(value.into())
    }
}

impl<const N: usize> From<MetaResult<N>> for CallbackReturn {
    fn from(value: MetaResult<N>) -> Self {
        match value {
            MetaResult::Value(v) => CallbackReturn::Return(v),
            MetaResult::TailCall(f, args) => CallbackReturn::TailCall(f, Vec::from(args)),
            MetaResult::TailEffect(e, args) => CallbackReturn::TailEffect(e, args),
        }
    }
}

pub trait IntoCallbackResult {
    fn into_callback_result(self) -> CallbackResult;
}

impl<T: Into<CallbackReturn>> IntoCallbackResult for T {
    fn into_callback_result(self) -> CallbackResult {
        Ok(self.into())
    }
}

impl<T: Into<CallbackReturn>> IntoCallbackResult for Result<T, Error> {
    fn into_callback_result(self) -> CallbackResult {
        self.map(Into::into)
    }
}

pub trait CallbackFn {
    fn call(&self, ctx: &Context, args: &[Value]) -> CallbackResult;
}

pub type Callback = Rc<CallbackInner>;

#[derive(Display, Debug)]
#[display("<callback {self:p}>")]
#[debug("{self}")]
pub struct CallbackInner(Box<dyn CallbackFn>);

impl PartialEq for CallbackInner {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(ptr::from_ref(&*self.0), ptr::from_ref(&*other.0))
    }
}

impl Eq for CallbackInner {}

impl hash::Hash for CallbackInner {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        ptr::from_ref(&*self.0).hash(state);
    }
}

impl CallbackInner {
    pub fn new<F: CallbackFn + 'static>(f: F) -> Self {
        Self(Box::new(f))
    }

    pub fn from_fn<F, T>(call: F) -> Self
    where
        F: IntoCallback<(), T> + 'static,
        T: 'static,
    {
        Self::from_fn_with((), call)
    }

    pub fn from_fn_with<V, F, T>(value: V, call: F) -> Self
    where
        V: 'static,
        F: IntoCallback<V, T> + 'static,
        T: 'static,
    {
        struct Shim<V, F, T>(V, F, PhantomData<T>);

        impl<V, F, T> CallbackFn for Shim<V, F, T>
        where
            F: IntoCallback<V, T>,
        {
            fn call(&self, ctx: &Context, args: &[Value]) -> CallbackResult {
                self.1.call(&self.0, ctx, args)
            }
        }

        Self(Box::new(Shim(value, call, PhantomData)))
    }

    pub fn call(&self, ctx: &Context, args: &[Value]) -> CallbackResult {
        self.0.call(ctx, args)
    }
}

impl From<CallbackInner> for Value {
    fn from(value: CallbackInner) -> Value {
        Value::Function(Rc::new(value).into())
    }
}

pub trait IntoCallback<V, Marker> {
    fn call(&self, value: &V, ctx: &Context, args: &[Value]) -> CallbackResult;
}

macro_rules! impl_into_callback {
    (@CHECK_ARGS $args:ident, $argument_range:expr) => {
        let args_len = $args.len();
        let required = ArgumentRange::from($argument_range);
        if !required.contains(&args_len) {
            return Err(Error::CallArguments {
                required,
                given: args_len
            });
        }
    };
    ($len:literal, $($idx:literal $t:ident),*) => {
        impl<Func, Ret, $($t,)*> IntoCallback<(), fn($($t,)*) -> Ret> for Func
        where
            Func: Fn($($t,)*) -> Ret,
            Ret: IntoCallbackResult,
            $($t: FromValue,)*
        {
            fn call(&self, _value: &(), _ctx: &Context, args: &[Value]) -> CallbackResult {
                impl_into_callback!(@CHECK_ARGS args, $len);
                debug_assert!(args.len() == $len);
                self($($t::from_value(args[$idx].clone())?,)*).into_callback_result()
            }
        }

        #[allow(unused_comparisons, clippy::allow_attributes)]
        impl<Func, Ret, $($t,)*> IntoCallback<(), fn($($t,)* &[Value]) -> Ret> for Func
        where
            Func: Fn($($t,)* &[Value]) -> Ret,
            Ret: IntoCallbackResult,
            $($t: FromValue,)*
        {
            fn call(&self, _value: &(), _ctx: &Context, args: &[Value]) -> CallbackResult {
                impl_into_callback!(@CHECK_ARGS args, ($len, None));
                debug_assert!(args.len() >= $len);
                self($($t::from_value(args[$idx].clone())?,)* &args[$len..]).into_callback_result()
            }
        }

        impl<Func, Ret, $($t,)*> IntoCallback<(), fn(Context, $($t,)*) -> Ret> for Func
        where
            Func: Fn(&Context, $($t,)*) -> Ret,
            Ret: IntoCallbackResult,
            $($t: FromValue,)*
        {
            fn call(&self, _value: &(), ctx: &Context, args: &[Value]) -> CallbackResult {
                impl_into_callback!(@CHECK_ARGS args, $len);
                debug_assert!(args.len() == $len);
                self(ctx, $($t::from_value(args[$idx].clone())?,)*).into_callback_result()
            }
        }

        #[allow(unused_comparisons, clippy::allow_attributes)]
        impl<Func, Ret, $($t,)*> IntoCallback<(), fn(Context, $($t,)* &[Value]) -> Ret> for Func
        where
            Func: Fn(&Context, $($t,)* &[Value]) -> Ret,
            Ret: IntoCallbackResult,
            $($t: FromValue,)*
        {
            fn call(&self, _value: &(), ctx: &Context, args: &[Value]) -> CallbackResult {
                impl_into_callback!(@CHECK_ARGS args, ($len, None));
                debug_assert!(args.len() >= $len);
                self(ctx, $($t::from_value(args[$idx].clone())?,)* &args[$len..]).into_callback_result()
            }
        }

        impl<V, Func, Ret, $($t,)*> IntoCallback<V, fn(&V, $($t,)*) -> Ret> for Func
        where
            Func: Fn(&V, $($t,)*) -> Ret,
            Ret: IntoCallbackResult,
            $($t: FromValue,)*
        {
            fn call(&self, value: &V, _ctx: &Context, args: &[Value]) -> CallbackResult {
                impl_into_callback!(@CHECK_ARGS args, $len);
                debug_assert!(args.len() == $len);
                self(value, $($t::from_value(args[$idx].clone())?,)*).into_callback_result()
            }
        }

        #[allow(unused_comparisons, clippy::allow_attributes)]
        impl<V, Func, Ret, $($t,)*> IntoCallback<V, fn(&V, $($t,)* &[Value]) -> Ret> for Func
        where
            Func: Fn(&V, $($t,)* &[Value]) -> Ret,
            Ret: IntoCallbackResult,
            $($t: FromValue,)*
        {
            fn call(&self, value: &V, _ctx: &Context, args: &[Value]) -> CallbackResult {
                impl_into_callback!(@CHECK_ARGS args, ($len, None));
                debug_assert!(args.len() >= $len);
                self(value, $($t::from_value(args[$idx].clone())?,)* &args[$len..]).into_callback_result()
            }
        }

        impl<V, Func, Ret, $($t,)*> IntoCallback<V, fn(&V, &Context, $($t,)*) -> Ret> for Func
        where
            Func: Fn(&V, &Context, $($t,)*) -> Ret,
            Ret: IntoCallbackResult,
            $($t: FromValue,)*
        {
            fn call(&self, value: &V, ctx: &Context, args: &[Value]) -> CallbackResult {
                impl_into_callback!(@CHECK_ARGS args, $len);
                debug_assert!(args.len() == $len);
                self(value, ctx, $($t::from_value(args[$idx].clone())?,)*).into_callback_result()
            }
        }

        #[allow(unused_comparisons, clippy::allow_attributes)]
        impl<V, Func, Ret, $($t,)*> IntoCallback<V, fn(&V, &Context, $($t,)* &[Value]) -> Ret> for Func
        where
            Func: Fn(&V, &Context, $($t,)* &[Value]) -> Ret,
            Ret: IntoCallbackResult,
            $($t: FromValue,)*
        {
            fn call(&self, value: &V, ctx: &Context, args: &[Value]) -> CallbackResult {
                impl_into_callback!(@CHECK_ARGS args, ($len, None));
                debug_assert!(args.len() >= $len);
                self(value, ctx, $($t::from_value(args[$idx].clone())?,)* &args[$len..]).into_callback_result()
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
    use std::cell::Cell;

    use crate::{Context, objects::CallbackReturn};

    use super::*;

    #[test]
    fn test_dyn_callback() {
        struct CB;

        impl CallbackFn for CB {
            fn call(&self, _ctx: &Context, _args: &[Value]) -> CallbackResult {
                Ok(CallbackReturn::Return(Value::Int(42)))
            }
        }

        let context = Context::empty();
        let dyn_callback = CallbackInner::new(CB);
        assert_eq!(
            dyn_callback.call(&context, &[]),
            Ok(CallbackReturn::Return(Value::Int(42)))
        );
    }

    #[test]
    fn test_callback_from_fn_with() {
        let context = Context::empty();
        let callback = CallbackInner::from_fn_with(Cell::new(0), |cell: &Cell<usize>| {
            let value = cell.get();
            cell.set(value + 1);
            value
        });
        assert_eq!(
            callback.call(&context, &[]),
            Ok(CallbackReturn::Return(Value::Int(0)))
        );
        assert_eq!(
            callback.call(&context, &[]),
            Ok(CallbackReturn::Return(Value::Int(1)))
        );
    }
}
