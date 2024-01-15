use std::{
    fmt,
    hash::{Hash, Hasher},
};

use gc_arena::{Collect, Gc, Mutation};

use crate::{
    errors::Error,
    meta_ops::MetaResult,
    objects::{Function, Value},
    Context,
};

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

pub trait CallbackFn<'gc>: Collect {
    fn call(
        &mut self,
        ctx: Context<'gc>,
        args: Vec<Value<'gc>>,
    ) -> Result<CallbackReturn<'gc>, Error<'gc>>;
}

// Represents a callback as a single pointer with an inline VTable header.
#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Callback<'gc>(Gc<'gc, CallbackInner<'gc>>);

pub struct CallbackInner<'gc> {
    call: unsafe fn(
        *const CallbackInner<'gc>,
        Context<'gc>,
        Vec<Value<'gc>>,
    ) -> Result<CallbackReturn<'gc>, Error<'gc>>,
}

impl<'gc> Callback<'gc> {
    pub fn new<C: CallbackFn<'gc> + 'gc>(mc: &Mutation<'gc>, callback: C) -> Self {
        #[repr(C)]
        struct HeaderCallback<'gc, C> {
            header: CallbackInner<'gc>,
            callback: C,
        }

        // SAFETY: We can't auto-implement `Collect` due to the function pointer lifetimes, but
        // function pointers can't hold any data. It would be nice if function pointers could have
        // higher rank `for<'gc>` lifetimes.
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
                        let hc = ptr as *mut HeaderCallback<C>;
                        ((*hc).callback).call(ctx, args)
                    },
                },
                callback,
            },
        );

        Self(unsafe { Gc::cast::<CallbackInner>(hc) })
    }

    pub fn from_fn<F>(mc: &Mutation<'gc>, call: F) -> Callback<'gc>
    where
        F: 'static + Fn(Context<'gc>, Vec<Value<'gc>>) -> Result<CallbackReturn<'gc>, Error<'gc>>,
    {
        Self::from_fn_with(mc, (), move |_, ctx, args| call(ctx, args))
    }

    pub fn from_fn_with<R, F>(mc: &Mutation<'gc>, root: R, call: F) -> Callback<'gc>
    where
        R: 'gc + Collect,
        F: 'static
            + Fn(&R, Context<'gc>, Vec<Value<'gc>>) -> Result<CallbackReturn<'gc>, Error<'gc>>,
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
            F: 'static
                + Fn(&R, Context<'gc>, Vec<Value<'gc>>) -> Result<CallbackReturn<'gc>, Error<'gc>>,
        {
            fn call(
                &mut self,
                ctx: Context<'gc>,
                args: Vec<Value<'gc>>,
            ) -> Result<CallbackReturn<'gc>, Error<'gc>> {
                (self.call)(&self.root, ctx, args)
            }
        }

        Callback::new(mc, RootCallback { root, call })
    }

    pub fn from_inner(inner: Gc<'gc, CallbackInner<'gc>>) -> Self {
        Self(inner)
    }

    pub fn into_inner(self) -> Gc<'gc, CallbackInner<'gc>> {
        self.0
    }

    pub fn call(
        self,
        ctx: Context<'gc>,
        args: Vec<Value<'gc>>,
    ) -> Result<CallbackReturn<'gc>, Error<'gc>> {
        unsafe { (self.0.call)(Gc::as_ptr(self.0), ctx, args) }
    }
}

impl<'gc> fmt::Debug for Callback<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_tuple("Callback")
            .field(&Gc::as_ptr(self.0))
            .finish()
    }
}

impl<'gc> PartialEq for Callback<'gc> {
    fn eq(&self, other: &Callback<'gc>) -> bool {
        Gc::ptr_eq(self.0, other.0)
    }
}

impl<'gc> Eq for Callback<'gc> {}

impl<'gc> Hash for Callback<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Gc::as_ptr(self.0).hash(state)
    }
}

#[cfg(test)]
mod tests {
    use gc_arena::{Arena, Rootable};

    use crate::{context::State, objects::CallbackReturn};

    use super::*;

    #[test]
    fn test_dyn_callback() {
        #[derive(Collect)]
        #[collect(require_static)]
        struct CB(i64);

        impl<'gc> CallbackFn<'gc> for CB {
            fn call(
                &mut self,
                _ctx: Context<'gc>,
                _args: Vec<Value<'gc>>,
            ) -> Result<CallbackReturn<'gc>, Error<'gc>> {
                Ok(CallbackReturn::Return(Value::Int(42)))
            }
        }

        #[allow(clippy::redundant_closure)]
        let arena = Arena::<Rootable![State<'_>]>::new(|mc| State::new(mc));
        arena.mutate(|mc, state| {
            let ctx = state.ctx(mc);
            let dyn_callback = Callback::new(mc, CB(17));
            assert_eq!(
                dyn_callback.call(ctx, Vec::new()),
                Ok(CallbackReturn::Return(Value::Int(42)))
            );
        });
    }
}
