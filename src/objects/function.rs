use std::{
    fmt,
    ops::{Bound, RangeBounds},
    rc::Rc,
};

use derive_more::{Display, From, IsVariant};

use crate::{
    Context,
    compiler::value::MetaMethod,
    errors::Error,
    objects::{
        Callback, CallbackFn, CallbackReturn, FromValue, RcCallback, RcClosure, RcContinuation,
        Value, ValueType, impl_metamethod, unexpected_type_error,
    },
};

/// Enum of lucia function (Closure / Callback).
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, Display)]
#[display("<function {:p}>", self.const_ptr())]
pub enum Function {
    Closure(RcClosure),
    Callback(RcCallback),
    Continuation(RcContinuation),
}

impl Function {
    pub fn const_ptr(&self) -> *const () {
        match self {
            Function::Closure(v) => Rc::as_ptr(v).cast::<()>(),
            Function::Callback(v) => Rc::as_ptr(v).cast::<()>(),
            Function::Continuation(v) => Rc::as_ptr(v).cast::<()>(),
        }
    }
}

impl MetaMethod<&Context> for Function {
    impl_metamethod!(Function);

    #[inline]
    fn meta_call(self, _ctx: &Context) -> Result<Self::ResultCall, Self::Error> {
        Ok(self)
    }

    #[inline]
    fn meta_iter(self, _ctx: &Context) -> Result<Self::ResultIter, Self::Error> {
        #[derive(Clone)]
        struct FunctionIterCallback(Function);

        impl CallbackFn for FunctionIterCallback {
            fn call(&mut self, _ctx: &Context, args: &[Value]) -> super::CallbackResult {
                ArgumentRange::from(1).check(args)?;
                <()>::from_value(args[0].clone())?;
                Ok(CallbackReturn::TailCall {
                    function: self.0.clone(),
                    args: Vec::new(),
                })
            }
        }

        Ok(Rc::new(Callback::new(FunctionIterCallback(self))).into())
    }

    impl_metamethod!(Function, str);
    impl_metamethod!(Function, repr);

    impl_metamethod!(Function, eq_ne);
}

macro_rules! impl_conversion {
    ($($e:ident($t:ty)),* $(,)?) => {
        $(
            impl From<$t> for Value {
                fn from(value: $t) -> Value {
                    Value::Function(value.into())
                }
            }

            impl FromValue for $t {
                fn from_value(value: Value) -> Result<Self, Error> {
                    if let Value::Function(Function::$e(v)) = value {
                        Ok(v)
                    } else {
                        Err(unexpected_type_error!(ValueType::Function, value))
                    }
                }
            }
        )*
    };
}
impl_conversion! {
    Closure(RcClosure),
    Callback(RcCallback),
    Continuation(RcContinuation),
}

/// The required number of arguments when calling a function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ArgumentRange {
    pub start: usize,
    pub end: Option<usize>,
}

impl ArgumentRange {
    pub fn new(start: usize, end: Option<usize>) -> Self {
        Self { start, end }
    }

    pub fn more_then(start: usize) -> Self {
        Self { start, end: None }
    }

    pub fn check(&self, args: &[Value]) -> Result<(), Error> {
        let args_len = args.len();
        if self.contains(&args_len) {
            Ok(())
        } else {
            Err(Error::CallArguments {
                required: *self,
                given: args_len,
            })
        }
    }

    #[inline]
    pub fn check_iter_callback(args: &[Value], first_call: bool) -> Result<(), Error> {
        ArgumentRange::from(usize::from(!first_call)).check(args)?;
        if !first_call {
            <()>::from_value(args[0].clone())?;
        }
        Ok(())
    }
}

impl From<usize> for ArgumentRange {
    fn from(value: usize) -> Self {
        ArgumentRange {
            start: value,
            end: Some(value),
        }
    }
}

impl From<(usize, usize)> for ArgumentRange {
    fn from(value: (usize, usize)) -> Self {
        ArgumentRange {
            start: value.0,
            end: Some(value.1),
        }
    }
}

impl From<(usize, Option<usize>)> for ArgumentRange {
    fn from(value: (usize, Option<usize>)) -> Self {
        ArgumentRange {
            start: value.0,
            end: value.1,
        }
    }
}

impl RangeBounds<usize> for ArgumentRange {
    fn start_bound(&self) -> Bound<&usize> {
        Bound::Included(&self.start)
    }

    fn end_bound(&self) -> Bound<&usize> {
        if let Some(end) = &self.end {
            Bound::Included(end)
        } else {
            Bound::Unbounded
        }
    }
}

impl fmt::Display for ArgumentRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(end) = self.end {
            if self.start == end {
                write!(f, "{end}")
            } else {
                write!(f, "[{}, {}]", self.start, end)
            }
        } else {
            write!(f, "at least {}", self.start)
        }
    }
}
