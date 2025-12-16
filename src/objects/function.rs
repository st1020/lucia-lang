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
        Callback, CallbackFn, CallbackInner, CallbackReturn, Closure, Continuation, FromValue,
        Value, ValueType, impl_metamethod, unexpected_type_error,
    },
};

/// Enum of lucia function (Closure / Callback).
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, Display)]
#[display("<function {:p}>", self.const_ptr())]
pub enum Function {
    Closure(Closure),
    Callback(Callback),
    Continuation(Continuation),
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
    fn meta_call(self, _: &Context) -> Result<Self::ResultCall, Self::Error> {
        Ok(self)
    }

    #[inline]
    fn meta_iter(self, _: &Context) -> Result<Self::ResultIter, Self::Error> {
        struct FunctionIter(Function);

        impl CallbackFn for FunctionIter {
            fn call(&self, _: &Context, _: &[Value]) -> super::CallbackResult {
                Ok(CallbackReturn::TailCall(self.0.clone(), Vec::new()))
            }
        }

        Ok(Rc::new(CallbackInner::new(FunctionIter(self))).into())
    }

    impl_metamethod!(Function, str);
    impl_metamethod!(Function, repr);

    impl_metamethod!(Function, eq_ne);
}

macro_rules! impl_conversion {
    ($($i:tt),*) => {
        $(
            impl From<$i> for Value {
                fn from(value: $i) -> Value {
                    Value::Function(value.into())
                }
            }

            impl FromValue for $i {
                fn from_value(value: Value) -> Result<Self, Error> {
                    if let Value::Function(Function::$i(v)) = value {
                        Ok(v)
                    } else {
                        Err(unexpected_type_error!(ValueType::Function, value))
                    }
                }
            }
        )*
    };
}
impl_conversion!(Closure, Callback, Continuation);

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
