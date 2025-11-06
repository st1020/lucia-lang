use std::{
    fmt,
    ops::{Bound, RangeBounds},
};

use derive_more::{From, IsVariant, TryInto};
use gc_arena::{Collect, Gc};

use crate::{
    Context,
    compiler::value::MetaMethod,
    objects::{Callback, Closure, IntoMetaResult, impl_metamethod},
};

/// Enum of lucia function (Closure / Callback).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Collect, From, TryInto, IsVariant)]
#[collect(no_drop)]
pub enum Function<'gc> {
    Closure(Closure<'gc>),
    Callback(Callback<'gc>),
}

impl Function<'_> {
    pub fn const_ptr(&self) -> *const () {
        match self {
            Function::Closure(v) => Gc::as_ptr(v.into_inner()) as *const (),
            Function::Callback(v) => Gc::as_ptr(v.into_inner()) as *const (),
        }
    }
}

impl fmt::Display for Function<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<function {:p}>", self.const_ptr())
    }
}

impl<'gc> MetaMethod<Context<'gc>> for Function<'gc> {
    impl_metamethod!(Function);

    fn meta_call(&self, _ctx: Context<'gc>) -> Result<Self::ResultCall, Self::Error> {
        Ok(*self)
    }

    fn meta_iter(&self, _ctx: Context<'gc>) -> Result<Self::ResultIter, Self::Error> {
        Ok(*self)
    }

    impl_metamethod!(Function, eq_ne);
}

/// The required number of arguments when calling a function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Collect)]
#[collect(require_static)]
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

impl ArgumentRange {
    pub fn contains(&self, item: usize) -> bool {
        <Self as RangeBounds<usize>>::contains(self, &item)
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
