use std::fmt;

use gc_arena::{Collect, DynamicRoot, DynamicRootSet, Mutation, Rootable};

use crate::{
    objects::{Callback, Closure, Function, RuntimeCode, Str, Table, UserData, Value},
    utils::{Float, impl_enum_from},
};

/// A collection of stashed values.
#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Registry<'gc> {
    roots: DynamicRootSet<'gc>,
}

impl<'gc> Registry<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> Self {
        Self {
            roots: DynamicRootSet::new(mc),
        }
    }

    /// Returns the inner [`DynamicRootSet`] held inside the global registry.
    ///
    /// This can be used to create `'static` roots directly without having to deal with the
    /// [`Stashable`] trait.
    pub fn roots(&self) -> DynamicRootSet<'gc> {
        self.roots
    }

    /// "Stash" a value with a `'gc` branding lifetime in the global registry, creating a `'static`
    /// handle to it.
    ///
    /// This works for any type that implements the [`Stashable`] trait.
    pub fn stash<S: Stashable<'gc>>(&self, mc: &Mutation<'gc>, s: S) -> S::Stashed {
        s.stash(mc, self.roots)
    }

    /// "Fetch" the real value for a handle that has been returned from `Registry::stash`.
    ///
    /// It can be implemented for external types by implementing the [`Fetchable`] trait.
    pub fn fetch<F: Fetchable>(&self, f: &F) -> F::Fetched<'gc> {
        f.fetch(self.roots)
    }
}

/// A trait for types that can be stashed into a [`DynamicRootSet`].
pub trait Stashable<'gc> {
    type Stashed;

    fn stash(self, mc: &Mutation<'gc>, roots: DynamicRootSet<'gc>) -> Self::Stashed;
}

/// A trait for types that can be fetched from a [`DynamicRootSet`].
pub trait Fetchable {
    type Fetched<'gc>;

    fn fetch<'gc>(&self, roots: DynamicRootSet<'gc>) -> Self::Fetched<'gc>;
}

macro_rules! define_stash {
    ($t:ident, $r:ident) => {
        #[derive(Clone)]
        pub struct $r(DynamicRoot<Rootable![$t<'_>]>);

        impl fmt::Debug for $r {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_tuple(stringify!($r))
                    .field(&self.0.as_ptr())
                    .finish()
            }
        }

        impl<'gc> Stashable<'gc> for $t<'gc> {
            type Stashed = $r;

            fn stash(self, mc: &Mutation<'gc>, roots: DynamicRootSet<'gc>) -> Self::Stashed {
                $r(roots.stash::<Rootable![$t<'_>]>(mc, self))
            }
        }

        impl Fetchable for $r {
            type Fetched<'gc> = $t<'gc>;

            fn fetch<'gc>(&self, roots: DynamicRootSet<'gc>) -> Self::Fetched<'gc> {
                *roots.fetch(&self.0)
            }
        }
    };
}

define_stash!(Str, StashedStr);
define_stash!(Table, StashedTable);
define_stash!(Closure, StashedClosure);
define_stash!(Callback, StashedCallback);
define_stash!(UserData, StashedUserData);
define_stash!(RuntimeCode, StashedRuntimeCode);

#[derive(Debug, Clone)]
pub enum StashedFunction {
    Closure(StashedClosure),
    Callback(StashedCallback),
}

impl_enum_from!(StashedFunction, {
    Closure(StashedClosure),
    Callback(StashedCallback),
});

impl<'gc> Stashable<'gc> for Function<'gc> {
    type Stashed = StashedFunction;

    fn stash(self, mc: &Mutation<'gc>, roots: DynamicRootSet<'gc>) -> Self::Stashed {
        match self {
            Function::Closure(c) => StashedFunction::Closure(c.stash(mc, roots)),
            Function::Callback(c) => StashedFunction::Callback(c.stash(mc, roots)),
        }
    }
}

impl Fetchable for StashedFunction {
    type Fetched<'gc> = Function<'gc>;

    fn fetch<'gc>(&self, roots: DynamicRootSet<'gc>) -> Self::Fetched<'gc> {
        match self {
            StashedFunction::Closure(c) => Function::Closure(c.fetch(roots)),
            StashedFunction::Callback(c) => Function::Callback(c.fetch(roots)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum StashedValue {
    Null,
    Bool(bool),
    Int(i64),
    Float(Float),
    Str(StashedStr),
    Table(StashedTable),
    Function(StashedFunction),
    UserData(StashedUserData),
}

impl_enum_from!(StashedValue, {
    Bool(bool),
    Int(i64),
    Float(Float),
    Str(StashedStr),
    Table(StashedTable),
    Function(StashedFunction),
    UserData(StashedUserData),
});

impl<'gc> Stashable<'gc> for Value<'gc> {
    type Stashed = StashedValue;

    fn stash(self, mc: &Mutation<'gc>, roots: DynamicRootSet<'gc>) -> Self::Stashed {
        match self {
            Value::Null => StashedValue::Null,
            Value::Bool(b) => StashedValue::Bool(b),
            Value::Int(i) => StashedValue::Int(i),
            Value::Float(n) => StashedValue::Float(n),
            Value::Str(s) => StashedValue::Str(s.stash(mc, roots)),
            Value::Table(t) => StashedValue::Table(t.stash(mc, roots)),
            Value::Function(f) => StashedValue::Function(f.stash(mc, roots)),
            Value::UserData(u) => StashedValue::UserData(u.stash(mc, roots)),
        }
    }
}

impl Fetchable for StashedValue {
    type Fetched<'gc> = Value<'gc>;

    fn fetch<'gc>(&self, roots: DynamicRootSet<'gc>) -> Self::Fetched<'gc> {
        match self {
            StashedValue::Null => Value::Null,
            StashedValue::Bool(b) => Value::Bool(*b),
            StashedValue::Int(i) => Value::Int(*i),
            StashedValue::Float(n) => Value::Float(*n),
            StashedValue::Str(s) => Value::Str(s.fetch(roots)),
            StashedValue::Table(t) => Value::Table(t.fetch(roots)),
            StashedValue::Function(f) => Value::Function(f.fetch(roots)),
            StashedValue::UserData(u) => Value::UserData(u.fetch(roots)),
        }
    }
}
