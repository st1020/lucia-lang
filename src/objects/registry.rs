use std::fmt;

use gc_arena::{Collect, DynamicRoot, DynamicRootSet, Mutation, Rootable};

use crate::{
    objects::{Callback, Closure, Function, GcError, Str, Table, UserData, Value},
    utils::Float,
    Context,
};

#[derive(Clone)]
pub struct StaticStr(pub DynamicRoot<Rootable![Str<'_>]>);

impl fmt::Debug for StaticStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StaticStr").field(&self.0.as_ptr()).finish()
    }
}

#[derive(Clone)]
pub struct StaticTable(pub DynamicRoot<Rootable![Table<'_>]>);

impl fmt::Debug for StaticTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StaticTable")
            .field(&self.0.as_ptr())
            .finish()
    }
}

#[derive(Clone)]
pub struct StaticClosure(pub DynamicRoot<Rootable![Closure<'_>]>);

impl fmt::Debug for StaticClosure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StaticClosure")
            .field(&self.0.as_ptr())
            .finish()
    }
}

#[derive(Clone)]
pub struct StaticCallback(pub DynamicRoot<Rootable![Callback<'_>]>);

impl fmt::Debug for StaticCallback {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StaticCallback")
            .field(&self.0.as_ptr())
            .finish()
    }
}

#[derive(Clone)]
pub struct StaticUserData(pub DynamicRoot<Rootable![UserData<'_>]>);

impl fmt::Debug for StaticUserData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StaticUserData")
            .field(&self.0.as_ptr())
            .finish()
    }
}

#[derive(Debug, Clone)]
pub enum StaticFunction {
    Closure(StaticClosure),
    Callback(StaticCallback),
}

impl From<StaticClosure> for StaticFunction {
    fn from(closure: StaticClosure) -> Self {
        Self::Closure(closure)
    }
}

impl From<StaticCallback> for StaticFunction {
    fn from(callback: StaticCallback) -> Self {
        Self::Callback(callback)
    }
}

#[derive(Debug, Clone)]
pub enum StaticValue {
    Null,
    Bool(bool),
    Int(i64),
    Float(Float),
    Str(StaticStr),
    Table(StaticTable),
    Function(StaticFunction),
    UserData(StaticUserData),
}

impl From<bool> for StaticValue {
    fn from(v: bool) -> StaticValue {
        StaticValue::Bool(v)
    }
}

impl From<i64> for StaticValue {
    fn from(v: i64) -> StaticValue {
        StaticValue::Int(v)
    }
}

impl From<f64> for StaticValue {
    fn from(v: f64) -> StaticValue {
        StaticValue::Float(v.into())
    }
}

impl From<Float> for StaticValue {
    fn from(v: Float) -> StaticValue {
        StaticValue::Float(v)
    }
}

impl From<StaticStr> for StaticValue {
    fn from(v: StaticStr) -> StaticValue {
        StaticValue::Str(v)
    }
}

impl From<StaticTable> for StaticValue {
    fn from(v: StaticTable) -> StaticValue {
        StaticValue::Table(v)
    }
}

impl From<StaticFunction> for StaticValue {
    fn from(v: StaticFunction) -> StaticValue {
        StaticValue::Function(v)
    }
}

impl From<StaticClosure> for StaticValue {
    fn from(v: StaticClosure) -> StaticValue {
        StaticValue::Function(StaticFunction::Closure(v))
    }
}

impl From<StaticCallback> for StaticValue {
    fn from(v: StaticCallback) -> StaticValue {
        StaticValue::Function(StaticFunction::Callback(v))
    }
}

impl From<StaticUserData> for StaticValue {
    fn from(v: StaticUserData) -> StaticValue {
        StaticValue::UserData(v)
    }
}

pub trait Singleton<'gc> {
    fn create(ctx: Context<'gc>) -> Self;
}

impl<'gc, T: Default> Singleton<'gc> for T {
    fn create(_: Context<'gc>) -> Self {
        Self::default()
    }
}

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

    pub fn roots(&self) -> DynamicRootSet<'gc> {
        self.roots
    }

    pub fn stash<R: Stashable<'gc>>(&self, mc: &Mutation<'gc>, r: R) -> R::Stashed {
        r.stash(&self.roots, mc)
    }

    pub fn fetch<F: Fetchable<'gc>>(&self, f: &F) -> F::Fetched {
        f.fetch(&self.roots)
    }
}

pub trait Stashable<'gc> {
    type Stashed;

    fn stash(self, roots: &DynamicRootSet<'gc>, mc: &Mutation<'gc>) -> Self::Stashed;
}

pub trait Fetchable<'gc> {
    type Fetched;

    fn fetch(&self, roots: &DynamicRootSet<'gc>) -> Self::Fetched;
}

macro_rules! reg_type {
    ($t:ident, $r:ident) => {
        impl<'gc> Stashable<'gc> for $t<'gc> {
            type Stashed = $r;

            fn stash(self, roots: &DynamicRootSet<'gc>, mc: &Mutation<'gc>) -> Self::Stashed {
                $r(roots.stash::<Rootable![$t<'_>]>(mc, self))
            }
        }
    };
}

reg_type!(Str, StaticStr);
reg_type!(Table, StaticTable);
reg_type!(Closure, StaticClosure);
reg_type!(Callback, StaticCallback);
reg_type!(UserData, StaticUserData);

macro_rules! fetch_type {
    ($r:ident, $t:ident) => {
        impl<'gc> Fetchable<'gc> for $r {
            type Fetched = $t<'gc>;

            fn fetch(&self, roots: &DynamicRootSet<'gc>) -> Self::Fetched {
                *roots.fetch::<Rootable![$t<'_>]>(&self.0)
            }
        }
    };
}

fetch_type!(StaticStr, Str);
fetch_type!(StaticTable, Table);
fetch_type!(StaticClosure, Closure);
fetch_type!(StaticCallback, Callback);
fetch_type!(StaticUserData, UserData);

impl<'gc> Stashable<'gc> for Function<'gc> {
    type Stashed = StaticFunction;

    fn stash(self, roots: &DynamicRootSet<'gc>, mc: &Mutation<'gc>) -> Self::Stashed {
        match self {
            Function::Closure(closure) => StaticFunction::Closure(closure.stash(roots, mc)),
            Function::Callback(callback) => StaticFunction::Callback(callback.stash(roots, mc)),
        }
    }
}

impl<'gc> Fetchable<'gc> for StaticFunction {
    type Fetched = Function<'gc>;

    fn fetch(&self, roots: &DynamicRootSet<'gc>) -> Self::Fetched {
        match self {
            StaticFunction::Closure(closure) => Function::Closure(closure.fetch(roots)),
            StaticFunction::Callback(callback) => Function::Callback(callback.fetch(roots)),
        }
    }
}

impl<'gc> Stashable<'gc> for Value<'gc> {
    type Stashed = StaticValue;

    fn stash(self, roots: &DynamicRootSet<'gc>, mc: &Mutation<'gc>) -> Self::Stashed {
        match self {
            Value::Null => StaticValue::Null,
            Value::Bool(b) => StaticValue::Bool(b),
            Value::Int(i) => StaticValue::Int(i),
            Value::Float(n) => StaticValue::Float(n),
            Value::Str(s) => StaticValue::Str(s.stash(roots, mc)),
            Value::Table(t) => StaticValue::Table(t.stash(roots, mc)),
            Value::Function(f) => StaticValue::Function(f.stash(roots, mc)),
            Value::UserData(u) => StaticValue::UserData(u.stash(roots, mc)),
        }
    }
}

impl<'gc> Fetchable<'gc> for StaticValue {
    type Fetched = Value<'gc>;

    fn fetch(&self, roots: &DynamicRootSet<'gc>) -> Self::Fetched {
        match self {
            StaticValue::Null => Value::Null,
            StaticValue::Bool(b) => Value::Bool(*b),
            StaticValue::Int(i) => Value::Int(*i),
            StaticValue::Float(n) => Value::Float(*n),
            StaticValue::Str(s) => Value::Str(s.fetch(roots)),
            StaticValue::Table(t) => Value::Table(t.fetch(roots)),
            StaticValue::Function(f) => Value::Function(f.fetch(roots)),
            StaticValue::UserData(u) => Value::UserData(u.fetch(roots)),
        }
    }
}

#[derive(Clone)]
pub struct StaticError(pub DynamicRoot<Rootable![GcError<'_>]>);

impl fmt::Debug for StaticError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StaticError")
            .field(&self.0.as_ptr())
            .finish()
    }
}

reg_type!(GcError, StaticError);
fetch_type!(StaticError, GcError);
