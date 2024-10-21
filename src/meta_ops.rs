//! The meta method used in lucia lang.

use std::fmt;

use gc_arena::{lock::RefLock, Collect, Gc};
use smol_str::ToSmolStr;

use crate::{
    errors::{Error, ErrorKind},
    objects::{Callback, CallbackReturn, Function, IntoValue, Str, Table, Value},
    Context,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Collect)]
#[collect(require_static)]
pub enum MetaMethod {
    Call,
    Iter,
    GetAttr,
    GetItem,
    SetAttr,
    SetItem,
    Neg,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,
    Len,
    Bool,
    Int,
    Float,
    Str,
    Repr,
}

macro_rules! meta_operator_error {
    ($ctx:expr, $operator:expr, $arg1:expr) => {
        Error::new(ErrorKind::MetaUnOperator {
            operator: $operator,
            operand: $arg1.value_type(),
        })
    };
    ($ctx:expr, $operator:expr, $arg1:expr, $arg2:expr) => {
        Error::new(ErrorKind::MetaBinOperator {
            operator: $operator,
            operand: ($arg1.value_type(), $arg2.value_type()),
        })
    };
}

impl MetaMethod {
    pub const fn name(self) -> &'static str {
        match self {
            MetaMethod::Call => "__call__",
            MetaMethod::Iter => "__iter__",
            MetaMethod::GetAttr => "__getattr__",
            MetaMethod::GetItem => "__getitem__",
            MetaMethod::SetAttr => "__setattr__",
            MetaMethod::SetItem => "__setitem__",
            MetaMethod::Neg => "__neg__",
            MetaMethod::Add => "__add__",
            MetaMethod::Sub => "__sub__",
            MetaMethod::Mul => "__mul__",
            MetaMethod::Div => "__div__",
            MetaMethod::Rem => "__rem__",
            MetaMethod::Eq => "__eq__",
            MetaMethod::Ne => "__ne__",
            MetaMethod::Gt => "__gt__",
            MetaMethod::Ge => "__ge__",
            MetaMethod::Lt => "__lt__",
            MetaMethod::Le => "__le__",
            MetaMethod::Len => "__len__",
            MetaMethod::Bool => "__bool__",
            MetaMethod::Int => "__int__",
            MetaMethod::Float => "__float__",
            MetaMethod::Str => "__str__",
            MetaMethod::Repr => "__repr__",
        }
    }
}

impl fmt::Display for MetaMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl<'gc> IntoValue<'gc> for MetaMethod {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        self.name().into_value(ctx)
    }
}

#[derive(Debug, Clone, Copy, Collect)]
#[collect(no_drop)]
pub enum MetaResult<'gc, const N: usize> {
    Value(Value<'gc>),
    Call(Function<'gc>, [Value<'gc>; N]),
}

pub fn call<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Result<Function<'gc>, Error<'gc>> {
    if let Value::Function(f) = v {
        return Ok(f);
    }
    let metatable = v
        .metatable()
        .ok_or_else(|| meta_operator_error!(ctx, MetaMethod::Call, v))?;
    match metatable.get(ctx, MetaMethod::Call) {
        Value::Function(f) => Ok(f),
        v @ Value::Table(_) => call(ctx, v),
        v => Err(meta_operator_error!(ctx, MetaMethod::Call, v)),
    }
}

pub fn raw_iter<'gc>(ctx: Context<'gc>, t: Table<'gc>) -> Callback<'gc> {
    Callback::from_fn_with(
        &ctx,
        (t, Gc::new(&ctx, RefLock::new(0usize))),
        |(t, i), ctx, _args| {
            let mut i = i.borrow_mut(&ctx);
            *i += 1;
            Ok(CallbackReturn::Return(t.get_index(*i - 1).map_or(
                Value::Null,
                |(k, v)| {
                    let t = Table::new(&ctx);
                    t.set(ctx, 0, k);
                    t.set(ctx, 1, v);
                    t.into()
                },
            )))
        },
    )
}

pub fn iter<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Result<Function<'gc>, Error<'gc>> {
    if let Some(metatable) = v.metatable() {
        let t = metatable.get(ctx, MetaMethod::Iter);
        if !t.is_null() {
            return Ok(Function::Callback(Callback::from_fn_with(
                &ctx,
                (call(ctx, t)?, v),
                |(f, v), _ctx, _args| Ok(CallbackReturn::TailCall(*f, vec![*v])),
            )));
        }
    }

    match v {
        Value::Table(v) => Ok(Function::Callback(raw_iter(ctx, v))),
        Value::Function(v) => Ok(v),
        _ => Err(meta_operator_error!(ctx, MetaMethod::Iter, v)),
    }
}

macro_rules! get_table {
    ($name:tt, $meta_method:tt) => {
        pub fn $name<'gc>(
            ctx: Context<'gc>,
            table: Value<'gc>,
            key: Value<'gc>,
        ) -> Result<MetaResult<'gc, 2>, Error<'gc>> {
            if let Some(metatable) = table.metatable() {
                let t = metatable.get(ctx, MetaMethod::$meta_method);
                if !t.is_null() {
                    return Ok(MetaResult::Call(call(ctx, t)?, [table, key]));
                }
            }

            match table {
                Value::Table(v) => Ok(MetaResult::Value(v.get(ctx, key))),
                _ => Err(meta_operator_error!(
                    ctx,
                    MetaMethod::$meta_method,
                    table,
                    key
                )),
            }
        }
    };
}

get_table!(get_attr, GetAttr);
get_table!(get_item, GetItem);

macro_rules! set_table {
    ($name:tt, $meta_method:tt) => {
        pub fn $name<'gc>(
            ctx: Context<'gc>,
            table: Value<'gc>,
            key: Value<'gc>,
            value: Value<'gc>,
        ) -> Result<MetaResult<'gc, 3>, Error<'gc>> {
            if let Some(metatable) = table.metatable() {
                let t = metatable.get(ctx, MetaMethod::$meta_method);
                if !t.is_null() {
                    return Ok(MetaResult::Call(call(ctx, t)?, [table, key, value]));
                }
            }

            match table {
                Value::Table(v) => {
                    v.set(ctx, key, value);
                    Ok(MetaResult::Value(Value::Null))
                }
                _ => Err(meta_operator_error!(
                    ctx,
                    MetaMethod::$meta_method,
                    table,
                    key
                )),
            }
        }
    };
}

set_table!(set_attr, SetAttr);
set_table!(set_item, SetItem);

pub fn neg<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Result<MetaResult<'gc, 1>, Error<'gc>> {
    if let Some(metatable) = v.metatable() {
        let t = metatable.get(ctx, MetaMethod::Neg);
        if !t.is_null() {
            return Ok(MetaResult::Call(call(ctx, t)?, [v]));
        }
    }

    match v {
        Value::Int(v) => Ok(MetaResult::Value((-v).into())),
        Value::Float(v) => Ok(MetaResult::Value((-v).into())),
        _ => Err(meta_operator_error!(ctx, MetaMethod::Neg, v)),
    }
}

macro_rules! bin_op {
    ($name:tt, $op:tt, $meta_method:tt) => {
        pub fn $name<'gc>(
            ctx: Context<'gc>,
            v1: Value<'gc>,
            v2: Value<'gc>,
        ) -> Result<MetaResult<'gc, 2>, Error<'gc>> {
            if let Some(metatable) = v1.metatable() {
                let t = metatable.get(ctx, MetaMethod::$meta_method);
                if !t.is_null() {
                    return Ok(MetaResult::Call(call(ctx, t)?, [v1, v2]));
                }
            }

            match (v1, v2) {
                (Value::Int(v1), Value::Int(v2)) => Ok(MetaResult::Value((v1 $op v2).into())),
                (Value::Float(v1), Value::Float(v2)) => Ok(MetaResult::Value((v1 $op v2).into())),
                _ => Err(meta_operator_error!(ctx, MetaMethod::$meta_method, v1, v2)),
            }
        }
    };
}

pub fn add<'gc>(
    ctx: Context<'gc>,
    v1: Value<'gc>,
    v2: Value<'gc>,
) -> Result<MetaResult<'gc, 2>, Error<'gc>> {
    if let Some(metatable) = v1.metatable() {
        let t = metatable.get(ctx, MetaMethod::Add);
        if !t.is_null() {
            return Ok(MetaResult::Call(call(ctx, t)?, [v1, v2]));
        }
    }

    match (v1, v2) {
        (Value::Int(v1), Value::Int(v2)) => Ok(MetaResult::Value((v1 + v2).into())),
        (Value::Float(v1), Value::Float(v2)) => Ok(MetaResult::Value((v1 + v2).into())),
        (Value::Str(v1), Value::Str(v2)) => Ok(MetaResult::Value(Value::Str(Str::new(
            &ctx,
            (v1.to_string() + v2.as_ref()).into(),
        )))),
        _ => Err(meta_operator_error!(ctx, MetaMethod::Add, v1, v2)),
    }
}

bin_op!(sub, -, Sub);
bin_op!(mul, *, Mul);
bin_op!(div, /, Div);
bin_op!(rem, %, Rem);

macro_rules! eq_ne {
    ($name:tt, $op:tt, $meta_method:tt) => {
        pub fn $name<'gc>(
            ctx: Context<'gc>,
            tos: Value<'gc>,
            tos1: Value<'gc>,
        ) -> Result<MetaResult<'gc, 2>, Error<'gc>> {
            if let Some(metatable) = tos1.metatable() {
                let t = metatable.get(ctx, MetaMethod::$meta_method);
                if !t.is_null() {
                    return Ok(MetaResult::Call(call(ctx, t)?, [tos1, tos]));
                }
            }

            Ok(MetaResult::Value((tos1 $op tos).into()))
        }
    };
}

eq_ne!(eq, ==, Eq);
eq_ne!(ne, !=, Ne);

bin_op!(gt, >, Gt);
bin_op!(ge, >=, Ge);
bin_op!(lt, <, Lt);
bin_op!(le, <=, Le);

pub fn len<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Result<MetaResult<'gc, 1>, Error<'gc>> {
    if let Some(metatable) = v.metatable() {
        let t = metatable.get(ctx, MetaMethod::Len);
        if !t.is_null() {
            return Ok(MetaResult::Call(call(ctx, t)?, [v]));
        }
    }

    match v {
        Value::Str(s) => Ok(MetaResult::Value(Value::Int(s.len().try_into().unwrap()))),
        Value::Table(t) => Ok(MetaResult::Value(i64::try_from(t.len()).unwrap().into())),
        _ => Err(meta_operator_error!(ctx, MetaMethod::Len, v)),
    }
}

pub fn bool<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Result<MetaResult<'gc, 1>, Error<'gc>> {
    if let Some(metatable) = v.metatable() {
        let t = metatable.get(ctx, MetaMethod::Bool);
        if !t.is_null() {
            return Ok(MetaResult::Call(call(ctx, t)?, [v]));
        }
    }

    match v {
        Value::Null => Ok(MetaResult::Value(false.into())),
        Value::Bool(v) => Ok(MetaResult::Value(v.into())),
        Value::Int(v) => Ok(MetaResult::Value((v != 0).into())),
        Value::Float(v) => Ok(MetaResult::Value((v.0 != 0.0).into())),
        Value::Str(v) => Ok(MetaResult::Value((!v.is_empty()).into())),
        Value::Table(v) => Ok(MetaResult::Value((!v.is_empty()).into())),
        _ => Ok(MetaResult::Value(true.into())),
    }
}

pub fn int<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Result<MetaResult<'gc, 1>, Error<'gc>> {
    if let Some(metatable) = v.metatable() {
        let t = metatable.get(ctx, MetaMethod::Int);
        if !t.is_null() {
            return Ok(MetaResult::Call(call(ctx, t)?, [v]));
        }
    }

    match v {
        Value::Null => Ok(MetaResult::Value(0.into())),
        Value::Bool(v) => Ok(MetaResult::Value((if v { 1 } else { 0 }).into())),
        Value::Int(v) => Ok(MetaResult::Value(v.into())),
        Value::Float(v) => Ok(MetaResult::Value((v.0 as i64).into())),
        Value::Str(s) => Ok(MetaResult::Value(
            s.parse::<i64>()
                .map_err(|_| meta_operator_error!(ctx, MetaMethod::Int, v))?
                .into(),
        )),
        _ => Err(meta_operator_error!(ctx, MetaMethod::Int, v)),
    }
}

pub fn float<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Result<MetaResult<'gc, 1>, Error<'gc>> {
    if let Some(metatable) = v.metatable() {
        let t = metatable.get(ctx, MetaMethod::Float);
        if !t.is_null() {
            return Ok(MetaResult::Call(call(ctx, t)?, [v]));
        }
    }

    match v {
        Value::Null => Ok(MetaResult::Value((0.0).into())),
        Value::Bool(v) => Ok(MetaResult::Value((if v { 1.0 } else { 0.0 }).into())),
        Value::Int(v) => Ok(MetaResult::Value((v as f64).into())),
        Value::Float(v) => Ok(MetaResult::Value(v.into())),
        Value::Str(s) => Ok(MetaResult::Value(
            s.parse::<f64>()
                .map_err(|_| meta_operator_error!(ctx, MetaMethod::Int, v))?
                .into(),
        )),
        _ => Err(meta_operator_error!(ctx, MetaMethod::Float, v)),
    }
}

pub fn str<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Result<MetaResult<'gc, 1>, Error<'gc>> {
    if let Some(metatable) = v.metatable() {
        let t = metatable.get(ctx, MetaMethod::Str);
        if !t.is_null() {
            return Ok(MetaResult::Call(call(ctx, t)?, [v]));
        }
    }

    Ok(MetaResult::Value(v.to_smolstr().into_value(ctx)))
}

pub fn repr<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Result<MetaResult<'gc, 1>, Error<'gc>> {
    if let Some(metatable) = v.metatable() {
        let t = metatable.get(ctx, MetaMethod::Repr);
        if !t.is_null() {
            return Ok(MetaResult::Call(call(ctx, t)?, [v]));
        }
    }

    Ok(MetaResult::Value(v.repr().into_value(ctx)))
}
