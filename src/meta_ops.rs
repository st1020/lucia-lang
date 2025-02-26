//! The meta method used in lucia lang.

use std::{
    fmt,
    ops::{Div, Mul, Rem, Sub},
};

use compact_str::ToCompactString;
use gc_arena::{lock::RefLock, Collect, Gc};

use crate::{
    errors::{Error, RuntimeError},
    objects::{Callback, CallbackReturn, Equal, Function, IntoValue, Repr, Str, Table, Value},
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

macro_rules! meta_operator_error {
    ($ctx:expr, $operator:expr, $arg1:expr) => {
        Error::new(RuntimeError::MetaUnOperator {
            operator: $operator,
            operand: $arg1.value_type(),
        })
    };
    ($ctx:expr, $operator:expr, $arg1:expr, $arg2:expr) => {
        Error::new(RuntimeError::MetaBinOperator {
            operator: $operator,
            operand: ($arg1.value_type(), $arg2.value_type()),
        })
    };
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
        Gc::new(&ctx, RefLock::new(t.iter())),
        |iter, ctx, _args| {
            Ok(CallbackReturn::Return(iter.borrow_mut(&ctx).next().map_or(
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
    ($name:ident, $meta_method:ident) => {
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
    ($name:ident, $meta_method:ident) => {
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
        (Value::Int(v1), Value::Int(v2)) => Ok(MetaResult::Value((v1.wrapping_add(v2)).into())),
        (Value::Float(v1), Value::Float(v2)) => Ok(MetaResult::Value((v1 + v2).into())),
        (Value::Str(v1), Value::Str(v2)) => Ok(MetaResult::Value(Value::Str(Str::new(
            &ctx,
            (v1.to_string() + v2.as_ref()).into(),
        )))),
        _ => Err(meta_operator_error!(ctx, MetaMethod::Add, v1, v2)),
    }
}

macro_rules! sub_mul {
    ($name:ident, $int_op:ident, $float_op:ident, $meta_method:ident) => {
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
                (Value::Int(v1), Value::Int(v2)) => Ok(MetaResult::Value((v1.$int_op(v2)).into())),
                (Value::Float(v1), Value::Float(v2)) => {
                    Ok(MetaResult::Value((v1.$float_op(v2)).into()))
                }
                _ => Err(meta_operator_error!(ctx, MetaMethod::$meta_method, v1, v2)),
            }
        }
    };
}
sub_mul!(sub, wrapping_sub, sub, Sub);
sub_mul!(mul, wrapping_mul, mul, Mul);

macro_rules! div_rem {
    ($name:ident, $int_op:ident, $float_op:ident, $meta_method:ident) => {
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
                (Value::Int(v1), Value::Int(v2)) => {
                    if v2 == 0 {
                        Err(Error::new(RuntimeError::DivideByZero))
                    } else {
                        Ok(MetaResult::Value((v1.$int_op(v2)).into()))
                    }
                }
                (Value::Float(v1), Value::Float(v2)) => {
                    Ok(MetaResult::Value((v1.$float_op(v2)).into()))
                }
                _ => Err(meta_operator_error!(ctx, MetaMethod::$meta_method, v1, v2)),
            }
        }
    };
}
div_rem!(div, wrapping_div, div, Div);
div_rem!(rem, wrapping_rem, rem, Rem);

macro_rules! eq_ne {
    ($name:ident, $op:ident, $meta_method:ident) => {
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

            Ok(MetaResult::Value((v1.$op(&v2)).into()))
        }
    };
}
eq_ne!(eq, equal, Eq);
eq_ne!(ne, not_equal, Ne);

macro_rules! cmp {
    ($name:ident, $op:ident, $meta_method:ident) => {
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
                (Value::Int(v1), Value::Int(v2)) => Ok(MetaResult::Value((v1.$op(&v2)).into())),
                (Value::Float(v1), Value::Float(v2)) => Ok(MetaResult::Value((v1.$op(&v2)).into())),
                _ => Err(meta_operator_error!(ctx, MetaMethod::$meta_method, v1, v2)),
            }
        }
    };
}
cmp!(gt, gt, Gt);
cmp!(ge, ge, Ge);
cmp!(lt, lt, Lt);
cmp!(le, le, Le);

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

    Ok(MetaResult::Value(v.to_compact_string().into_value(ctx)))
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
