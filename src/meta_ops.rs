//! The meta method used in lucia lang.

use std::ops::{Add, Div, Mul, Rem, Sub};

use compact_str::ToCompactString;
use gc_arena::{Collect, static_collect};

use crate::{
    Context,
    compiler::value::{MetaMethod, MetaName},
    errors::{Error, RuntimeError},
    objects::{Callback, CallbackReturn, Equal, Function, IntoValue, Repr, Str, Value},
};

static_collect!(MetaName);

impl<'gc> IntoValue<'gc> for MetaName {
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
    ($operator:expr, $arg1:expr) => {
        Error::new(RuntimeError::MetaUnOperator {
            operator: $operator,
            operand: $arg1.value_type(),
        })
    };
    ($operator:expr, $arg1:expr, $arg2:expr) => {
        Error::new(RuntimeError::MetaBinOperator {
            operator: $operator,
            operand: ($arg1.value_type(), $arg2.value_type()),
        })
    };
}

macro_rules! call_metamethod {
    ($ctx:ident, $meta_name:expr, $v1:ident $(,)? $($arg:ident),*) => {
        if let Some(metatable) = $v1.metatable() {
            let metamethod = metatable.get($ctx, $meta_name);
            if !metamethod.is_null() {
                return Ok(MetaResult::Call($ctx.call(metamethod)?, [$v1, $($arg),*]));
            }
        }
    };
}

impl<'gc> MetaMethod<Value<'gc>> for Context<'gc> {
    type ResultCall = Result<Function<'gc>, Error<'gc>>;
    type ResultIter = Self::ResultCall;
    type Result1 = Result<MetaResult<'gc, 1>, Error<'gc>>;
    type Result2 = Result<MetaResult<'gc, 2>, Error<'gc>>;
    type Result3 = Result<MetaResult<'gc, 3>, Error<'gc>>;

    fn call(self, value: Value<'gc>) -> Self::ResultCall {
        if let Value::Function(f) = value {
            return Ok(f);
        }
        let metatable = value
            .metatable()
            .ok_or_else(|| meta_operator_error!(MetaName::Call, value))?;
        match metatable.get(self, MetaName::Call) {
            Value::Function(f) => Ok(f),
            v @ Value::Table(_) => self.call(v),
            v => Err(meta_operator_error!(MetaName::Call, v)),
        }
    }

    fn iter(self, value: Value<'gc>) -> Self::ResultIter {
        if let Some(metatable) = value.metatable() {
            let t = metatable.get(self, MetaName::Iter);
            if !t.is_null() {
                return Ok(Function::Callback(Callback::from_fn_with(
                    &self,
                    (self.call(t)?, value),
                    |(f, v), _ctx, _args| Ok(CallbackReturn::TailCall(*f, vec![*v])),
                )));
            }
        }
        match value {
            Value::Table(v) => Ok(Function::Callback(v.iter_callback(self))),
            Value::Function(v) => Ok(v),
            _ => Err(meta_operator_error!(MetaName::Iter, value)),
        }
    }

    fn get_attr(self, table: Value<'gc>, key: Value<'gc>) -> Self::Result2 {
        call_metamethod!(self, MetaName::GetAttr, table, key);
        match table {
            Value::Table(v) => Ok(MetaResult::Value(v.get(self, key))),
            _ => Err(meta_operator_error!(MetaName::GetAttr, table, key)),
        }
    }

    fn get_item(self, table: Value<'gc>, key: Value<'gc>) -> Self::Result2 {
        call_metamethod!(self, MetaName::GetItem, table, key);
        match table {
            Value::Table(v) => Ok(MetaResult::Value(v.get(self, key))),
            _ => Err(meta_operator_error!(MetaName::GetItem, table, key)),
        }
    }

    fn set_attr(self, table: Value<'gc>, key: Value<'gc>, value: Value<'gc>) -> Self::Result3 {
        call_metamethod!(self, MetaName::SetAttr, table, key, value);
        match table {
            Value::Table(v) => {
                v.set(self, key, value);
                Ok(MetaResult::Value(Value::Null))
            }
            _ => Err(meta_operator_error!(MetaName::SetAttr, table, key)),
        }
    }

    fn set_item(self, table: Value<'gc>, key: Value<'gc>, value: Value<'gc>) -> Self::Result3 {
        call_metamethod!(self, MetaName::SetItem, table, key, value);
        match table {
            Value::Table(v) => {
                v.set(self, key, value);
                Ok(MetaResult::Value(Value::Null))
            }
            _ => Err(meta_operator_error!(MetaName::SetItem, table, key)),
        }
    }

    fn neg(self, value: Value<'gc>) -> Self::Result1 {
        call_metamethod!(self, MetaName::Neg, value);
        match value {
            Value::Int(v) => Ok(MetaResult::Value((-v).into())),
            Value::Float(v) => Ok(MetaResult::Value((-v).into())),
            _ => Err(meta_operator_error!(MetaName::Neg, value)),
        }
    }

    fn add(self, lhs: Value<'gc>, rhs: Value<'gc>) -> Self::Result2 {
        call_metamethod!(self, MetaName::Add, lhs, rhs);
        match (lhs, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => {
                Ok(MetaResult::Value((lhs.wrapping_add(rhs)).into()))
            }
            (Value::Float(lhs), Value::Float(rhs)) => Ok(MetaResult::Value(lhs.add(rhs).into())),
            (Value::Str(lhs), Value::Str(rhs)) => Ok(MetaResult::Value(Value::Str(Str::new(
                &self,
                lhs.to_compact_string() + rhs.as_ref(),
            )))),
            _ => Err(meta_operator_error!(MetaName::Add, lhs, rhs)),
        }
    }

    fn sub(self, lhs: Value<'gc>, rhs: Value<'gc>) -> Self::Result2 {
        call_metamethod!(self, MetaName::Sub, lhs, rhs);
        match (lhs, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => {
                Ok(MetaResult::Value(lhs.wrapping_sub(rhs).into()))
            }
            (Value::Float(lhs), Value::Float(rhs)) => Ok(MetaResult::Value(lhs.sub(rhs).into())),
            _ => Err(meta_operator_error!(MetaName::Sub, lhs, rhs)),
        }
    }

    fn mul(self, lhs: Value<'gc>, rhs: Value<'gc>) -> Self::Result2 {
        call_metamethod!(self, MetaName::Mul, lhs, rhs);
        match (lhs, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => {
                Ok(MetaResult::Value(lhs.wrapping_mul(rhs).into()))
            }
            (Value::Float(lhs), Value::Float(rhs)) => Ok(MetaResult::Value(lhs.mul(rhs).into())),
            _ => Err(meta_operator_error!(MetaName::Mul, lhs, rhs)),
        }
    }

    fn div(self, lhs: Value<'gc>, rhs: Value<'gc>) -> Self::Result2 {
        call_metamethod!(self, MetaName::Div, lhs, rhs);
        match (lhs, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => {
                if rhs == 0 {
                    Err(Error::new(RuntimeError::DivideByZero))
                } else {
                    Ok(MetaResult::Value(lhs.wrapping_div(rhs).into()))
                }
            }
            (Value::Float(lhs), Value::Float(rhs)) => Ok(MetaResult::Value(lhs.div(rhs).into())),
            _ => Err(meta_operator_error!(MetaName::Div, lhs, rhs)),
        }
    }

    fn rem(self, lhs: Value<'gc>, rhs: Value<'gc>) -> Self::Result2 {
        call_metamethod!(self, MetaName::Rem, lhs, rhs);
        match (lhs, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => {
                if rhs == 0 {
                    Err(Error::new(RuntimeError::DivideByZero))
                } else {
                    Ok(MetaResult::Value(lhs.wrapping_rem(rhs).into()))
                }
            }
            (Value::Float(lhs), Value::Float(rhs)) => Ok(MetaResult::Value(lhs.rem(rhs).into())),
            _ => Err(meta_operator_error!(MetaName::Rem, lhs, rhs)),
        }
    }

    fn eq(self, lhs: Value<'gc>, rhs: Value<'gc>) -> Self::Result2 {
        call_metamethod!(self, MetaName::Eq, lhs, rhs);
        Ok(MetaResult::Value(lhs.equal(&rhs).into()))
    }

    fn ne(self, lhs: Value<'gc>, rhs: Value<'gc>) -> Self::Result2 {
        call_metamethod!(self, MetaName::Ne, lhs, rhs);
        Ok(MetaResult::Value(lhs.not_equal(&rhs).into()))
    }

    fn gt(self, lhs: Value<'gc>, rhs: Value<'gc>) -> Self::Result2 {
        call_metamethod!(self, MetaName::Gt, lhs, rhs);
        match (lhs, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(MetaResult::Value((lhs > rhs).into())),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(MetaResult::Value((lhs > rhs).into())),
            _ => Err(meta_operator_error!(MetaName::Gt, lhs, rhs)),
        }
    }

    fn ge(self, lhs: Value<'gc>, rhs: Value<'gc>) -> Self::Result2 {
        call_metamethod!(self, MetaName::Ge, lhs, rhs);
        match (lhs, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(MetaResult::Value((lhs >= rhs).into())),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(MetaResult::Value((lhs >= rhs).into())),
            _ => Err(meta_operator_error!(MetaName::Ge, lhs, rhs)),
        }
    }

    fn lt(self, lhs: Value<'gc>, rhs: Value<'gc>) -> Self::Result2 {
        call_metamethod!(self, MetaName::Lt, lhs, rhs);
        match (lhs, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(MetaResult::Value((lhs < rhs).into())),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(MetaResult::Value((lhs < rhs).into())),
            _ => Err(meta_operator_error!(MetaName::Lt, lhs, rhs)),
        }
    }

    fn le(self, lhs: Value<'gc>, rhs: Value<'gc>) -> Self::Result2 {
        call_metamethod!(self, MetaName::Le, lhs, rhs);
        match (lhs, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(MetaResult::Value((lhs <= rhs).into())),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(MetaResult::Value((lhs <= rhs).into())),
            _ => Err(meta_operator_error!(MetaName::Le, lhs, rhs)),
        }
    }

    fn len(self, value: Value<'gc>) -> Self::Result1 {
        call_metamethod!(self, MetaName::Len, value);
        match value {
            Value::Str(s) => Ok(MetaResult::Value(Value::Int(s.len().try_into().unwrap()))),
            Value::Table(t) => Ok(MetaResult::Value(i64::try_from(t.len()).unwrap().into())),
            _ => Err(meta_operator_error!(MetaName::Len, value)),
        }
    }

    fn bool(self, value: Value<'gc>) -> Self::Result1 {
        call_metamethod!(self, MetaName::Bool, value);
        match value {
            Value::Null => Ok(MetaResult::Value(false.into())),
            Value::Bool(v) => Ok(MetaResult::Value(v.into())),
            Value::Int(v) => Ok(MetaResult::Value((v != 0).into())),
            Value::Float(v) => Ok(MetaResult::Value((v.0 != 0.0).into())),
            Value::Str(v) => Ok(MetaResult::Value((!v.is_empty()).into())),
            Value::Table(v) => Ok(MetaResult::Value((!v.is_empty()).into())),
            _ => Ok(MetaResult::Value(true.into())),
        }
    }

    fn int(self, value: Value<'gc>) -> Self::Result1 {
        call_metamethod!(self, MetaName::Int, value);
        match value {
            Value::Null => Ok(MetaResult::Value(0.into())),
            Value::Bool(v) => Ok(MetaResult::Value((if v { 1 } else { 0 }).into())),
            Value::Int(v) => Ok(MetaResult::Value(v.into())),
            Value::Float(v) => Ok(MetaResult::Value((v.0 as i64).into())),
            Value::Str(s) => Ok(MetaResult::Value(
                s.parse::<i64>()
                    .map_err(|_| meta_operator_error!(MetaName::Int, value))?
                    .into(),
            )),
            _ => Err(meta_operator_error!(MetaName::Int, value)),
        }
    }

    fn float(self, value: Value<'gc>) -> Self::Result1 {
        call_metamethod!(self, MetaName::Float, value);
        match value {
            Value::Null => Ok(MetaResult::Value((0.0).into())),
            Value::Bool(v) => Ok(MetaResult::Value((if v { 1.0 } else { 0.0 }).into())),
            Value::Int(v) => Ok(MetaResult::Value((v as f64).into())),
            Value::Float(v) => Ok(MetaResult::Value(v.into())),
            Value::Str(s) => Ok(MetaResult::Value(
                s.parse::<f64>()
                    .map_err(|_| meta_operator_error!(MetaName::Int, value))?
                    .into(),
            )),
            _ => Err(meta_operator_error!(MetaName::Float, value)),
        }
    }

    fn str(self, value: Value<'gc>) -> Self::Result1 {
        call_metamethod!(self, MetaName::Str, value);
        Ok(MetaResult::Value(
            value.to_compact_string().into_value(self),
        ))
    }

    fn repr(self, value: Value<'gc>) -> Self::Result1 {
        call_metamethod!(self, MetaName::Repr, value);
        Ok(MetaResult::Value(value.repr().into_value(self)))
    }
}
