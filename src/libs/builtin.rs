use std::collections::HashMap;

use crate::errors::{Error, RuntimeError, RuntimeErrorKind};
use crate::objects::{Value, ValueType, BOOL, FLOAT, INT, LEN, REPR, STR};
use crate::{check_args, get_metamethod, return_error, unexpect_type_error};

pub fn builtin_variables() -> HashMap<String, Value> {
    let mut t = HashMap::new();
    t.insert(
        "id".to_string(),
        Value::ExtFunction(|args, lvm| {
            let (v,) = check_args!(lvm, args, Value);
            Ok(v.id().map_or_else(
                || Value::Null,
                |x| Value::Int(usize::from(x).try_into().unwrap()),
            ))
        }),
    );
    t.insert(
        "type".to_string(),
        Value::ExtFunction(|args, lvm| {
            let (v,) = check_args!(lvm, args, Value);
            Ok(lvm.new_str_value(v.value_type().to_string()))
        }),
    );
    t.insert(
        "is_error".to_string(),
        Value::ExtFunction(|args, lvm| {
            let (v,) = check_args!(lvm, args, Value);
            Ok(Value::Bool(v.is_error()))
        }),
    );
    t.insert(
        "panic".to_string(),
        Value::ExtFunction(|args, lvm| {
            let (v,) = check_args!(lvm, args, Value);
            Err(Error::RuntimeError(RuntimeError {
                kind: RuntimeErrorKind::UserPanic(v),
                traceback: lvm.traceback(),
            }))
        }),
    );
    t.insert(
        "assert".to_string(),
        Value::ExtFunction(|args, lvm| {
            let (v, msg) = check_args!(lvm, args, Value | Value);
            if v.is_error() || !(bool::from(v)) {
                Err(Error::RuntimeError(RuntimeError {
                    kind: RuntimeErrorKind::AssertError(msg.unwrap_or(Value::Null)),
                    traceback: lvm.traceback(),
                }))
            } else {
                Ok(v)
            }
        }),
    );
    t.insert(
        "len".to_string(),
        Value::ExtFunction(|args, lvm| {
            let (value,) = check_args!(lvm, args, Value);
            if let Some(v) = get_metamethod!(lvm, value, LEN) {
                lvm.call(v, vec![value])
            } else if let Some(v) = value.as_str() {
                Ok(Value::Int(v.len().try_into().unwrap()))
            } else if let Some(v) = value.as_table() {
                Ok(Value::Int(v.len().try_into().unwrap()))
            } else {
                return_error!(
                    lvm,
                    unexpect_type_error!(
                        value.value_type(),
                        vec![ValueType::Str, ValueType::Table]
                    )
                );
            }
        }),
    );
    t.insert(
        "bool".to_string(),
        Value::ExtFunction(|args, lvm| {
            let (value,) = check_args!(lvm, args, Value);
            if let Some(v) = get_metamethod!(lvm, value, BOOL) {
                lvm.call(v, vec![value])
            } else {
                Ok(Value::Bool(value.into()))
            }
        }),
    );
    t.insert(
        "int".to_string(),
        Value::ExtFunction(|args, lvm| {
            let (value,) = check_args!(lvm, args, Value);
            if let Some(v) = get_metamethod!(lvm, value, INT) {
                lvm.call(v, vec![value])
            } else {
                Ok(match value.try_into() {
                    Ok(v) => Value::Int(v),
                    Err(v) => v.into_table_value(lvm),
                })
            }
        }),
    );
    t.insert(
        "float".to_string(),
        Value::ExtFunction(|args, lvm| {
            let (value,) = check_args!(lvm, args, Value);
            if let Some(v) = get_metamethod!(lvm, value, FLOAT) {
                lvm.call(v, vec![value])
            } else {
                Ok(match value.try_into() {
                    Ok(v) => Value::Float(v),
                    Err(v) => v.into_table_value(lvm),
                })
            }
        }),
    );
    t.insert(
        "str".to_string(),
        Value::ExtFunction(|args, lvm| {
            let (value,) = check_args!(lvm, args, Value);
            if let Some(v) = get_metamethod!(lvm, value, STR) {
                lvm.call(v, vec![value])
            } else {
                Ok(lvm.new_str_value(value.into()))
            }
        }),
    );
    t.insert(
        "repr".to_string(),
        Value::ExtFunction(|args, lvm| {
            let (value,) = check_args!(lvm, args, Value);
            if let Some(v) = get_metamethod!(lvm, value, REPR) {
                lvm.call(v, vec![value])
            } else {
                Ok(lvm.new_str_value(value.repr()))
            }
        }),
    );
    t
}
