use std::collections::HashMap;

use crate::errors::{Error, RuntimeError, RuntimeErrorKind};
use crate::objects::{Value, ValueType};
use crate::{check_args, error, get_metamethod, return_error, unexpect_type_error};

pub fn builtin_variables() -> HashMap<String, Value> {
    let mut t = HashMap::new();
    t.insert(
        "id".to_string(),
        Value::ExtFunction(|args, lvm| {
            let (v,) = check_args!(lvm, args, Value);
            Ok(match v {
                Value::GCObject(v) => Value::Int((v as usize).try_into().unwrap()),
                _ => Value::Null,
            })
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
                if let Some(msg) = msg {
                    Ok(error!(msg))
                } else {
                    Ok(error!(lvm.new_str_value("assertion_error".to_string())))
                }
            } else {
                Ok(v)
            }
        }),
    );
    t.insert(
        "len".to_string(),
        Value::ExtFunction(|args, lvm| {
            let (value,) = check_args!(lvm, args, Value);
            if let Some(v) = get_metamethod!(lvm, value, "__len__") {
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
            if let Some(v) = get_metamethod!(lvm, value, "__bool__") {
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
            if let Some(v) = get_metamethod!(lvm, value, "__int__") {
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
            if let Some(v) = get_metamethod!(lvm, value, "__float__") {
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
            if let Some(v) = get_metamethod!(lvm, value, "__str__") {
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
            if let Some(v) = get_metamethod!(lvm, value, "__repr__") {
                lvm.call(v, vec![value])
            } else {
                Ok(lvm.new_str_value(value.repr()))
            }
        }),
    );
    t
}
