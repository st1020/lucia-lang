use std::collections::HashMap;

use crate::errors::{Error, RuntimeError, RuntimeErrorKind};
use crate::objects::{Value, ValueType};
use crate::{check_arguments_num, error, get_metamethod, return_error, type_convert_error};

pub fn builtin_variables() -> HashMap<String, Value> {
    let mut t = HashMap::new();
    t.insert(
        "id".to_string(),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, Eq(1));
            Ok(match args.first().unwrap() {
                Value::GCObject(v) => Value::Int((*v as usize).try_into().unwrap()),
                _ => Value::Null,
            })
        }),
    );
    t.insert(
        "type".to_string(),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, Eq(1));
            Ok(lvm.new_str_value(args.first().unwrap().value_type().to_string()))
        }),
    );
    t.insert(
        "is_error".to_string(),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, Eq(1));
            Ok(Value::Bool(args.first().unwrap().is_error()))
        }),
    );
    t.insert(
        "panic".to_string(),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, Eq(1));
            Err(Error::RuntimeError(RuntimeError {
                kind: RuntimeErrorKind::UserPanic(*args.first().unwrap()),
                traceback: lvm.traceback(),
            }))
        }),
    );
    t.insert(
        "assert".to_string(),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, RangeInclusive(1..=2));
            let value = args[0];
            if value.is_error() || !(bool::from(value)) {
                if args.len() == 2 {
                    Ok(error!(args[1]))
                } else {
                    Ok(error!(lvm.new_str_value("assertion_error".to_string())))
                }
            } else {
                Ok(value)
            }
        }),
    );
    t.insert(
        "len".to_string(),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, Eq(1));
            if let Some(v) = get_metamethod!(lvm, args[0], "__len__") {
                lvm.call(v, vec![args[0]])
            } else if let Some(v) = args[0].as_str() {
                Ok(Value::Int(v.len().try_into().unwrap()))
            } else if let Some(v) = args[0].as_table() {
                Ok(Value::Int(v.len().try_into().unwrap()))
            } else {
                return_error!(
                    type_convert_error!(args[0].value_type(), ValueType::Table),
                    lvm
                );
            }
        }),
    );
    t.insert(
        "bool".to_string(),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, Eq(1));
            if let Some(v) = get_metamethod!(lvm, args[0], "__bool__") {
                lvm.call(v, vec![args[0]])
            } else {
                Ok(Value::Bool(args[0].into()))
            }
        }),
    );
    t.insert(
        "int".to_string(),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, Eq(1));
            if let Some(v) = get_metamethod!(lvm, args[0], "__int__") {
                lvm.call(v, vec![args[0]])
            } else {
                Ok(match args[0].try_into() {
                    Ok(v) => Value::Int(v),
                    Err(v) => v.into_table_value(lvm),
                })
            }
        }),
    );
    t.insert(
        "float".to_string(),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, Eq(1));
            if let Some(v) = get_metamethod!(lvm, args[0], "__float__") {
                lvm.call(v, vec![args[0]])
            } else {
                Ok(match args[0].try_into() {
                    Ok(v) => Value::Float(v),
                    Err(v) => v.into_table_value(lvm),
                })
            }
        }),
    );
    t.insert(
        "str".to_string(),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, Eq(1));
            if let Some(v) = get_metamethod!(lvm, args[0], "__str__") {
                lvm.call(v, vec![args[0]])
            } else {
                Ok(lvm.new_str_value(args[0].into()))
            }
        }),
    );
    t.insert(
        "repr".to_string(),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, Eq(1));
            if let Some(v) = get_metamethod!(lvm, args[0], "__repr__") {
                lvm.call(v, vec![args[0]])
            } else {
                Ok(lvm.new_str_value(args[0].repr()))
            }
        }),
    );
    t
}
