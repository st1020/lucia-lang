use std::collections::HashMap;

use crate::errors::{Error, RuntimeError, RuntimeErrorKind};
use crate::objects::{Value, ValueType};
use crate::{as_table, check_arguments_num, return_error, type_convert_error};

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
        "len".to_string(),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, Eq(1));
            if let Some(v) = args[0].as_str() {
                Ok(Value::Int(v.len().try_into().unwrap()))
            } else if let Some(v) = as_table!(args[0]) {
                if let Some(t) = v.get(&lvm.get_builtin_str("__len__")) {
                    lvm.call(t, vec![args[0]])
                } else {
                    Ok(Value::Int(v.len().try_into().unwrap()))
                }
            } else {
                return_error!(
                    type_convert_error!(args[0].value_type(), ValueType::Table),
                    lvm
                );
            }
        }),
    );
    t
}
