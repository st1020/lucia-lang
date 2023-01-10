use std::collections::HashMap;

use crate::check_arguments_num;
use crate::errors::Error;
use crate::objects::Value;

pub fn builtin_variables() -> HashMap<String, Value> {
    let mut t = HashMap::new();
    t.insert(
        "id".to_string(),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, 1);
            Ok(match args.first().unwrap() {
                Value::GCObject(v) => Value::Int((*v as usize).try_into().unwrap()),
                _ => Value::Null,
            })
        }),
    );
    t.insert(
        "type".to_string(),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, 1);
            Ok(lvm.new_str_value(args.first().unwrap().value_type().to_string()))
        }),
    );
    t.insert(
        "is_error".to_string(),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, 1);
            Ok(Value::Bool(args.first().unwrap().is_error()))
        }),
    );
    t.insert(
        "panic".to_string(),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, 1);
            Err(Error::UserPanic(*args.first().unwrap()))
        }),
    );
    t
}
