use std::collections::HashMap;

use crate::check_arguments_num;
use crate::errors::LuciaError;
use crate::objects::LuciaValue;

pub fn builtin_variables() -> HashMap<String, LuciaValue> {
    let mut t = HashMap::new();
    t.insert(
        "id".to_string(),
        LuciaValue::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, 1);
            Ok(match args.first().unwrap() {
                LuciaValue::GCObject(v) => LuciaValue::Int((*v as usize).try_into().unwrap()),
                _ => LuciaValue::Null,
            })
        }),
    );
    t.insert(
        "type".to_string(),
        LuciaValue::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, 1);
            Ok(lvm.new_str_value(args.first().unwrap().value_type().to_string()))
        }),
    );
    t.insert(
        "is_error".to_string(),
        LuciaValue::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, 1);
            Ok(LuciaValue::Bool(args.first().unwrap().is_error()))
        }),
    );
    t.insert(
        "panic".to_string(),
        LuciaValue::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, 1);
            Err(LuciaError::UserPanic(*args.first().unwrap()))
        }),
    );
    t
}
