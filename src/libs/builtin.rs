use std::collections::HashMap;

use crate::call_arguments_error;
use crate::object::{GCObjectKind, LuciaValue};

pub fn builtin_variables() -> HashMap<String, LuciaValue> {
    let mut t = HashMap::new();
    t.insert(
        String::from("id"),
        LuciaValue::ExtFunction(|args, _| {
            if args.len() != 1 {
                return Err(call_arguments_error!(None, 1, args.len()));
            }
            Ok(match args.first().unwrap() {
                LuciaValue::GCObject(v) => LuciaValue::Int((*v as usize).try_into().unwrap()),
                _ => LuciaValue::Null,
            })
        }),
    );
    t.insert(
        String::from("type"),
        LuciaValue::ExtFunction(|args, lvm| {
            if args.len() != 1 {
                return Err(call_arguments_error!(None, 1, args.len()));
            }
            Ok(lvm.new_gc_value(GCObjectKind::Str(
                args.first().unwrap().value_type().to_string(),
            )))
        }),
    );
    t
}
