use std::collections::HashMap;

use crate::check_arguments_num;
use crate::objects::LuciaValue;

pub fn builtin_variables() -> HashMap<String, LuciaValue> {
    let mut t = HashMap::new();
    t.insert(
        String::from("id"),
        LuciaValue::ExtFunction(|args, _| {
            check_arguments_num!(args, None, 1);
            Ok(match args.first().unwrap() {
                LuciaValue::GCObject(v) => LuciaValue::Int((*v as usize).try_into().unwrap()),
                _ => LuciaValue::Null,
            })
        }),
    );
    t.insert(
        String::from("type"),
        LuciaValue::ExtFunction(|args, lvm| {
            check_arguments_num!(args, None, 1);
            Ok(lvm.new_str_value(args.first().unwrap().value_type().to_string()))
        }),
    );
    t
}
