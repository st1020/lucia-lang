use std::collections::HashMap;

use crate::object::{GCObject, GCObjectKind, LucyValue};

pub fn builtin_variables() -> HashMap<String, LucyValue> {
    let mut t = HashMap::new();
    t.insert(
        String::from("id"),
        LucyValue::ExtFunction(|args, _| {
            if args.len() != 1 {
                panic!()
            }
            match args.first().unwrap() {
                LucyValue::GCObject(v) => LucyValue::Int((*v as usize).try_into().unwrap()),
                _ => LucyValue::Null,
            }
        }),
    );
    t.insert(
        String::from("type"),
        LucyValue::ExtFunction(|args, lvm| {
            if args.len() != 1 {
                panic!()
            }
            LucyValue::GCObject(
                lvm.new_gc_object(GCObject::new(GCObjectKind::Str(String::from(
                    match args.first().unwrap() {
                        LucyValue::Null => "null",
                        LucyValue::Bool(_) => "bool",
                        LucyValue::Int(_) => "int",
                        LucyValue::Float(_) => "float",
                        LucyValue::ExtFunction(_) => "function",
                        LucyValue::GCObject(v) => unsafe {
                            match &(**v).kind {
                                GCObjectKind::Str(_) => "str",
                                GCObjectKind::Table(_) => "table",
                                GCObjectKind::Closuer(_) => "function",
                            }
                        },
                    },
                )))),
            )
        }),
    );
    t
}
