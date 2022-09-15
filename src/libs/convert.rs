use crate::lvm::Lvm;
use crate::object::{GCObjectKind, LucyTable, LucyValue};
use crate::str_to_value;

pub fn libs(lvm: &mut Lvm) -> LucyTable {
    let mut t = LucyTable::new();
    t.set(
        &str_to_value!(lvm, "bool"),
        LucyValue::ExtFunction(|args, _| {
            if args.len() != 1 {
                panic!()
            }
            Ok(LucyValue::Bool(match args.first().unwrap() {
                LucyValue::Null => false,
                LucyValue::Bool(v) => *v,
                LucyValue::Int(v) => {
                    if *v == 0 {
                        false
                    } else {
                        true
                    }
                }
                LucyValue::Float(v) => {
                    if *v == 0.0 {
                        false
                    } else {
                        true
                    }
                }
                LucyValue::ExtFunction(_) => true,
                LucyValue::GCObject(_) => true,
            }))
        }),
    );
    t.set(
        &str_to_value!(lvm, "int"),
        LucyValue::ExtFunction(|args, _| {
            if args.len() != 1 {
                panic!()
            }
            Ok(LucyValue::Int(match args.first().unwrap() {
                LucyValue::Null => 0,
                LucyValue::Bool(v) => i64::from(*v),
                LucyValue::Int(v) => *v,
                LucyValue::Float(v) => *v as i64,
                LucyValue::ExtFunction(_) => panic!(),
                LucyValue::GCObject(v) => unsafe {
                    match &(**v).kind {
                        GCObjectKind::Str(v) => v.parse().unwrap(),
                        _ => panic!(),
                    }
                },
            }))
        }),
    );
    t.set(
        &str_to_value!(lvm, "float"),
        LucyValue::ExtFunction(|args, _| {
            if args.len() != 1 {
                panic!()
            }
            Ok(LucyValue::Float(match args.first().unwrap() {
                LucyValue::Null => 0.0,
                LucyValue::Bool(v) => f64::from(u8::from(*v)),
                LucyValue::Int(v) => *v as f64,
                LucyValue::Float(v) => *v,
                LucyValue::ExtFunction(_) => panic!(),
                LucyValue::GCObject(v) => unsafe {
                    match &(**v).kind {
                        GCObjectKind::Str(v) => v.parse().unwrap(),
                        _ => panic!(),
                    }
                },
            }))
        }),
    );
    t.set(
        &str_to_value!(lvm, "str"),
        LucyValue::ExtFunction(|args, lvm| {
            if args.len() != 1 {
                panic!()
            }
            Ok(lvm.new_gc_value(GCObjectKind::Str(args.first().unwrap().to_string())))
        }),
    );
    t
}
