use std::io;

use crate::lvm::Lvm;
use crate::object::{GCObject, GCObjectKind, LucyTable, LucyValue};

pub fn libs(lvm: &mut Lvm) -> LucyTable {
    let mut t = LucyTable::new();
    t.set(
        &LucyValue::GCObject(
            lvm.new_gc_object(GCObject::new(GCObjectKind::Str(String::from("print")))),
        ),
        LucyValue::ExtFunction(|args, _| {
            if args.len() == 1 {
                panic!()
            }
            print!("{}", args.first().unwrap().to_string());
            LucyValue::Null
        }),
    );
    t.set(
        &LucyValue::GCObject(
            lvm.new_gc_object(GCObject::new(GCObjectKind::Str(String::from("println")))),
        ),
        LucyValue::ExtFunction(|args, _| {
            if args.len() != 1 {
                panic!()
            }
            println!("{}", args.first().unwrap().to_string());
            LucyValue::Null
        }),
    );
    t.set(
        &LucyValue::GCObject(
            lvm.new_gc_object(GCObject::new(GCObjectKind::Str(String::from("input")))),
        ),
        LucyValue::ExtFunction(|args, lvm| {
            if args.len() != 0 {
                panic!()
            }
            let mut t = String::new();
            io::stdin().read_line(&mut t).unwrap();
            LucyValue::GCObject(
                lvm.new_gc_object(GCObject::new(GCObjectKind::Str(String::from(
                    t.strip_suffix("\n").unwrap(),
                )))),
            )
        }),
    );
    t
}
