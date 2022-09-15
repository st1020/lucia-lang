use std::io;

use crate::lvm::Lvm;
use crate::object::{GCObjectKind, LucyTable, LucyValue};
use crate::str_to_value;

pub fn libs(lvm: &mut Lvm) -> LucyTable {
    let mut t = LucyTable::new();
    t.set(
        &str_to_value!(lvm, "print"),
        LucyValue::ExtFunction(|args, _| {
            if args.len() == 1 {
                panic!()
            }
            print!("{}", args.first().unwrap().to_string());
            Ok(LucyValue::Null)
        }),
    );
    t.set(
        &str_to_value!(lvm, "println"),
        LucyValue::ExtFunction(|args, _| {
            if args.len() != 1 {
                panic!()
            }
            println!("{}", args.first().unwrap().to_string());
            Ok(LucyValue::Null)
        }),
    );
    t.set(
        &str_to_value!(lvm, "input"),
        LucyValue::ExtFunction(|args, lvm| {
            if args.len() != 0 {
                panic!()
            }
            let mut t = String::new();
            io::stdin().read_line(&mut t).unwrap();
            Ok(lvm.new_gc_value(GCObjectKind::Str(String::from(
                t.strip_suffix("\n").unwrap(),
            ))))
        }),
    );
    t
}
