use std::io;

use crate::errors::{LucyError, TypeErrorKind};
use crate::lvm::Lvm;
use crate::object::{GCObjectKind, LucyTable, LucyValue};
use crate::{call_arguments_error, str_to_value};

pub fn libs(lvm: &mut Lvm) -> LucyTable {
    let mut t = LucyTable::new();
    t.set(
        &str_to_value!(lvm, "print"),
        LucyValue::ExtFunction(|args, _| {
            if args.len() == 1 {
                return Err(call_arguments_error!(None, 1, args.len()));
            }
            print!("{}", args.first().unwrap().to_string());
            Ok(LucyValue::Null)
        }),
    );
    t.set(
        &str_to_value!(lvm, "println"),
        LucyValue::ExtFunction(|args, _| {
            if args.len() != 1 {
                return Err(call_arguments_error!(None, 1, args.len()));
            }
            println!("{}", args.first().unwrap().to_string());
            Ok(LucyValue::Null)
        }),
    );
    t.set(
        &str_to_value!(lvm, "input"),
        LucyValue::ExtFunction(|args, lvm| {
            if args.len() != 0 {
                return Err(call_arguments_error!(None, 1, args.len()));
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
