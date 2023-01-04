use std::io;

use crate::lvm::Lvm;
use crate::objects::{GCObjectKind, LuciaTable, LuciaValue};
use crate::{call_arguments_error, str_to_value};

pub fn libs(lvm: &mut Lvm) -> LuciaTable {
    let mut t = LuciaTable::new();
    t.set(
        &str_to_value!(lvm, "print"),
        LuciaValue::ExtFunction(|args, _| {
            if args.len() == 1 {
                return Err(call_arguments_error!(None, 1, args.len()));
            }
            print!("{}", args.first().unwrap());
            Ok(LuciaValue::Null)
        }),
    );
    t.set(
        &str_to_value!(lvm, "println"),
        LuciaValue::ExtFunction(|args, _| {
            if args.len() != 1 {
                return Err(call_arguments_error!(None, 1, args.len()));
            }
            println!("{}", args.first().unwrap());
            Ok(LuciaValue::Null)
        }),
    );
    t.set(
        &str_to_value!(lvm, "input"),
        LuciaValue::ExtFunction(|args, lvm| {
            if !args.is_empty() {
                return Err(call_arguments_error!(None, 1, args.len()));
            }
            let mut t = String::new();
            io::stdin().read_line(&mut t).unwrap();
            Ok(lvm.new_gc_value(GCObjectKind::Str(String::from(
                t.strip_suffix('\n').unwrap(),
            ))))
        }),
    );
    t
}
