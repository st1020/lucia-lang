use std::io;

use crate::lvm::Lvm;
use crate::objects::{GCObjectKind, LuciaTable, LuciaValue};
use crate::{check_arguments_num, str_to_value};

pub fn libs(lvm: &mut Lvm) -> LuciaTable {
    let mut t = LuciaTable::new();
    t.set(
        &str_to_value!(lvm, "print"),
        LuciaValue::ExtFunction(|args, _| {
            check_arguments_num!(args, None, 1);
            print!("{}", args.first().unwrap());
            Ok(LuciaValue::Null)
        }),
    );
    t.set(
        &str_to_value!(lvm, "println"),
        LuciaValue::ExtFunction(|args, _| {
            check_arguments_num!(args, None, 1);
            println!("{}", args.first().unwrap());
            Ok(LuciaValue::Null)
        }),
    );
    t.set(
        &str_to_value!(lvm, "input"),
        LuciaValue::ExtFunction(|args, lvm| {
            check_arguments_num!(args, None, 0);
            let mut t = String::new();
            io::stdin().read_line(&mut t).unwrap();
            Ok(lvm.new_gc_value(GCObjectKind::Str(String::from(
                t.strip_suffix('\n').unwrap(),
            ))))
        }),
    );
    t
}
