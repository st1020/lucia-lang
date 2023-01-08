use std::io;

use crate::check_arguments_num;
use crate::lvm::Lvm;
use crate::objects::{LuciaTable, LuciaValue};

pub fn libs(lvm: &mut Lvm) -> LuciaTable {
    let mut t = LuciaTable::new();
    t.set(
        &lvm.new_str_value("print".to_string()),
        LuciaValue::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, 1);
            print!("{}", args.first().unwrap());
            Ok(LuciaValue::Null)
        }),
    );
    t.set(
        &lvm.new_str_value("println".to_string()),
        LuciaValue::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, 1);
            println!("{}", args.first().unwrap());
            Ok(LuciaValue::Null)
        }),
    );
    t.set(
        &lvm.new_str_value("input".to_string()),
        LuciaValue::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, 0);
            let mut t = String::new();
            io::stdin().read_line(&mut t).unwrap();
            Ok(lvm.new_str_value(t.strip_suffix('\n').unwrap_or(&t).to_string()))
        }),
    );
    t
}
