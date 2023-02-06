use std::io;

use crate::check_arguments_num;
use crate::lvm::Lvm;
use crate::objects::{Table, Value};
use crate::utils::Join;

pub fn libs(lvm: &mut Lvm) -> Table {
    let mut t = Table::new();
    t.set(
        &lvm.new_str_value("print".to_string()),
        Value::ExtFunction(|args, _| {
            match args.len() {
                0 => (),
                1 => print!("{}", args.first().unwrap()),
                _ => print!("{}", args.iter().join(" ")),
            }
            Ok(Value::Null)
        }),
    );
    t.set(
        &lvm.new_str_value("println".to_string()),
        Value::ExtFunction(|args, _| {
            match args.len() {
                0 => println!(),
                1 => println!("{}", args.first().unwrap()),
                _ => println!("{}", args.iter().join(" ")),
            }
            Ok(Value::Null)
        }),
    );
    t.set(
        &lvm.new_str_value("input".to_string()),
        Value::ExtFunction(|args, lvm| {
            check_arguments_num!(lvm, args, None, Eq(0));
            let mut t = String::new();
            io::stdin().read_line(&mut t).unwrap();
            Ok(lvm.new_str_value(t.strip_suffix('\n').unwrap_or(&t).to_string()))
        }),
    );
    t
}
