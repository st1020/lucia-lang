pub mod builtin;
pub mod convert;
pub mod io;
pub mod table;

use std::collections::HashMap;

use crate::lvm::Lvm;
use crate::objects::Value;

#[macro_export]
macro_rules! check_arguments_num {
    ($lvm:expr, $args:expr, $value:expr, $require:expr) => {
        if $args.len() != $require {
            return Ok($crate::builtin_error_to_table!(
                $lvm,
                $crate::call_arguments_error!($value, $require, $args.len())
            ));
        }
    };
}

pub fn std_libs(lvm: &mut Lvm) -> HashMap<String, Value> {
    let mut std_libs = HashMap::new();
    macro_rules! add_std_module {
        ($name:expr, $path:path) => {
            let t = $path(lvm);
            std_libs.insert(String::from($name), lvm.new_table_value(t));
        };
    }
    add_std_module!("std::convert", convert::libs);
    add_std_module!("std::io", io::libs);
    add_std_module!("std::table", table::libs);
    std_libs
}
