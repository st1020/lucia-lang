pub mod builtin;
pub mod io;
pub mod table;

use std::collections::HashMap;

use crate::lvm::Lvm;
use crate::objects::Value;

#[macro_export]
macro_rules! check_arguments_num {
    ($lvm:expr, $args:expr, $value:expr, $require_ident:ident($require_value:expr)) => {
        let required = $crate::errors::CallArgumentsErrorKind::$require_ident($require_value);
        if !required.contains(&$args.len()) {
            $crate::return_error!(
                $crate::errors::BuiltinError::TypeError(
                    $crate::errors::TypeError::CallArgumentsError {
                        value: $value,
                        required,
                        given: $args.len(),
                    }
                ),
                $lvm
            );
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
    add_std_module!("std::io", io::libs);
    add_std_module!("std::table", table::libs);
    std_libs
}
