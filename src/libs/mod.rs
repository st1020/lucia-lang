pub mod builtin;
pub mod convert;
pub mod io;

use std::collections::HashMap;

use crate::lvm::Lvm;
use crate::object::{GCObject, GCObjectKind, LucyValue};

pub fn std_libs(lvm: &mut Lvm) -> HashMap<String, LucyValue> {
    let mut std_libs = HashMap::new();
    macro_rules! add_std_module {
        ($name:expr, $path:path) => {
            let t = $path(lvm);
            std_libs.insert(
                String::from($name),
                LucyValue::GCObject(lvm.new_gc_object(GCObject::new(GCObjectKind::Table(t)))),
            );
        };
    }
    add_std_module!("convert", convert::libs);
    add_std_module!("io", io::libs);
    std_libs
}
