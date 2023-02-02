use std::fmt::{Debug, Display};

use super::Table;

#[derive(Clone)]
pub struct UserData {
    pub ptr: *mut u8,
    pub metatable: Table,
    pub drop_func: fn(&mut UserData),
}

impl Debug for UserData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("UserData")
            .field("ptr", &self.ptr)
            .field("metatable", &self.metatable)
            .finish()
    }
}

impl PartialEq for UserData {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl Eq for UserData {}

impl Display for UserData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<userdata>")
    }
}

impl UserData {
    pub fn new(ptr: *mut u8, table: Table, drop_func: fn(&mut UserData)) -> Self {
        UserData {
            ptr,
            metatable: table,
            drop_func,
        }
    }
}
