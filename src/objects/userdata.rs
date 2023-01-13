use std::fmt::{Debug, Display};

use super::Table;

#[derive(Debug, Clone)]
pub struct UserData {
    pub ptr: *mut u8,
    pub table: Table,
}

impl PartialEq for UserData {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl Eq for UserData {}

impl Display for UserData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "userdata")
    }
}

impl UserData {
    pub fn new(ptr: *mut u8, table: Table) -> Self {
        UserData { ptr, table }
    }
}
