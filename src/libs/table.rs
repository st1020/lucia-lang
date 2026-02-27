use std::rc::Rc;

use crate::objects::{Callback, RcTable, Table, TableIterCallback, Value};

pub fn table_lib() -> Table {
    let mut t = Table::new();
    t.set(
        "keys",
        Callback::from_fn(|table: RcTable| Callback::new(TableIterCallback::keys(table))),
    );
    t.set(
        "values",
        Callback::from_fn(|table: RcTable| Callback::new(TableIterCallback::values(table))),
    );
    t.set("raw_len", Callback::from_fn(|table: RcTable| table.len()));
    t.set(
        "raw_get",
        Callback::from_fn(|table: RcTable, key: Value| table.get(key)),
    );
    t.set(
        "raw_set",
        Callback::from_fn(|table: RcTable, key: Value, value: Value| {
            let mut table = Rc::unwrap_or_clone(table);
            table.set(key, value);
            table
        }),
    );
    t.set(
        "raw_iter",
        Callback::from_fn(|table: RcTable| Callback::new(TableIterCallback::new(table))),
    );
    t
}
