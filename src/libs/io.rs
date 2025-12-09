use std::io;

use itertools::Itertools;

use crate::objects::{CallbackInner, Table, TableInner, Value};

pub fn io_lib() -> Table {
    let mut t = TableInner::new();
    t.set(
        "print",
        #[expect(clippy::print_stdout)]
        CallbackInner::from(|args: &[Value]| match args.len() {
            0 => (),
            1 => print!("{}", args.first().unwrap()),
            _ => print!("{}", args.iter().join(" ")),
        }),
    );
    t.set(
        "println",
        #[expect(clippy::print_stdout)]
        CallbackInner::from(|args: &[Value]| match args.len() {
            0 => println!(),
            1 => println!("{}", args.first().unwrap()),
            _ => println!("{}", args.iter().join(" ")),
        }),
    );
    t.set(
        "input",
        CallbackInner::from(|| {
            let mut buf = String::new();
            io::stdin().read_line(&mut buf).unwrap();
            buf.strip_suffix('\n').unwrap_or(&buf).to_owned()
        }),
    );
    t.into()
}
