use std::io;

use itertools::Itertools;

use crate::{
    errors::Error,
    objects::{Callback, Table, Value},
};

pub fn io_lib() -> Table {
    let mut t = Table::new();
    t.set(
        "print",
        #[expect(clippy::print_stdout)]
        Callback::from_fn(|args: &[Value]| match args {
            [] => (),
            [value] => print!("{value}"),
            _ => print!("{}", args.iter().join(" ")),
        }),
    );
    t.set(
        "println",
        #[expect(clippy::print_stdout)]
        Callback::from_fn(|args: &[Value]| match args {
            [] => println!(),
            [value] => println!("{value}"),
            _ => println!("{}", args.iter().join(" ")),
        }),
    );
    t.set(
        "input",
        Callback::from_fn(|| {
            let mut buf = String::new();
            io::stdin()
                .read_line(&mut buf)
                .map_err(|e| Error::IOError { reason: e.kind() })?;
            Ok(buf.strip_suffix('\n').unwrap_or(&buf).to_owned())
        }),
    );
    t
}
