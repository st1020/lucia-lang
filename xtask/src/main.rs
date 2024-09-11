use std::env;

pub mod codegen;

fn main() {
    codegen::grammar::generate(false);
    match env::args().nth(1).as_deref() {
        Some("codegen") => codegen::grammar::generate(false),
        _ => panic!("Invalid command"),
    }
}
