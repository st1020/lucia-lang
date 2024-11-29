use std::process::exit;

use clap::Parser;

use crate::args::LuciaCliArgs;

pub mod args;
pub mod commands;

#[derive(Debug, Clone)]
pub struct Context;

#[tokio::main]
async fn main() {
    env_logger::init();
    let mut context = Context;
    let args = LuciaCliArgs::parse();
    match context.execute(args).await {
        Ok(()) => exit(0),
        Err(err) => {
            eprintln!("{err:#}");
            exit(1);
        }
    }
}
