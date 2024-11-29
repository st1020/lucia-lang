use std::fs;

use lucia_lang::Lucia;

use crate::{args::RunCommand, Context};

impl Context {
    pub async fn execute_run(&mut self, cmd: RunCommand) -> Result<(), anyhow::Error> {
        let input = fs::read_to_string(cmd.path)?;
        let mut lucia = Lucia::new();
        let code = match lucia.compile(&input) {
            Ok(code) => code,
            Err(errors) => {
                for err in errors {
                    eprintln!("{err}");
                }
                return Err(anyhow::Error::msg("compile error"));
            }
        };
        lucia.execute(&code)?;
        Ok(())
    }
}
