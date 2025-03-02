use crate::{
    Context,
    args::{LuciaCliArgs, LuciaCliCommand},
};

mod lsp;
mod run;

impl Context {
    pub async fn execute(&mut self, args: LuciaCliArgs) -> Result<(), anyhow::Error> {
        match args.cmd {
            LuciaCliCommand::Run(cmd) => self.execute_run(cmd).await,
            LuciaCliCommand::Lsp => self.execute_lsp().await,
        }
    }
}
