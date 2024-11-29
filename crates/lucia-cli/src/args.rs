use clap::{Args, Parser, Subcommand};

#[derive(Debug, Clone, Parser)]
#[command(name = "lucia")]
#[command(bin_name = "lucia")]
#[command(version, about, long_about = None)]
pub struct LuciaCliArgs {
    #[command(subcommand)]
    pub cmd: LuciaCliCommand,
}

#[derive(Debug, Clone, Subcommand)]
pub enum LuciaCliCommand {
    /// Compile and execute a Lucia script.
    Run(RunCommand),
    /// Start Language server.
    Lsp,
}

#[derive(Debug, Clone, Args)]
pub struct RunCommand {
    /// The path of the Lucia script to run.
    pub path: String,
}
