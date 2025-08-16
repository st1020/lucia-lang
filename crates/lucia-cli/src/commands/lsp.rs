use lucia_lsp::lsp_service;
use tower_lsp_server::Server;

use crate::Context;

impl Context {
    pub async fn execute_lsp(&mut self) -> Result<(), anyhow::Error> {
        let stdin = tokio::io::stdin();
        let stdout = tokio::io::stdout();
        let (service, socket) = lsp_service();
        Server::new(stdin, stdout, socket).serve(service).await;
        Ok(())
    }
}
