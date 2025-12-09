use std::{collections::HashMap, sync::Arc};

use dashmap::DashMap;
use lucia_lang::utils::Locatable;

use tower_lsp_server::{
    Client, ClientSocket, LanguageServer, LspService, jsonrpc::Result, ls_types::*,
};

use crate::{
    document::Document,
    interning::ThreadSafeInterner,
    semantic_token::{IntoSemanticTokenType, TOKEN_TYPES},
    utils::OffsetPosition,
};

pub mod document;
mod interning;
pub mod semantic_token;
pub mod utils;

pub fn lsp_service() -> (LspService<Backend>, ClientSocket) {
    LspService::build(Backend::new).finish()
}

#[derive(Debug)]
pub struct Backend {
    client: Client,
    document_map: DashMap<String, Document<Arc<str>>>,
}

struct TextDocumentItem<'a> {
    uri: Uri,
    text: &'a str,
    version: Option<i32>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            document_map: DashMap::new(),
        }
    }

    async fn on_change(&self, params: TextDocumentItem<'_>) {
        let interner = ThreadSafeInterner::default();
        let (document, errors) = Document::build(params.text, interner);

        let diagnostics = errors
            .iter()
            .filter_map(|err| {
                let range = document.rope.text_range_to_range(err.range())?;
                Some(Diagnostic::new_simple(range, err.to_string()))
            })
            .collect();

        self.document_map.insert(params.uri.to_string(), document);

        self.client
            .publish_diagnostics(params.uri.clone(), diagnostics, params.version)
            .await;
    }
}

impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                position_encoding: Some(PositionEncodingKind::UTF16),
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        will_save: None,
                        will_save_wait_until: None,
                        save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                            include_text: Some(true),
                        })),
                    },
                )),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: {
                                TextDocumentRegistrationOptions {
                                    document_selector: Some(vec![DocumentFilter {
                                        language: Some("lucia".to_string()),
                                        scheme: Some("file".to_string()),
                                        pattern: None,
                                    }]),
                                }
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                work_done_progress_options: Default::default(),
                                legend: SemanticTokensLegend {
                                    token_types: TOKEN_TYPES.to_vec(),
                                    token_modifiers: vec![],
                                },
                                range: None,
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                            static_registration_options: Default::default(),
                        },
                    ),
                ),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "lucia-lsp".to_string(),
                version: option_env!("CARGO_PKG_VERSION").map(String::from),
            }),
            offset_encoding: None,
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        log::info!("Lucia-lsp initialized!");
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: &params.text_document.text,
            version: Some(params.text_document.version),
        })
        .await
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.on_change(TextDocumentItem {
            text: &params.content_changes[0].text,
            uri: params.text_document.uri,
            version: Some(params.text_document.version),
        })
        .await
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        if let Some(text) = params.text {
            let item = TextDocumentItem {
                uri: params.text_document.uri,
                text: &text,
                version: None,
            };
            self.on_change(item).await;
            _ = self.client.semantic_tokens_refresh().await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.document_map.remove(params.text_document.uri.as_str());
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let definition = || -> Option<GotoDefinitionResponse> {
            let uri = params.text_document_position_params.text_document.uri;
            let document = self.document_map.get(uri.as_str())?;

            let position = params.text_document_position_params.position;
            let offset = document.rope.position_to_offset(position)?;

            let symbol = document.get_symbol_from_reference_offset(offset)?;
            let range = document.rope.text_range_to_range(symbol.range)?;
            Some(GotoDefinitionResponse::Scalar(Location::new(uri, range)))
        }();
        Ok(definition)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let references = || -> Option<Vec<Location>> {
            let uri = params.text_document_position.text_document.uri;
            let document = self.document_map.get(uri.as_str())?;

            let position = params.text_document_position.position;
            let offset = document.rope.position_to_offset(position)?;

            let references = document.get_references(offset)?;
            Some(
                references
                    .filter_map(|reference| {
                        let range = document.rope.text_range_to_range(reference.range)?;
                        Some(Location::new(uri.clone(), range))
                    })
                    .collect(),
            )
        }();
        Ok(references)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;
        let Some(document) = self.document_map.get(uri.as_str()) else {
            return Ok(None);
        };

        let mut pre_line = 0;
        let mut pre_start = 0;
        let semantic_tokens = document
            .tokens
            .iter()
            .filter_map(|token| {
                let token_start: usize = token.range().start().into();
                let line = document.rope.try_byte_to_line(token_start).ok()? as u32;
                let first = document.rope.try_line_to_char(line as usize).ok()? as u32;
                let start = document.rope.try_byte_to_char(token_start).ok()? as u32 - first;
                let delta_line = line - pre_line;
                let delta_start = if delta_line == 0 {
                    start - pre_start
                } else {
                    start
                };
                let token_kind = token.kind.into_semantic_token_type()?;
                let ret = Some(SemanticToken {
                    delta_line,
                    delta_start,
                    length: token.range().len().into(),
                    token_type: TOKEN_TYPES
                        .iter()
                        .position(|item| item == &token_kind)
                        .unwrap() as u32,
                    token_modifiers_bitset: 0,
                });
                pre_line = line;
                pre_start = start;
                ret
            })
            .collect::<Vec<_>>();

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: semantic_tokens,
        })))
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let workspace_edit = || -> Option<WorkspaceEdit> {
            let uri = params.text_document_position.text_document.uri;
            let document = self.document_map.get(uri.as_str())?;

            let position = params.text_document_position.position;
            let offset = document.rope.position_to_offset(position)?;

            let references = document.get_references(offset)?;
            let text_edits = references
                .filter_map(|reference| {
                    let range = document.rope.text_range_to_range(reference.range)?;
                    Some(TextEdit::new(range, params.new_name.clone()))
                })
                .collect();

            #[allow(clippy::mutable_key_type)]
            let mut changes = HashMap::new();
            changes.insert(uri, text_edits);
            Some(WorkspaceEdit::new(changes))
        }();
        Ok(workspace_edit)
    }
}
