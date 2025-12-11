use dashmap::DashMap;
use serde_json::Value;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::completions::{get_completions, resolve_completion};
use crate::document::Document;
use crate::hover::get_hover;
use crate::semantic_tokens::{compute_semantic_tokens, semantic_token_legend};
use crate::signature_help::get_signature_help;

use nimbyscript_analyzer::ApiDefinitions;

pub struct Backend {
    client: Client,
    documents: DashMap<Url, Document>,
    api_definitions: ApiDefinitions,
}

// Embed the API definitions TOML at compile time
const API_DEFINITIONS_TOML: &str = include_str!("../../../api-definitions/nimbyrails.v1.toml");

impl Backend {
    pub fn new(client: Client) -> Self {
        // Load embedded API definitions
        let api_definitions =
            ApiDefinitions::load_from_str(API_DEFINITIONS_TOML).unwrap_or_else(|e| {
                eprintln!("Failed to load API definitions: {e}");
                ApiDefinitions::default()
            });

        Self {
            client,
            documents: DashMap::new(),
            api_definitions,
        }
    }

    async fn update_document(&self, uri: Url, content: String, version: i32) {
        let doc = Document::new(content, Some(&self.api_definitions));
        let diagnostics: Vec<_> = doc.diagnostics().to_vec();
        self.documents.insert(uri.clone(), doc);

        // Publish diagnostics
        let lsp_diagnostics: Vec<Diagnostic> = diagnostics
            .iter()
            .map(|d| {
                let range = self.offset_to_range(&uri, d.span.start, d.span.end);
                Diagnostic {
                    range,
                    severity: Some(match d.severity {
                        nimbyscript_analyzer::diagnostics::Severity::Error => {
                            DiagnosticSeverity::ERROR
                        }
                        nimbyscript_analyzer::diagnostics::Severity::Warning => {
                            DiagnosticSeverity::WARNING
                        }
                        nimbyscript_analyzer::diagnostics::Severity::Info => {
                            DiagnosticSeverity::INFORMATION
                        }
                        nimbyscript_analyzer::diagnostics::Severity::Hint => {
                            DiagnosticSeverity::HINT
                        }
                    }),
                    message: d.message.clone(),
                    code: d.code.clone().map(NumberOrString::String),
                    source: Some("nimbyscript".to_string()),
                    ..Default::default()
                }
            })
            .collect();

        self.client
            .publish_diagnostics(uri, lsp_diagnostics, Some(version))
            .await;
    }

    fn offset_to_range(&self, uri: &Url, start: usize, end: usize) -> Range {
        if let Some(doc) = self.documents.get(uri) {
            let start_pos = doc.offset_to_position(start);
            let end_pos = doc.offset_to_position(end);
            Range::new(start_pos, end_pos)
        } else {
            Range::new(Position::new(0, 0), Position::new(0, 0))
        }
    }

    fn resolve_completion_doc(&self, data: &Value) -> Option<Documentation> {
        resolve_completion(data, &self.api_definitions)
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".into(), ":".into()]),
                    resolve_provider: Some(true),
                    ..Default::default()
                }),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".into(), ",".into()]),
                    retrigger_characters: Some(vec![",".into()]),
                    ..Default::default()
                }),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: semantic_token_legend(),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            range: Some(false),
                            ..Default::default()
                        },
                    ),
                ),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "nimbyscript-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "NimbyScript LSP initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let content = params.text_document.text;
        let version = params.text_document.version;
        self.update_document(uri, content, version).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let version = params.text_document.version;

        // Full sync - use the last change
        if let Some(change) = params.content_changes.into_iter().last() {
            self.update_document(uri, change.text, version).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.documents.remove(&params.text_document.uri);
        // Clear diagnostics for closed document
        self.client
            .publish_diagnostics(params.text_document.uri, vec![], None)
            .await;
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        if let Some(doc) = self.documents.get(uri) {
            let items = get_completions(&doc, position, &self.api_definitions);
            Ok(Some(CompletionResponse::Array(items)))
        } else {
            Ok(None)
        }
    }

    async fn completion_resolve(&self, mut item: CompletionItem) -> Result<CompletionItem> {
        // If item already has documentation, return as-is
        if item.documentation.is_some() {
            return Ok(item);
        }

        // Try to resolve documentation from the data field
        if let Some(data) = &item.data {
            if let Some(resolved_doc) = self.resolve_completion_doc(data) {
                item.documentation = Some(resolved_doc);
            }
        }

        Ok(item)
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        if let Some(doc) = self.documents.get(uri) {
            Ok(get_signature_help(&doc, position, &self.api_definitions))
        } else {
            Ok(None)
        }
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;

        if let Some(doc) = self.documents.get(uri) {
            let tokens = compute_semantic_tokens(&doc, &self.api_definitions);
            Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: tokens,
            })))
        } else {
            Ok(None)
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        if let Some(doc) = self.documents.get(uri) {
            return Ok(get_hover(&doc, position, &self.api_definitions));
        }

        Ok(None)
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;

        if let Some(doc) = self.documents.get(uri) {
            let symbols = doc.document_symbols();
            let lsp_symbols: Vec<DocumentSymbol> = symbols
                .iter()
                .map(|s| {
                    let range = self.offset_to_range(uri, s.span.start, s.span.end);

                    #[allow(deprecated)]
                    DocumentSymbol {
                        name: s.name.clone(),
                        detail: s.type_name.clone(),
                        kind: s.kind,
                        range,
                        selection_range: range,
                        children: None,
                        tags: None,
                        deprecated: None,
                    }
                })
                .collect();

            Ok(Some(DocumentSymbolResponse::Nested(lsp_symbols)))
        } else {
            Ok(None)
        }
    }
}
