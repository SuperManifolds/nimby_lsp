use std::sync::RwLock;

use dashmap::DashMap;
use serde::Deserialize;
use serde_json::Value;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::completions::{get_completions, resolve_completion};
use crate::document::Document;
use crate::hover::get_hover;
use crate::inlay_hints::get_inlay_hints;
use crate::semantic_tokens::{compute_semantic_tokens, semantic_token_legend};
use crate::signature_help::get_signature_help;
use crate::type_hierarchy::{get_subtypes, get_supertypes, prepare_type_hierarchy};

use nimbyscript_analyzer::ApiDefinitions;

/// LSP server settings that can be configured by the client.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Settings {
    #[serde(default = "default_true")]
    pub inlay_hints_enabled: bool,
    #[serde(default = "default_true")]
    pub semantic_tokens_enabled: bool,
}

fn default_true() -> bool {
    true
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            inlay_hints_enabled: true,
            semantic_tokens_enabled: true,
        }
    }
}

pub struct Backend {
    client: Client,
    documents: DashMap<Url, Document>,
    api_definitions: ApiDefinitions,
    settings: RwLock<Settings>,
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
            settings: RwLock::new(Settings::default()),
        }
    }

    /// Update settings from initialization options or configuration change.
    fn update_settings(&self, value: &Value) {
        // Check if this is the nested VS Code format: { "inlayHints": { "enabled": true }, ... }
        // We check for nested format FIRST because flat format deserialization with defaults
        // would succeed even when the data is in nested format (ignoring unknown fields).
        if value.get("inlayHints").is_some() || value.get("semanticTokens").is_some() {
            self.update_settings_nested(value);
            return;
        }

        // Try direct/flat deserialization (for init options from extension)
        if let Ok(settings) = serde_json::from_value::<Settings>(value.clone()) {
            if let Ok(mut current) = self.settings.write() {
                *current = settings;
            }
        }
    }

    /// Update settings from nested VS Code format.
    fn update_settings_nested(&self, value: &Value) {
        let Ok(mut current) = self.settings.write() else {
            return;
        };

        if let Some(enabled) = value
            .get("inlayHints")
            .and_then(|v| v.get("enabled"))
            .and_then(Value::as_bool)
        {
            current.inlay_hints_enabled = enabled;
        }

        if let Some(enabled) = value
            .get("semanticTokens")
            .and_then(|v| v.get("enabled"))
            .and_then(Value::as_bool)
        {
            current.semantic_tokens_enabled = enabled;
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

    /// Pull configuration from the client using workspace/configuration request.
    async fn pull_configuration(&self) {
        let items = vec![ConfigurationItem {
            scope_uri: None,
            section: Some("nimbyscript".to_string()),
        }];

        match self.client.configuration(items).await {
            Ok(configs) => {
                if let Some(config) = configs.into_iter().next() {
                    self.update_settings(&config);
                }
            }
            Err(e) => {
                tracing::warn!("Failed to pull configuration: {e}");
            }
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        // Parse initialization options for settings
        if let Some(init_options) = params.initialization_options {
            self.update_settings(&init_options);
        }

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
                inlay_hint_provider: Some(OneOf::Left(true)),
                // Note: type_hierarchy_provider is not yet in lsp-types 0.94.1
                // The handlers are implemented and will respond if clients send requests
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

        // Dynamically register type hierarchy capability
        // (not available in ServerCapabilities in lsp-types 0.94.1)
        let registrations = vec![Registration {
            id: "type-hierarchy".to_string(),
            method: "textDocument/prepareTypeHierarchy".to_string(),
            register_options: Some(serde_json::json!({
                "documentSelector": [{ "language": "nimbyscript" }]
            })),
        }];

        if let Err(e) = self.client.register_capability(registrations).await {
            self.client
                .log_message(
                    MessageType::WARNING,
                    format!("Failed to register type hierarchy capability: {e}"),
                )
                .await;
        }
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

    async fn did_change_configuration(&self, params: DidChangeConfigurationParams) {
        // VS Code sends settings under "nimbyscript" key
        if let Some(nimbyscript) = params.settings.get("nimbyscript") {
            self.update_settings(nimbyscript);
            return;
        }

        // VS Code with vscode-languageclient may send an empty object and expect us to pull
        // configuration using workspace/configuration request
        if params.settings.is_null()
            || (params.settings.is_object()
                && params
                    .settings
                    .as_object()
                    .is_some_and(serde_json::Map::is_empty))
        {
            self.pull_configuration().await;
            return;
        }

        // Some clients send the settings directly
        self.update_settings(&params.settings);
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
        // Check if semantic tokens are enabled
        if let Ok(settings) = self.settings.read() {
            if !settings.semantic_tokens_enabled {
                return Ok(None);
            }
        }

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

    async fn prepare_type_hierarchy(
        &self,
        params: TypeHierarchyPrepareParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        if let Some(doc) = self.documents.get(uri) {
            Ok(prepare_type_hierarchy(
                &doc,
                position,
                uri,
                &self.api_definitions,
            ))
        } else {
            Ok(None)
        }
    }

    async fn supertypes(
        &self,
        params: TypeHierarchySupertypesParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        let uri = &params.item.uri;

        if let Some(doc) = self.documents.get(uri) {
            Ok(get_supertypes(&params.item, &doc, &self.api_definitions))
        } else {
            Ok(None)
        }
    }

    async fn subtypes(
        &self,
        params: TypeHierarchySubtypesParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        let uri = &params.item.uri;

        if let Some(doc) = self.documents.get(uri) {
            Ok(get_subtypes(&params.item, &doc, &self.api_definitions))
        } else {
            Ok(None)
        }
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        // Check if inlay hints are enabled
        if let Ok(settings) = self.settings.read() {
            if !settings.inlay_hints_enabled {
                return Ok(Some(vec![]));
            }
        }

        let uri = &params.text_document.uri;
        let range = params.range;

        if let Some(doc) = self.documents.get(uri) {
            let hints = get_inlay_hints(&doc, range, &self.api_definitions);
            Ok(Some(hints))
        } else {
            Ok(None)
        }
    }
}
