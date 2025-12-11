mod backend;
mod completions;
mod document;
mod hover;
mod semantic_tokens;
mod signature_help;
mod validation;

use std::env;
use std::process::ExitCode;

use tower_lsp::{LspService, Server};
use tracing_subscriber::EnvFilter;

use document::Document;
use nimbyscript_analyzer::ApiDefinitions;

// Embed the API definitions TOML at compile time
const API_DEFINITIONS_TOML: &str = include_str!("../../../api-definitions/nimbyrails.v1.toml");

fn run_check(file_path: &str) -> ExitCode {
    // Load API definitions
    let api = match ApiDefinitions::load_from_str(API_DEFINITIONS_TOML) {
        Ok(api) => api,
        Err(e) => {
            eprintln!("Error loading API definitions: {e}");
            return ExitCode::FAILURE;
        }
    };

    // Read the file
    let content = match std::fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", file_path, e);
            return ExitCode::FAILURE;
        }
    };

    // Analyze
    let doc = Document::new(content, Some(&api));
    let diagnostics = doc.diagnostics();

    if diagnostics.is_empty() {
        println!("No issues found.");
        return ExitCode::SUCCESS;
    }

    // Print diagnostics
    let mut has_errors = false;
    for d in diagnostics {
        let severity = match d.severity {
            nimbyscript_analyzer::diagnostics::Severity::Error => {
                has_errors = true;
                "error"
            }
            nimbyscript_analyzer::diagnostics::Severity::Warning => "warning",
            nimbyscript_analyzer::diagnostics::Severity::Info => "info",
            nimbyscript_analyzer::diagnostics::Severity::Hint => "hint",
        };

        let code = d.code.as_deref().unwrap_or("?");
        let pos = doc.offset_to_position(d.span.start);
        println!(
            "{}:{}:{}: {} [{}]: {}",
            file_path,
            pos.line + 1,
            pos.character + 1,
            severity,
            code,
            d.message
        );
    }

    if has_errors {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

#[tokio::main]
async fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();

    // Check for --check mode
    if args.len() >= 3 && args[1] == "--check" {
        return run_check(&args[2]);
    }

    // Initialize logging
    tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info")),
        )
        .with_writer(std::io::stderr)
        .init();

    tracing::info!("Starting NimbyScript Language Server");

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(backend::Backend::new);

    Server::new(stdin, stdout, socket).serve(service).await;

    ExitCode::SUCCESS
}
