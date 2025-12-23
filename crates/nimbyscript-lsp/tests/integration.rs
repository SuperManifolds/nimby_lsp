//! Integration tests for NimbyScript LSP using fixture files.
//!
//! This module tests that:
//! - Valid fixtures produce no errors
//! - Invalid fixtures produce expected errors with specific codes and messages
//! - Valid fixtures have no Unknown types in inferred bindings
//!
//! Expected errors are declared in fixture files using comment annotations:
//! ```text
//! // @error E0307 "has no static method"
//! let x = BadType::invalid();
//! ```

use std::fs;
use std::path::Path;

use nimbyscript_analyzer::diagnostics::Severity;
use nimbyscript_analyzer::{ApiDefinitions, Diagnostic};
use nimbyscript_lsp::document::Document;
use tower_lsp::lsp_types::{InlayHintLabel, Position, Range};

/// Load the API definitions from the standard location.
fn load_api() -> ApiDefinitions {
    let api_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("parent")
        .parent()
        .expect("parent")
        .join("api-definitions/nimbyrails.v1.toml");
    ApiDefinitions::load_from_file(&api_path).expect("Failed to load API definitions")
}

/// Get the fixtures directory path.
fn fixtures_dir() -> std::path::PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("parent")
        .parent()
        .expect("parent")
        .join("tests/fixtures")
}

/// Analyze a file and return its diagnostics.
fn analyze_file(path: &Path) -> Vec<Diagnostic> {
    let content = fs::read_to_string(path).expect("Failed to read fixture file");
    let api = load_api();
    let doc = Document::new(content, Some(&api));
    doc.diagnostics().to_vec()
}

/// Expected diagnostic parsed from a comment annotation.
#[derive(Debug, Clone)]
struct ExpectedDiagnostic {
    severity: Severity,
    code: String,
    message_substring: String,
    line: usize,
}

/// Parse expected diagnostics from comment annotations in a file.
///
/// Format: `// @error E0307 "substring"` or `// @warning W0601 "substring"`
fn parse_expected_diagnostics(content: &str) -> Vec<ExpectedDiagnostic> {
    let mut expected = Vec::new();

    for (line_num, line) in content.lines().enumerate() {
        let trimmed = line.trim();
        if let Some(annotation) = trimmed.strip_prefix("// @") {
            let parts: Vec<&str> = annotation.splitn(3, ' ').collect();
            if parts.len() >= 3 {
                let severity = match parts[0] {
                    "error" => Severity::Error,
                    "warning" => Severity::Warning,
                    _ => continue,
                };
                let code = parts[1].to_string();
                // Extract message from quotes
                let msg_part = parts[2];
                if let Some(msg) = msg_part.strip_prefix('"').and_then(|s| s.strip_suffix('"')) {
                    expected.push(ExpectedDiagnostic {
                        severity,
                        code,
                        message_substring: msg.to_string(),
                        line: line_num + 1, // 1-indexed for error messages
                    });
                }
            }
        }
    }

    expected
}

/// Check if actual diagnostics match expected diagnostics.
fn assert_diagnostics_match(
    actual: &[Diagnostic],
    expected: &[ExpectedDiagnostic],
    file_name: &str,
) {
    // For each expected diagnostic, find a matching actual diagnostic
    let mut unmatched_expected = Vec::new();

    for exp in expected {
        let found = actual.iter().any(|d| {
            d.severity == exp.severity
                && d.code.as_deref() == Some(&exp.code)
                && d.message.contains(&exp.message_substring)
        });

        if !found {
            unmatched_expected.push(exp);
        }
    }

    // Check for unexpected errors
    let unexpected_errors: Vec<_> = actual
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .filter(|d| {
            !expected.iter().any(|e| {
                d.severity == e.severity
                    && d.code.as_deref() == Some(&e.code)
                    && d.message.contains(&e.message_substring)
            })
        })
        .collect();

    let mut errors = Vec::new();

    if !unmatched_expected.is_empty() {
        errors.push(format!(
            "Expected diagnostics not found:\n{}",
            unmatched_expected
                .iter()
                .map(|e| format!(
                    "  - {} {} \"{}\" (near line {})",
                    if e.severity == Severity::Error {
                        "error"
                    } else {
                        "warning"
                    },
                    e.code,
                    e.message_substring,
                    e.line
                ))
                .collect::<Vec<_>>()
                .join("\n")
        ));
    }

    if !unexpected_errors.is_empty() {
        errors.push(format!(
            "Unexpected errors:\n{}",
            unexpected_errors
                .iter()
                .map(|d| format!(
                    "  - {} {}: {}",
                    d.code.as_deref().unwrap_or("?"),
                    d.span.start,
                    d.message
                ))
                .collect::<Vec<_>>()
                .join("\n")
        ));
    }

    if !errors.is_empty() {
        // Print actual diagnostics for debugging
        let actual_str = actual
            .iter()
            .map(|d| {
                format!(
                    "  {} {} at {}: {}",
                    if d.severity == Severity::Error {
                        "error"
                    } else {
                        "warning"
                    },
                    d.code.as_deref().unwrap_or("?"),
                    d.span.start,
                    d.message
                )
            })
            .collect::<Vec<_>>()
            .join("\n");

        panic!(
            "\n{}\n\n{}\n\nActual diagnostics:\n{}",
            file_name,
            errors.join("\n\n"),
            actual_str
        );
    }
}

// =============================================================================
// Valid Fixture Tests
// =============================================================================

#[test]
fn valid_minimal_has_no_errors() {
    let path = fixtures_dir().join("valid/minimal.nimbyscript");
    let diagnostics = analyze_file(&path);
    let errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert!(
        errors.is_empty(),
        "minimal.nimbyscript should have no errors, but found: {errors:?}"
    );
}

#[test]
fn valid_example_has_no_errors() {
    let path = fixtures_dir().join("valid/example.nimbyscript");
    let diagnostics = analyze_file(&path);
    let errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert!(
        errors.is_empty(),
        "example.nimbyscript should have no errors, but found: {errors:?}"
    );
}

#[test]
fn valid_all_callbacks_has_no_errors() {
    let path = fixtures_dir().join("valid/all_callbacks.nimbyscript");
    let diagnostics = analyze_file(&path);
    let errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert!(
        errors.is_empty(),
        "all_callbacks.nimbyscript should have no errors, but found: {errors:?}"
    );
}

#[test]
fn valid_complex_types_has_no_errors() {
    let path = fixtures_dir().join("valid/complex_types.nimbyscript");
    let diagnostics = analyze_file(&path);
    let errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert!(
        errors.is_empty(),
        "complex_types.nimbyscript should have no errors, but found: {errors:?}"
    );
}

#[test]
fn valid_hitch_has_no_errors() {
    let path = fixtures_dir().join("valid/hitch.nimbyscript");
    let diagnostics = analyze_file(&path);
    let errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert!(
        errors.is_empty(),
        "hitch.nimbyscript should have no errors, but found: {errors:?}"
    );
}

// =============================================================================
// Invalid Fixture Tests
// =============================================================================

#[test]
fn invalid_missing_semicolon() {
    let path = fixtures_dir().join("invalid/missing_semicolon.nimbyscript");
    let content = fs::read_to_string(&path).expect("read file");
    let diagnostics = analyze_file(&path);
    let expected = parse_expected_diagnostics(&content);

    assert_diagnostics_match(&diagnostics, &expected, "missing_semicolon.nimbyscript");
}

#[test]
fn invalid_unclosed_brace() {
    let path = fixtures_dir().join("invalid/unclosed_brace.nimbyscript");
    let content = fs::read_to_string(&path).expect("read file");
    let diagnostics = analyze_file(&path);
    let expected = parse_expected_diagnostics(&content);

    assert_diagnostics_match(&diagnostics, &expected, "unclosed_brace.nimbyscript");
}

#[test]
fn invalid_bad_callback_sig() {
    let path = fixtures_dir().join("invalid/bad_callback_sig.nimbyscript");
    let content = fs::read_to_string(&path).expect("read file");
    let diagnostics = analyze_file(&path);
    let expected = parse_expected_diagnostics(&content);

    assert_diagnostics_match(&diagnostics, &expected, "bad_callback_sig.nimbyscript");
}

#[test]
fn invalid_method() {
    let path = fixtures_dir().join("invalid/invalid_method.nimbyscript");
    let content = fs::read_to_string(&path).expect("read file");
    let diagnostics = analyze_file(&path);
    let expected = parse_expected_diagnostics(&content);

    assert_diagnostics_match(&diagnostics, &expected, "invalid_method.nimbyscript");
}

#[test]
fn invalid_variable_param() {
    let path = fixtures_dir().join("invalid/invalid_variable_param.nimbyscript");
    let content = fs::read_to_string(&path).expect("read file");
    let diagnostics = analyze_file(&path);
    let expected = parse_expected_diagnostics(&content);

    assert_diagnostics_match(
        &diagnostics,
        &expected,
        "invalid_variable_param.nimbyscript",
    );
}

#[test]
fn invalid_struct_method() {
    let path = fixtures_dir().join("invalid/invalid_struct_method.nimbyscript");
    let content = fs::read_to_string(&path).expect("read file");
    let diagnostics = analyze_file(&path);
    let expected = parse_expected_diagnostics(&content);

    assert_diagnostics_match(&diagnostics, &expected, "invalid_struct_method.nimbyscript");
}

#[test]
fn invalid_in_assignment() {
    let path = fixtures_dir().join("invalid/invalid_in_assignment.nimbyscript");
    let content = fs::read_to_string(&path).expect("read file");
    let diagnostics = analyze_file(&path);
    let expected = parse_expected_diagnostics(&content);

    assert_diagnostics_match(&diagnostics, &expected, "invalid_in_assignment.nimbyscript");
}

// =============================================================================
// Type Inference Quality Tests
// =============================================================================

/// Check that inlay hints don't show Unknown types (displayed as "?").
/// If an inlay hint shows "?", it means type inference failed.
fn check_no_unknown_inlay_hints(path: &Path) -> Vec<String> {
    let content = fs::read_to_string(path).expect("read file");
    let api = load_api();
    let doc = Document::new(content.clone(), Some(&api));

    // Get inlay hints for the entire document
    let lines = content.lines().count() as u32;
    let last_line_len = content.lines().last().map_or(0, str::len) as u32;
    let range = Range {
        start: Position {
            line: 0,
            character: 0,
        },
        end: Position {
            line: lines,
            character: last_line_len,
        },
    };

    let hints = nimbyscript_lsp::inlay_hints::get_inlay_hints(&doc, range, &api);

    let mut unknown_hints = Vec::new();
    for hint in hints {
        let label = match &hint.label {
            InlayHintLabel::String(s) => s.clone(),
            InlayHintLabel::LabelParts(parts) => parts.iter().map(|p| p.value.as_str()).collect(),
        };

        // Check for Unknown type indicators
        if label.contains('?') || label.contains("Unknown") {
            unknown_hints.push(format!(
                "Line {}: inlay hint shows unknown type: {label}",
                hint.position.line + 1,
            ));
        }
    }

    unknown_hints
}

/// Extract hover content as a string.
fn extract_hover_content(hover: tower_lsp::lsp_types::Hover) -> String {
    match hover.contents {
        tower_lsp::lsp_types::HoverContents::Markup(m) => m.value,
        tower_lsp::lsp_types::HoverContents::Scalar(s) => match s {
            tower_lsp::lsp_types::MarkedString::String(s) => s,
            tower_lsp::lsp_types::MarkedString::LanguageString(ls) => ls.value,
        },
        tower_lsp::lsp_types::HoverContents::Array(arr) => arr
            .iter()
            .map(|s| match s {
                tower_lsp::lsp_types::MarkedString::String(s) => s.clone(),
                tower_lsp::lsp_types::MarkedString::LanguageString(ls) => ls.value.clone(),
            })
            .collect::<Vec<_>>()
            .join("\n"),
    }
}

/// Check a single binding for unknown type in hover.
fn check_binding_hover(
    doc: &Document,
    api: &ApiDefinitions,
    line_num: usize,
    line: &str,
    pattern: &str,
) -> Option<String> {
    let pos = line.find(pattern)?;
    let after_keyword = &line[pos + pattern.len()..];

    // Extract identifier
    let ident_end = after_keyword
        .find(|c: char| !c.is_alphanumeric() && c != '_')
        .unwrap_or(after_keyword.len());
    if ident_end == 0 {
        return None;
    }

    let ident = &after_keyword[..ident_end];
    // Skip keywords like "mut"
    if ident == "mut" {
        return None;
    }

    let position = Position {
        line: line_num as u32,
        character: (pos + pattern.len()) as u32,
    };

    let hover = nimbyscript_lsp::hover::get_hover(doc, position, api)?;
    let hover_content = extract_hover_content(hover);

    // Check for Unknown type
    if hover_content.contains(": ?")
        || hover_content.contains(": Unknown")
        || hover_content.contains("`: ?")
        || hover_content.contains("`: Unknown")
    {
        let first_line = hover_content.lines().next().unwrap_or(&hover_content);
        return Some(format!(
            "Line {}: hover on '{ident}' shows unknown type: {first_line}",
            line_num + 1,
        ));
    }

    None
}

/// Check that hover on identifiers doesn't show Unknown types.
fn check_no_unknown_hover_types(path: &Path) -> Vec<String> {
    let content = fs::read_to_string(path).expect("read file");
    let api = load_api();
    let doc = Document::new(content.clone(), Some(&api));

    let mut unknown_hovers = Vec::new();
    let patterns = ["let ", "if let "];

    for (line_num, line) in content.lines().enumerate() {
        for pattern in patterns {
            if let Some(err) = check_binding_hover(&doc, &api, line_num, line, pattern) {
                unknown_hovers.push(err);
            }
        }
    }

    unknown_hovers
}

#[test]
fn valid_fixtures_have_no_unknown_types() {
    let valid_dir = fixtures_dir().join("valid");
    let mut all_errors = Vec::new();

    for entry in fs::read_dir(&valid_dir).expect("read valid dir") {
        let entry = entry.expect("read entry");
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) != Some("nimbyscript") {
            continue;
        }

        let file_name = path.file_name().expect("file name").to_string_lossy();

        for err in check_no_unknown_inlay_hints(&path) {
            all_errors.push(format!("{file_name}: {err}"));
        }

        for err in check_no_unknown_hover_types(&path) {
            all_errors.push(format!("{file_name}: {err}"));
        }
    }

    assert!(
        all_errors.is_empty(),
        "Found Unknown types in valid fixtures (type inference failed):\n{}",
        all_errors.join("\n")
    );
}

fn find_parse_errors(node: nimbyscript_parser::Node, errors: &mut Vec<String>) {
    if node.is_error() || node.is_missing() {
        errors.push(format!(
            "Parse error at byte {}: {}",
            node.start_byte(),
            if node.is_missing() {
                "missing node"
            } else {
                "ERROR node"
            }
        ));
    }
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        find_parse_errors(child, errors);
    }
}

/// Check that a file has no tree-sitter parse errors (ERROR nodes).
fn check_no_parse_errors(path: &Path, api: &ApiDefinitions) -> Vec<String> {
    let content = fs::read_to_string(path).expect("read file");
    let doc = Document::new(content, Some(api));

    let mut errors = Vec::new();
    let root = doc.tree().root_node();
    find_parse_errors(root, &mut errors);
    errors
}

#[test]
fn valid_fixtures_have_no_parse_errors() {
    let valid_dir = fixtures_dir().join("valid");
    let api = load_api();
    let mut all_errors = Vec::new();

    for entry in fs::read_dir(&valid_dir).expect("read valid dir") {
        let entry = entry.expect("read entry");
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) != Some("nimbyscript") {
            continue;
        }

        let file_name = path.file_name().expect("file name").to_string_lossy();

        for err in check_no_parse_errors(&path, &api) {
            all_errors.push(format!("{file_name}: {err}"));
        }
    }

    assert!(
        all_errors.is_empty(),
        "Found parse errors in valid fixtures:\n{}",
        all_errors.join("\n")
    );
}

/// Check that semantic tokens are generated without panics and cover key constructs.
fn check_semantic_tokens(path: &Path, api: &ApiDefinitions) -> usize {
    let content = fs::read_to_string(path).expect("read file");
    let doc = Document::new(content, Some(api));

    // This will panic if there's an issue with semantic token generation
    let tokens = nimbyscript_lsp::semantic_tokens::compute_semantic_tokens(&doc, api);

    tokens.len()
}

#[test]
fn valid_fixtures_generate_semantic_tokens() {
    let valid_dir = fixtures_dir().join("valid");
    let api = load_api();

    for entry in fs::read_dir(&valid_dir).expect("read valid dir") {
        let entry = entry.expect("read entry");
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) != Some("nimbyscript") {
            continue;
        }

        let file_name = path.file_name().expect("file name").to_string_lossy();
        let count = check_semantic_tokens(&path, &api);

        // Valid files should produce some semantic tokens
        assert!(
            count > 0,
            "{file_name}: Expected semantic tokens but got none"
        );
    }
}

/// Check that document symbols are generated for structs and functions.
fn check_document_symbols(path: &Path, api: &ApiDefinitions) -> Vec<String> {
    let content = fs::read_to_string(path).expect("read file");
    let doc = Document::new(content.clone(), Some(api));

    let symbols = doc.document_symbols();

    let mut errors = Vec::new();

    // Count expected structs and functions from source
    let struct_count = content.matches("struct ").count();
    let fn_count = content.matches("fn ").count();

    // We should have at least some symbols if there are structs/functions
    if struct_count + fn_count > 0 && symbols.is_empty() {
        errors.push(format!(
            "Expected symbols for {struct_count} structs and {fn_count} functions, got none"
        ));
    }

    errors
}

#[test]
fn valid_fixtures_have_document_symbols() {
    let valid_dir = fixtures_dir().join("valid");
    let api = load_api();
    let mut all_errors = Vec::new();

    for entry in fs::read_dir(&valid_dir).expect("read valid dir") {
        let entry = entry.expect("read entry");
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) != Some("nimbyscript") {
            continue;
        }

        let file_name = path.file_name().expect("file name").to_string_lossy();

        for err in check_document_symbols(&path, &api) {
            all_errors.push(format!("{file_name}: {err}"));
        }
    }

    assert!(
        all_errors.is_empty(),
        "Document symbol issues in valid fixtures:\n{}",
        all_errors.join("\n")
    );
}
