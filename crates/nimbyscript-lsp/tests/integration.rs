//! Integration tests for NimbyScript LSP using fixture files.
//!
//! This module tests that:
//! - Valid fixtures produce no errors
//! - Invalid fixtures produce expected errors with specific codes and messages
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
