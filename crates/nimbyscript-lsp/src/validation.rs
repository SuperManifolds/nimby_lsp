//! Validation of NimbyScript code against the game API and meta specifications.

use nimbyscript_analyzer::{ApiDefinitions, Diagnostic};
use nimbyscript_parser::{kind, Node, NodeExt};

use crate::type_inference::extract_params_strings;

// =============================================================================
// Callback Validation
// =============================================================================

/// Validate public function signatures against API callbacks
pub fn validate_public_functions(
    node: Node,
    content: &str,
    api: &ApiDefinitions,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if node.kind() == kind::FUNCTION_DEFINITION {
        validate_fn_decl(node, content, api, diagnostics);
    } else {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            validate_public_functions(child, content, api, diagnostics);
        }
    }
}

/// Parsed function signature info
struct FnSignature {
    is_pub: bool,
    name_full: String,
    name_start: usize,
    name_end: usize,
    params: Vec<(String, String)>,
    return_type: Option<String>,
}

/// Parse function signature from AST node
fn parse_fn_signature(node: Node, content: &str) -> FnSignature {
    let mut sig = FnSignature {
        is_pub: false,
        name_full: String::new(),
        name_start: node.start_byte(),
        name_end: node.end_byte(),
        params: Vec::new(),
        return_type: None,
    };

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            kind::VISIBILITY_MODIFIER => sig.is_pub = true,
            kind::FUNCTION_NAME => {
                sig.name_full = child.text(content).to_string();
                sig.name_start = child.start_byte();
                sig.name_end = child.end_byte();
            }
            kind::PARAMETERS => {
                sig.params = extract_params_strings(child, content);
            }
            kind::TYPE => {
                if node.child_by_field("return_type").map(|n| n.id()) == Some(child.id()) {
                    sig.return_type = Some(child.text(content).to_string());
                }
            }
            _ => {}
        }
    }
    sig
}

fn validate_fn_decl(
    node: Node,
    content: &str,
    api: &ApiDefinitions,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let sig = parse_fn_signature(node, content);
    if !sig.is_pub {
        return;
    }

    let span = nimbyscript_parser::ast::Span::new(sig.name_start, sig.name_end);
    let method_name = sig
        .name_full
        .find("::")
        .map_or(sig.name_full.as_str(), |pos| &sig.name_full[pos + 2..]);

    let Some(callback) = api.get_callback(method_name) else {
        let valid_names: Vec<_> = api.callback_names().collect();
        diagnostics.push(
            Diagnostic::error(
                format!(
                    "'{method_name}' is not a valid game callback. Valid callbacks are: {}",
                    valid_names.join(", ")
                ),
                span,
            )
            .with_code("E0100"),
        );
        return;
    };

    let expected_params: Vec<_> = callback.params.iter().collect();
    if sig.params.len() != expected_params.len() {
        diagnostics.push(
            Diagnostic::error(
                format!(
                    "'{method_name}' expects {} parameters, but {} were provided",
                    expected_params.len(),
                    sig.params.len()
                ),
                span,
            )
            .with_code("E0101"),
        );
        return;
    }

    validate_callback_params(
        &sig.params,
        &expected_params,
        method_name,
        span,
        diagnostics,
    );
    validate_callback_return(
        sig.return_type.as_deref(),
        callback.return_type.as_deref(),
        method_name,
        span,
        diagnostics,
    );
}

fn validate_callback_params(
    params: &[(String, String)],
    expected: &[&nimbyscript_analyzer::ParamDef],
    method_name: &str,
    span: nimbyscript_parser::ast::Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for (i, ((param_name, param_type), exp)) in params.iter().zip(expected.iter()).enumerate() {
        if exp.name != "self" && param_name != &exp.name {
            diagnostics.push(
                Diagnostic::error(
                    format!(
                        "Parameter {} of '{method_name}' is named '{param_name}', expected '{}'",
                        i + 1,
                        exp.name
                    ),
                    span,
                )
                .with_code("E0104"),
            );
        }

        let param_norm = param_type.trim().replace(' ', "");
        let exp_norm = exp.ty.trim().replace(' ', "");
        let types_match = if exp_norm == "&Self" {
            param_norm.starts_with('&')
        } else {
            param_norm == exp_norm
        };

        if !types_match {
            diagnostics.push(
                Diagnostic::error(
                    format!(
                        "Parameter {} of '{method_name}' has type '{}', expected '{}'",
                        i + 1,
                        param_type.trim(),
                        exp.ty
                    ),
                    span,
                )
                .with_code("E0102"),
            );
        }
    }
}

fn validate_callback_return(
    actual: Option<&str>,
    expected: Option<&str>,
    method_name: &str,
    span: nimbyscript_parser::ast::Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match (actual, expected) {
        (Some(actual), Some(expected)) => {
            let actual_norm = actual.trim().replace(' ', "");
            let expected_norm = expected.trim().replace(' ', "");
            if actual_norm != expected_norm {
                diagnostics.push(
                    Diagnostic::error(
                        format!(
                            "'{method_name}' should return '{expected}', but returns '{actual}'"
                        ),
                        span,
                    )
                    .with_code("E0103"),
                );
            }
        }
        (None, Some(expected)) => {
            diagnostics.push(
                Diagnostic::error(format!("'{method_name}' should return '{expected}'"), span)
                    .with_code("E0103"),
            );
        }
        (Some(actual), None) => {
            diagnostics.push(
                Diagnostic::error(
                    format!(
                        "'{method_name}' should not have a return type, but returns '{actual}'"
                    ),
                    span,
                )
                .with_code("E0103"),
            );
        }
        (None, None) => {}
    }
}

// =============================================================================
// Meta Block Validation
// =============================================================================

/// Validate meta blocks according to wiki specification
pub fn validate_meta_blocks(node: Node, content: &str, diagnostics: &mut Vec<Diagnostic>) {
    validate_meta_blocks_inner(node, content, diagnostics, true);
}

fn validate_meta_blocks_inner(
    node: Node,
    content: &str,
    diagnostics: &mut Vec<Diagnostic>,
    is_top_level: bool,
) {
    let mut found_script_meta = false;

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            kind::SOURCE_FILE => {
                // source_file is the root rule, recurse as top level
                validate_meta_blocks_inner(child, content, diagnostics, true);
                return;
            }
            kind::SCRIPT_META => {
                found_script_meta = true;
                validate_script_meta(child, content, diagnostics);
            }
            kind::STRUCT_DEFINITION => {
                validate_struct_meta(child, content, diagnostics);
            }
            kind::ENUM_DEFINITION => {
                validate_enum_meta(child, content, diagnostics);
            }
            _ => {
                validate_meta_blocks_inner(child, content, diagnostics, false);
            }
        }
    }

    // Script meta is recommended but optional
    if is_top_level && !found_script_meta {
        diagnostics.push(
            Diagnostic::warning(
                "Missing script meta block. Add: script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }".to_string(),
                nimbyscript_parser::ast::Span::new(0, 0),
            )
            .with_code("W0200"),
        );
    }
}

/// Validate a single script meta entry
fn validate_script_meta_entry(
    child: Node,
    content: &str,
    diagnostics: &mut Vec<Diagnostic>,
) -> (bool, bool) {
    let Some(key_node) = child.child_by_field("key") else {
        return (false, false);
    };
    let key = key_node.text(content);
    let value_node = child.child_by_field("value");
    let span = nimbyscript_parser::ast::Span::new(child.start_byte(), child.end_byte());

    match key {
        "lang" => {
            if let Some(val) = value_node {
                validate_lang_value(val, content, span, diagnostics);
            }
            (true, false)
        }
        "api" => {
            if let Some(val) = value_node {
                validate_api_value(val, content, span, diagnostics);
            }
            (false, true)
        }
        "description" => {
            if let Some(val) = value_node {
                validate_description_value(val, content, span, diagnostics);
            }
            (false, false)
        }
        other => {
            diagnostics.push(
                Diagnostic::warning(format!("Unknown script meta field '{other}'"), span)
                    .with_code("E0201"),
            );
            (false, false)
        }
    }
}

/// Validate lang must be a name equal to "nimbyscript.v1"
fn validate_lang_value(
    node: Node,
    content: &str,
    span: nimbyscript_parser::ast::Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let value = node.text(content);
    // lang must be the identifier "nimbyscript.v1" (a meta_name, not a string)
    if node.kind() != kind::META_NAME || value != "nimbyscript.v1" {
        diagnostics.push(
            Diagnostic::error(
                format!("'lang' must be 'nimbyscript.v1', got '{value}'"),
                span,
            )
            .with_code("E0204"),
        );
    }
}

/// Validate api must be a name equal to "nimbyrails.v1"
fn validate_api_value(
    node: Node,
    content: &str,
    span: nimbyscript_parser::ast::Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let value = node.text(content);
    // api must be the identifier "nimbyrails.v1" (a meta_name, not a string)
    if node.kind() != kind::META_NAME || value != "nimbyrails.v1" {
        diagnostics.push(
            Diagnostic::error(
                format!("'api' must be 'nimbyrails.v1', got '{value}'"),
                span,
            )
            .with_code("E0204"),
        );
    }
}

/// Validate description must be an array of strings
fn validate_description_value(
    node: Node,
    content: &str,
    span: nimbyscript_parser::ast::Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if node.kind() != kind::META_ARRAY {
        diagnostics.push(
            Diagnostic::error(
                format!(
                    "'description' must be an array of strings (e.g., [\"line1\", \"line2\"]), got '{}'",
                    node.text(content)
                ),
                span,
            )
            .with_code("E0204"),
        );
        return;
    }

    // Validate that all array elements are strings
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        // Skip punctuation and whitespace
        if child.kind() == "[" || child.kind() == "]" || child.kind() == "," || child.is_extra() {
            continue;
        }
        // Array elements must be strings
        if child.kind() != kind::STRING_LITERAL {
            diagnostics.push(
                Diagnostic::error(
                    format!(
                        "'description' array elements must be strings, got '{}' ({})",
                        child.text(content),
                        child.kind()
                    ),
                    nimbyscript_parser::ast::Span::new(child.start_byte(), child.end_byte()),
                )
                .with_code("E0204"),
            );
        }
    }
}

/// Validate script-level meta (requires lang and api)
fn validate_script_meta(node: Node, content: &str, diagnostics: &mut Vec<Diagnostic>) {
    let (has_lang, has_api) = collect_script_meta_flags(node, content, diagnostics);

    if !has_lang {
        diagnostics.push(
            Diagnostic::error(
                "Script meta missing required 'lang' field (e.g., lang: nimbyscript.v1)"
                    .to_string(),
                nimbyscript_parser::ast::Span::new(node.start_byte(), node.end_byte()),
            )
            .with_code("E0200"),
        );
    }

    if !has_api {
        diagnostics.push(
            Diagnostic::error(
                "Script meta missing required 'api' field (e.g., api: nimbyrails.v1)".to_string(),
                nimbyscript_parser::ast::Span::new(node.start_byte(), node.end_byte()),
            )
            .with_code("E0200"),
        );
    }
}

fn collect_script_meta_flags(
    node: Node,
    content: &str,
    diagnostics: &mut Vec<Diagnostic>,
) -> (bool, bool) {
    let Some(meta_block) = node.child_by_kind(kind::META_BLOCK) else {
        return (false, false);
    };
    let Some(meta_map) = meta_block.child_by_kind(kind::META_MAP) else {
        return (false, false);
    };

    let mut has_lang = false;
    let mut has_api = false;
    let mut cursor = meta_map.walk();
    for child in meta_map.children(&mut cursor) {
        if child.kind() == kind::META_ENTRY {
            let (lang, api) = validate_script_meta_entry(child, content, diagnostics);
            has_lang = has_lang || lang;
            has_api = has_api || api;
        }
    }
    (has_lang, has_api)
}

/// Validate struct-level and field-level meta
fn validate_struct_meta(node: Node, content: &str, diagnostics: &mut Vec<Diagnostic>) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            "meta_field" => {
                // Struct-level meta: only 'label' is valid
                if let Some(meta_block) = child.child_by_kind(kind::META_BLOCK) {
                    validate_meta_keys(meta_block, content, &["label"], "struct", diagnostics);
                }
            }
            kind::STRUCT_FIELD => {
                // Field-level meta: depends on field type
                validate_field_meta(child, content, diagnostics);
            }
            _ => {}
        }
    }
}

/// Validate a single field meta entry
fn validate_field_meta_entry(
    child: Node,
    key: &str,
    field_type: &str,
    content: &str,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let is_numeric = field_type == "i64" || field_type == "f64";
    let is_bool = field_type == "bool";
    let span = nimbyscript_parser::ast::Span::new(child.start_byte(), child.end_byte());
    let value_node = child.child_by_field("value");

    match key {
        "label" => {
            // label must be a string
            if let Some(val) = value_node {
                if val.kind() != kind::STRING_LITERAL {
                    diagnostics.push(
                        Diagnostic::error(
                            format!("'label' must be a string, got '{}'", val.text(content)),
                            span,
                        )
                        .with_code("E0205"),
                    );
                }
            }
        }
        "min" | "max" => {
            if !is_numeric {
                diagnostics.push(
                    Diagnostic::error(
                        format!(
                            "'{key}' is only valid for numeric types (i64, f64), not '{field_type}'"
                        ),
                        span,
                    )
                    .with_code("E0202"),
                );
            } else if let Some(val) = value_node {
                // min/max must be a number literal
                if val.kind() != kind::NUMBER {
                    diagnostics.push(
                        Diagnostic::error(
                            format!("'{}' must be a number, got '{}'", key, val.text(content)),
                            span,
                        )
                        .with_code("E0205"),
                    );
                }
            }
        }
        "default" => {
            if !is_numeric && !is_bool && !field_type.chars().next().is_some_and(char::is_uppercase)
            {
                diagnostics.push(
                    Diagnostic::warning(
                        format!("'default' may not be valid for type '{field_type}'"),
                        span,
                    )
                    .with_code("E0203"),
                );
            } else if let Some(val) = value_node {
                validate_default_value(val, field_type, content, span, diagnostics);
            }
        }
        other => {
            diagnostics.push(
                Diagnostic::warning(
                    format!(
                        "Unknown field meta key '{other}'. Valid keys: label, min, max, default"
                    ),
                    span,
                )
                .with_code("E0201"),
            );
        }
    }
}

/// Validate the value of a 'default' meta key based on field type
fn validate_default_value(
    node: Node,
    field_type: &str,
    content: &str,
    span: nimbyscript_parser::ast::Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let value_kind = node.kind();
    let value_text = node.text(content);

    match field_type {
        "bool" => {
            // default for bool must be true/false
            // In meta context, these are parsed as META_NAME, not BOOLEAN
            let is_valid_bool = value_kind == kind::BOOLEAN
                || (value_kind == kind::META_NAME
                    && (value_text == "true" || value_text == "false"));
            if !is_valid_bool {
                diagnostics.push(
                    Diagnostic::error(
                        format!("'default' for bool must be 'true' or 'false', got '{value_text}'"),
                        span,
                    )
                    .with_code("E0205"),
                );
            }
        }
        "i64" | "f64" => {
            // default for numeric must be a number
            if value_kind != kind::NUMBER {
                diagnostics.push(
                    Diagnostic::error(
                        format!("'default' for {field_type} must be a number, got '{value_text}'"),
                        span,
                    )
                    .with_code("E0205"),
                );
            }
        }
        _ if field_type.chars().next().is_some_and(char::is_uppercase) => {
            // Enum type - default must be an identifier (enum variant name)
            if value_kind != kind::META_NAME && value_kind != kind::IDENTIFIER {
                diagnostics.push(
                    Diagnostic::error(
                        format!(
                            "'default' for enum '{field_type}' must be a variant name, got '{value_text}'"
                        ),
                        span,
                    )
                    .with_code("E0205"),
                );
            }
        }
        _ => {} // Other types - already warned by E0203
    }
}

/// Validate field meta based on field type
fn validate_field_meta(node: Node, content: &str, diagnostics: &mut Vec<Diagnostic>) {
    let mut field_type = String::new();
    let mut meta_node: Option<Node> = None;

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            kind::TYPE => field_type = child.text(content).to_string(),
            kind::META_BLOCK => meta_node = Some(child),
            _ => {}
        }
    }

    let Some(meta) = meta_node else {
        return;
    };
    let Some(meta_map) = meta.child_by_kind(kind::META_MAP) else {
        return;
    };

    let mut cursor = meta_map.walk();
    for child in meta_map.children(&mut cursor) {
        if child.kind() != kind::META_ENTRY {
            continue;
        }
        let Some(key_node) = child.child_by_field("key") else {
            continue;
        };
        validate_field_meta_entry(
            child,
            key_node.text(content),
            &field_type,
            content,
            diagnostics,
        );
    }
}

/// Validate enum definition meta (variant-level)
fn validate_enum_meta(node: Node, content: &str, diagnostics: &mut Vec<Diagnostic>) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == kind::ENUM_VARIANT {
            // Enum variant meta: only 'label' is valid
            if let Some(meta) = child.child_by_kind(kind::META_BLOCK) {
                validate_meta_keys(meta, content, &["label"], "enum variant", diagnostics);
            }
        }
    }
}

/// Helper to validate meta keys against allowed list
fn validate_meta_keys(
    node: Node,
    content: &str,
    allowed: &[&str],
    context: &str,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let Some(meta_map) = node.child_by_kind(kind::META_MAP) else {
        return;
    };

    let mut cursor = meta_map.walk();
    for child in meta_map.children(&mut cursor) {
        if child.kind() != kind::META_ENTRY {
            continue;
        }
        let Some(key_node) = child.child_by_field("key") else {
            continue;
        };
        let key = key_node.text(content);
        if allowed.contains(&key) {
            continue;
        }

        diagnostics.push(
            Diagnostic::warning(
                format!(
                    "Unknown {context} meta key '{key}'. Valid keys: {}",
                    allowed.join(", ")
                ),
                nimbyscript_parser::ast::Span::new(child.start_byte(), child.end_byte()),
            )
            .with_code("E0201"),
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::load_api;
    use nimbyscript_analyzer::diagnostics::Severity;
    use nimbyscript_parser::parse;

    fn get_diagnostics(content: &str) -> Vec<Diagnostic> {
        let tree = parse(content);
        let mut diagnostics = Vec::new();
        validate_meta_blocks(tree.root_node(), content, &mut diagnostics);
        diagnostics
    }

    fn get_callback_diagnostics(content: &str) -> Vec<Diagnostic> {
        let tree = parse(content);
        let api = load_api();
        let mut diagnostics = Vec::new();
        validate_public_functions(tree.root_node(), content, &api, &mut diagnostics);
        diagnostics
    }

    fn errors(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
        diags
            .iter()
            .filter(|d| matches!(d.severity, Severity::Error))
            .collect()
    }

    fn warnings(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
        diags
            .iter()
            .filter(|d| matches!(d.severity, Severity::Warning))
            .collect()
    }

    // Meta block validation tests

    #[test]
    fn test_valid_script_meta() {
        let content = "script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }";
        let diags = get_diagnostics(content);
        assert!(
            errors(&diags).is_empty(),
            "Valid meta should have no errors: {diags:?}"
        );
    }

    #[test]
    fn test_missing_script_meta() {
        let content = "pub struct Foo { }";
        let diags = get_diagnostics(content);
        let warns = warnings(&diags);
        assert!(!warns.is_empty(), "Missing meta should produce warning");
        assert!(warns.iter().any(|d| d.code.as_deref() == Some("W0200")));
    }

    #[test]
    fn test_missing_lang_field() {
        let content = "script meta { api: nimbyrails.v1, }";
        let diags = get_diagnostics(content);
        let errs = errors(&diags);
        assert!(errs.iter().any(|d| d.message.contains("lang")));
    }

    #[test]
    fn test_missing_api_field() {
        let content = "script meta { lang: nimbyscript.v1, }";
        let diags = get_diagnostics(content);
        let errs = errors(&diags);
        assert!(errs.iter().any(|d| d.message.contains("api")));
    }

    #[test]
    fn test_unknown_meta_field_warning() {
        let content = "script meta { lang: nimbyscript.v1, api: nimbyrails.v1, foo: bar, }";
        let diags = get_diagnostics(content);
        let warns = warnings(&diags);
        assert!(!warns.is_empty(), "Unknown field should produce warning");
        assert!(warns.iter().any(|d| d.message.contains("foo")));
    }

    #[test]
    fn test_description_meta_allowed() {
        let content =
            "script meta { lang: nimbyscript.v1, api: nimbyrails.v1, description: test, }";
        let diags = get_diagnostics(content);
        let warns = warnings(&diags);
        assert!(
            warns.iter().all(|d| !d.message.contains("description")),
            "description should be allowed"
        );
    }

    #[test]
    fn test_struct_meta_label() {
        let content = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Foo {
    meta { label: "My Struct", }
}
"#;
        let diags = get_diagnostics(content);
        // label is valid for struct meta
        assert!(errors(&diags).is_empty());
    }

    #[test]
    fn test_field_meta_min_max_numeric() {
        let content = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Foo {
    value: i64 meta { min: 0, max: 100, },
}
";
        let diags = get_diagnostics(content);
        assert!(errors(&diags).is_empty(), "min/max on i64 should be valid");
    }

    #[test]
    fn test_field_meta_min_on_string_error() {
        let content = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Foo {
    name: string meta { min: 0, },
}
";
        let diags = get_diagnostics(content);
        let errs = errors(&diags);
        assert!(!errs.is_empty(), "min on string should error");
        assert!(errs.iter().any(|d| d.code.as_deref() == Some("E0202")));
    }

    #[test]
    fn test_enum_variant_meta() {
        let content = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub enum Color {
    Red meta { label: "Red Color", },
    Green,
}
"#;
        let diags = get_diagnostics(content);
        assert!(
            errors(&diags).is_empty(),
            "label on enum variant should be valid"
        );
    }

    // Callback validation tests
    // NimbyScript syntax: `self: &Type` (not `&self`), return type uses `:` (not `->`)

    #[test]
    fn test_valid_callback_signature() {
        // event_signal_check has 5 params: self, ctx, train, motion, signal
        let content = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal { }
pub fn Test::event_signal_check(self: &Test, ctx: &EventCtx, train: &Train, motion: &Motion, signal: &Signal): SignalCheck {
    return SignalCheck::Pass;
}
";
        let diags = get_callback_diagnostics(content);
        assert!(
            errors(&diags).is_empty(),
            "Valid callback should have no errors: {diags:?}"
        );
    }

    #[test]
    fn test_invalid_callback_name() {
        let content = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal { }
pub fn Test::not_a_real_callback(self: &Test) { }
";
        let diags = get_callback_diagnostics(content);
        let errs = errors(&diags);
        assert!(!errs.is_empty(), "Invalid callback name should error");
        assert!(errs.iter().any(|d| d.code.as_deref() == Some("E0100")));
    }

    #[test]
    fn test_callback_param_count_mismatch() {
        // event_signal_check expects 5 params, but we only provide 1
        let content = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal { }
pub fn Test::event_signal_check(self: &Test): SignalCheck {
    return SignalCheck::Pass;
}
";
        let diags = get_callback_diagnostics(content);
        let errs = errors(&diags);
        assert!(!errs.is_empty(), "Wrong param count should error");
        assert!(errs.iter().any(|d| d.code.as_deref() == Some("E0101")));
    }

    #[test]
    fn test_callback_return_type_mismatch() {
        let content = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal { }
pub fn Test::event_signal_check(self: &Test, ctx: &EventCtx, train: &Train, motion: &Motion, signal: &Signal): i64 {
    return 0;
}
";
        let diags = get_callback_diagnostics(content);
        let errs = errors(&diags);
        assert!(!errs.is_empty(), "Wrong return type should error");
        assert!(errs.iter().any(|d| d.code.as_deref() == Some("E0103")));
    }

    #[test]
    fn test_nonpublic_function_ignored() {
        let content = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal { }
fn Test::not_a_callback(self: &Test) { }
";
        let diags = get_callback_diagnostics(content);
        // Non-public functions should not be validated as callbacks
        assert!(
            errors(&diags).is_empty(),
            "Non-public fn should be ignored: {diags:?}"
        );
    }

    #[test]
    fn test_callback_param_type_mismatch() {
        // All params correct except ctx which should be &EventCtx not i64
        let content = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal { }
pub fn Test::event_signal_check(self: &Test, ctx: i64, train: &Train, motion: &Motion, signal: &Signal): SignalCheck {
    return SignalCheck::Pass;
}
";
        let diags = get_callback_diagnostics(content);
        let errs = errors(&diags);
        assert!(!errs.is_empty(), "Wrong param type should error");
        assert!(errs.iter().any(|d| d.code.as_deref() == Some("E0102")));
    }

    #[test]
    fn test_self_param_type_flexible() {
        // self: &StructName should match &Self in callback definition
        let content = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct MyHandler extend Signal { }
pub fn MyHandler::event_signal_check(self: &MyHandler, ctx: &EventCtx, train: &Train, motion: &Motion, signal: &Signal): SignalCheck {
    return SignalCheck::Pass;
}
";
        let diags = get_callback_diagnostics(content);
        // Should not error on self: &StructName type
        let type_errs: Vec<_> = errors(&diags)
            .into_iter()
            .filter(|d| d.code.as_deref() == Some("E0102") && d.message.contains("self"))
            .collect();
        assert!(
            type_errs.is_empty(),
            "self: &StructName should be accepted: {type_errs:?}"
        );
    }

    #[test]
    fn test_callback_missing_return_type() {
        let content = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal { }
pub fn Test::event_signal_check(self: &Test, ctx: &EventCtx, train: &Train, motion: &Motion, signal: &Signal) {
    // Missing return type
}
";
        let diags = get_callback_diagnostics(content);
        let errs = errors(&diags);
        assert!(!errs.is_empty(), "Missing return type should error");
        assert!(errs.iter().any(|d| d.code.as_deref() == Some("E0103")));
    }

    // E0204 - Script meta value validation tests

    #[test]
    fn test_lang_wrong_value() {
        let content = "script meta { lang: wronglang.v1, api: nimbyrails.v1, }";
        let diags = get_diagnostics(content);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0204")),
            "Wrong lang value should error: {errs:?}"
        );
    }

    #[test]
    fn test_lang_wrong_type_number() {
        let content = "script meta { lang: 123, api: nimbyrails.v1, }";
        let diags = get_diagnostics(content);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0204")),
            "Numeric lang value should error: {errs:?}"
        );
    }

    #[test]
    fn test_api_wrong_value() {
        let content = "script meta { lang: nimbyscript.v1, api: wrongapi.v1, }";
        let diags = get_diagnostics(content);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0204")),
            "Wrong api value should error: {errs:?}"
        );
    }

    #[test]
    fn test_api_wrong_type_string() {
        let content = r#"script meta { lang: nimbyscript.v1, api: "nimbyrails.v1", }"#;
        let diags = get_diagnostics(content);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0204")),
            "String api value should error (must be name): {errs:?}"
        );
    }

    #[test]
    fn test_description_wrong_type_number() {
        let content = "script meta { lang: nimbyscript.v1, api: nimbyrails.v1, description: 42, }";
        let diags = get_diagnostics(content);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0204")),
            "Numeric description should error: {errs:?}"
        );
    }

    #[test]
    fn test_description_wrong_type_name() {
        let content =
            "script meta { lang: nimbyscript.v1, api: nimbyrails.v1, description: hello, }";
        let diags = get_diagnostics(content);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0204")),
            "Name description should error (must be array): {errs:?}"
        );
    }

    #[test]
    fn test_description_valid_array() {
        let content = r#"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, description: ["hello", "world"], }"#;
        let diags = get_diagnostics(content);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0204")),
            "Valid description array should not error: {errs:?}"
        );
    }

    #[test]
    fn test_description_array_with_non_string() {
        let content = r#"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, description: ["hello", 42], }"#;
        let diags = get_diagnostics(content);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0204")),
            "Description array with number should error: {errs:?}"
        );
    }

    // E0205 - Field meta value type validation tests

    #[test]
    fn test_label_must_be_string() {
        let content = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal {
    count: i64 meta { label: 123, },
}
";
        let diags = get_diagnostics(content);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0205")),
            "Non-string label should error: {errs:?}"
        );
    }

    #[test]
    fn test_min_must_be_number() {
        let content = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal {
    count: i64 meta { min: "hello", },
}
"#;
        let diags = get_diagnostics(content);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0205")),
            "Non-number min should error: {errs:?}"
        );
    }

    #[test]
    fn test_max_must_be_number() {
        let content = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal {
    count: f64 meta { max: true, },
}
";
        let diags = get_diagnostics(content);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0205")),
            "Non-number max should error: {errs:?}"
        );
    }

    #[test]
    fn test_default_bool_must_be_bool() {
        let content = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal {
    flag: bool meta { default: 42, },
}
";
        let diags = get_diagnostics(content);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0205")),
            "Non-bool default for bool field should error: {errs:?}"
        );
    }

    #[test]
    fn test_default_numeric_must_be_number() {
        let content = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal {
    count: i64 meta { default: "wrong", },
}
"#;
        let diags = get_diagnostics(content);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0205")),
            "Non-number default for i64 should error: {errs:?}"
        );
    }

    #[test]
    fn test_default_enum_must_be_name() {
        let content = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
enum Mode { Fast, Slow, }
pub struct Test extend Signal {
    mode: Mode meta { default: 123, },
}
";
        let diags = get_diagnostics(content);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0205")),
            "Non-name default for enum should error: {errs:?}"
        );
    }

    #[test]
    fn test_valid_field_meta_values() {
        let content = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
enum Mode { Fast, Slow, }
pub struct Test extend Signal {
    count: i64 meta { label: "Count", min: 0, max: 100, default: 50, },
    flag: bool meta { label: "Flag", default: true, },
    speed: f64 meta { min: 0.0, max: 1.0, default: 0.5, },
    mode: Mode meta { default: Fast, },
}
"#;
        let diags = get_diagnostics(content);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0205")),
            "Valid field meta values should not error: {errs:?}"
        );
    }
}
