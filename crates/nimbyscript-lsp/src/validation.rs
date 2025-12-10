//! Validation of NimbyScript code against the game API and meta specifications.

use nimbyscript_analyzer::{ApiDefinitions, Diagnostic};
use nimbyscript_parser::{kind, Node, NodeExt};

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

/// Extract a single parameter's name and type from a parameter node
fn extract_parameter(param: Node, content: &str) -> (String, String) {
    let param_name = param
        .child_by_field("name")
        .map_or_else(String::new, |n| n.text(content).to_string());
    let param_type = param
        .child_by_field("type")
        .map_or_else(String::new, |n| n.text(content).to_string());
    (param_name, param_type)
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
                let mut param_cursor = child.walk();
                for param in child.children(&mut param_cursor) {
                    if param.kind() == kind::PARAMETER {
                        sig.params.push(extract_parameter(param, content));
                    }
                }
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
    let method_name = sig.name_full.find("::").map_or(sig.name_full.as_str(), |pos| &sig.name_full[pos + 2..]);

    let Some(callback) = api.get_callback(method_name) else {
        let valid_names: Vec<_> = api.callback_names().collect();
        diagnostics.push(
            Diagnostic::error(
                format!("'{method_name}' is not a valid game callback. Valid callbacks are: {}", valid_names.join(", ")),
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
                format!("'{method_name}' expects {} parameters, but {} were provided", expected_params.len(), sig.params.len()),
                span,
            )
            .with_code("E0101"),
        );
        return;
    }

    validate_callback_params(&sig.params, &expected_params, method_name, span, diagnostics);
    validate_callback_return(sig.return_type.as_deref(), callback.return_type.as_deref(), method_name, span, diagnostics);
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
                    format!("Parameter {} of '{method_name}' is named '{param_name}', expected '{}'", i + 1, exp.name),
                    span,
                )
                .with_code("E0104"),
            );
        }

        let param_norm = param_type.trim().replace(' ', "");
        let exp_norm = exp.ty.trim().replace(' ', "");
        let types_match = if exp_norm == "&Self" { param_norm.starts_with('&') } else { param_norm == exp_norm };

        if !types_match {
            diagnostics.push(
                Diagnostic::error(
                    format!("Parameter {} of '{method_name}' has type '{}', expected '{}'", i + 1, param_type.trim(), exp.ty),
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
                    Diagnostic::error(format!("'{method_name}' should return '{expected}', but returns '{actual}'"), span)
                        .with_code("E0103"),
                );
            }
        }
        (None, Some(expected)) => {
            diagnostics.push(
                Diagnostic::error(format!("'{method_name}' should return '{expected}'"), span).with_code("E0103"),
            );
        }
        (Some(actual), None) => {
            diagnostics.push(
                Diagnostic::error(format!("'{method_name}' should not have a return type, but returns '{actual}'"), span)
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
pub fn validate_meta_blocks(
    node: Node,
    content: &str,
    diagnostics: &mut Vec<Diagnostic>,
) {
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

    // Script meta is required only at the top level
    if is_top_level && !found_script_meta {
        diagnostics.push(
            Diagnostic::error(
                "Missing script meta block. Add: script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }".to_string(),
                nimbyscript_parser::ast::Span::new(0, 0),
            )
            .with_code("E0200"),
        );
    }
}

/// Validate a single script meta entry
fn validate_script_meta_entry(child: Node, content: &str, diagnostics: &mut Vec<Diagnostic>) -> (bool, bool) {
    let Some(key_node) = child.child_by_field("key") else {
        return (false, false);
    };
    let key = key_node.text(content);
    match key {
        "lang" => (true, false),
        "api" => (false, true),
        "description" => (false, false),
        other => {
            diagnostics.push(
                Diagnostic::warning(
                    format!("Unknown script meta field '{other}'"),
                    nimbyscript_parser::ast::Span::new(child.start_byte(), child.end_byte()),
                )
                .with_code("E0201"),
            );
            (false, false)
        }
    }
}

/// Validate script-level meta (requires lang and api)
fn validate_script_meta(node: Node, content: &str, diagnostics: &mut Vec<Diagnostic>) {
    let (has_lang, has_api) = collect_script_meta_flags(node, content, diagnostics);

    if !has_lang {
        diagnostics.push(
            Diagnostic::error(
                "Script meta missing required 'lang' field (e.g., lang: nimbyscript.v1)".to_string(),
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

fn collect_script_meta_flags(node: Node, content: &str, diagnostics: &mut Vec<Diagnostic>) -> (bool, bool) {
    let Some(meta_block) = node.child_by_kind(kind::META_BLOCK) else { return (false, false); };
    let Some(meta_map) = meta_block.child_by_kind(kind::META_MAP) else { return (false, false); };

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
fn validate_field_meta_entry(child: Node, key: &str, field_type: &str, diagnostics: &mut Vec<Diagnostic>) {
    let is_numeric = field_type == "i64" || field_type == "f64";
    let is_bool = field_type == "bool";
    let span = nimbyscript_parser::ast::Span::new(child.start_byte(), child.end_byte());

    match key {
        "min" | "max" if !is_numeric => {
            diagnostics.push(
                Diagnostic::error(
                    format!("'{key}' is only valid for numeric types (i64, f64), not '{field_type}'"),
                    span,
                )
                .with_code("E0202"),
            );
        }
        "default" if !is_numeric && !is_bool && !field_type.chars().next().is_some_and(char::is_uppercase) => {
            diagnostics.push(
                Diagnostic::warning(format!("'default' may not be valid for type '{field_type}'"), span)
                    .with_code("E0203"),
            );
        }
        "label" | "min" | "max" | "default" => {} // Valid keys
        other => {
            diagnostics.push(
                Diagnostic::warning(
                    format!("Unknown field meta key '{other}'. Valid keys: label, min, max, default"),
                    span,
                )
                .with_code("E0201"),
            );
        }
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

    let Some(meta) = meta_node else { return; };
    let Some(meta_map) = meta.child_by_kind(kind::META_MAP) else { return; };

    let mut cursor = meta_map.walk();
    for child in meta_map.children(&mut cursor) {
        if child.kind() != kind::META_ENTRY { continue; }
        let Some(key_node) = child.child_by_field("key") else { continue; };
        validate_field_meta_entry(child, key_node.text(content), &field_type, diagnostics);
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
    let Some(meta_map) = node.child_by_kind(kind::META_MAP) else { return; };

    let mut cursor = meta_map.walk();
    for child in meta_map.children(&mut cursor) {
        if child.kind() != kind::META_ENTRY { continue; }
        let Some(key_node) = child.child_by_field("key") else { continue; };
        let key = key_node.text(content);
        if allowed.contains(&key) { continue; }

        diagnostics.push(
            Diagnostic::warning(
                format!("Unknown {context} meta key '{key}'. Valid keys: {}", allowed.join(", ")),
                nimbyscript_parser::ast::Span::new(child.start_byte(), child.end_byte()),
            )
            .with_code("E0201"),
        );
    }
}
