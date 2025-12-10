use std::fmt::Write;
use tower_lsp::lsp_types::*;

use nimbyscript_analyzer::{ApiDefinitions, FunctionDef};

use crate::document::Document;

/// NimbyScript keywords
const KEYWORDS: &[&str] = &[
    "script", "meta", "const", "struct", "enum", "fn", "pub", "extend",
    "let", "mut", "if", "else", "for", "in", "return", "break", "continue",
    "true", "false", "log",
];

/// Primitive types
const PRIMITIVE_TYPES: &[&str] = &[
    "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f32", "f64", "bool",
];

// ============================================================================
// Helper functions for formatting function completions
// ============================================================================

/// Format function signature for display in detail field
fn format_signature(func: &FunctionDef) -> String {
    let params = func.params.iter()
        .map(|p| format!("{}: {}", p.name, p.ty))
        .collect::<Vec<_>>()
        .join(", ");

    match &func.return_type {
        Some(ret) => format!("({params}) -> {ret}"),
        None => format!("({params})"),
    }
}

/// Generate snippet with named parameter placeholders
fn format_snippet(name: &str, func: &FunctionDef) -> String {
    if func.params.is_empty() {
        return format!("{name}()");
    }

    let placeholders = func.params.iter()
        .enumerate()
        .map(|(i, p)| format!("${{{}:{}}}", i + 1, p.name))
        .collect::<Vec<_>>()
        .join(", ");

    format!("{name}({placeholders})")
}

/// Format documentation with parameter details
fn format_documentation(func: &FunctionDef) -> Option<Documentation> {
    let mut doc = func.doc.clone().unwrap_or_default();

    if !func.params.is_empty() {
        if !doc.is_empty() {
            doc.push_str("\n\n");
        }
        doc.push_str("**Parameters:**\n");
        for p in &func.params {
            let _ = write!(doc, "- `{}`: `{}`", p.name, p.ty);
            if let Some(param_doc) = &p.doc {
                let _ = write!(doc, " — {param_doc}");
            }
            doc.push('\n');
        }
    }

    if let Some(ret) = &func.return_type {
        let _ = write!(doc, "\n**Returns:** `{ret}`");
    }

    if doc.is_empty() {
        None
    } else {
        Some(Documentation::MarkupContent(MarkupContent {
            kind: MarkupKind::Markdown,
            value: doc,
        }))
    }
}

pub fn get_completions(
    doc: &Document,
    position: Position,
    api: &ApiDefinitions,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    let offset = doc.position_to_offset(position);
    let prefix = get_prefix(&doc.content, offset);

    // Check context for smarter completions
    let context = get_completion_context(&doc.content, offset);

    match context {
        CompletionContext::Type => {
            // Type context - offer types
            add_primitive_types(&mut items, &prefix);
            add_api_types(&mut items, api, &prefix);
        }
        CompletionContext::PathAfterColon(name) => {
            // After :: - offer module members, enum variants, or struct callbacks
            add_module_members(&mut items, api, &name, &prefix);
            add_struct_callbacks(&mut items, doc, api, &name, &prefix);
        }
        CompletionContext::FieldAccess => {
            // After . - would need type info for smart completions
            // For now, just return empty
        }
        CompletionContext::General => {
            // General context - offer everything
            add_keywords(&mut items, &prefix);
            add_primitive_types(&mut items, &prefix);
            add_api_completions(&mut items, api, &prefix);
            add_document_symbols(&mut items, doc, &prefix);
        }
    }

    items
}

#[derive(Debug)]
enum CompletionContext {
    Type,
    PathAfterColon(String),
    FieldAccess,
    General,
}

fn get_completion_context(content: &str, offset: usize) -> CompletionContext {
    let before = &content[..offset];

    // Check if we're after :: (either immediately or with partial identifier)
    // e.g., "ProbeCheck::" or "ProbeCheck::ev"
    if let Some(colon_pos) = before.rfind("::") {
        // Check if everything after :: is a valid identifier prefix (or empty)
        let after_colon = &before[colon_pos + 2..];
        if after_colon.is_empty() || after_colon.chars().all(|c| c.is_alphanumeric() || c == '_') {
            // Find the module/struct name before ::
            let before_colon = &before[..colon_pos];
            let start = before_colon
                .rfind(|c: char| !c.is_alphanumeric() && c != '_')
                .map_or(0, |i| i + 1);
            let module = before_colon[start..].to_string();
            if !module.is_empty() {
                return CompletionContext::PathAfterColon(module);
            }
        }
    }

    // Check if we're after .
    if before.ends_with('.') {
        return CompletionContext::FieldAccess;
    }

    // Check if we're in a type annotation context
    if before.ends_with(": ") || before.ends_with(":<") || before.ends_with(", ") {
        // Could be a type context
        let trimmed = before.trim();
        if trimmed.ends_with(':') || trimmed.contains("->") {
            return CompletionContext::Type;
        }
    }

    CompletionContext::General
}

fn get_prefix(content: &str, offset: usize) -> String {
    let before = &content[..offset];
    let start = before
        .rfind(|c: char| !c.is_alphanumeric() && c != '_')
        .map_or(0, |i| i + 1);
    before[start..].to_string()
}

fn add_keywords(items: &mut Vec<CompletionItem>, prefix: &str) {
    for &keyword in KEYWORDS {
        if keyword.starts_with(prefix) {
            items.push(CompletionItem {
                label: (*keyword).to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                detail: Some("keyword".to_string()),
                ..Default::default()
            });
        }
    }
}

fn add_primitive_types(items: &mut Vec<CompletionItem>, prefix: &str) {
    for &ty in PRIMITIVE_TYPES {
        if ty.starts_with(prefix) {
            items.push(CompletionItem {
                label: (*ty).to_string(),
                kind: Some(CompletionItemKind::TYPE_PARAMETER),
                detail: Some("primitive type".to_string()),
                ..Default::default()
            });
        }
    }

    // Add ID<T> generic
    if "ID".starts_with(prefix) {
        items.push(CompletionItem {
            label: "ID".to_string(),
            kind: Some(CompletionItemKind::TYPE_PARAMETER),
            detail: Some("generic type".to_string()),
            insert_text: Some("ID<$1>".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        });
    }
}


fn add_api_types(items: &mut Vec<CompletionItem>, api: &ApiDefinitions, prefix: &str) {
    for name in api.type_names() {
        if name.starts_with(prefix) {
            let type_def = api.get_type(name);
            items.push(CompletionItem {
                label: name.to_string(),
                kind: Some(CompletionItemKind::STRUCT),
                detail: Some("game type".to_string()),
                documentation: type_def.and_then(|t| t.doc.clone()).map(Documentation::String),
                ..Default::default()
            });
        }
    }

    for name in api.enum_names() {
        if name.starts_with(prefix) {
            let enum_def = api.get_enum(name);
            items.push(CompletionItem {
                label: name.to_string(),
                kind: Some(CompletionItemKind::ENUM),
                detail: Some("game enum".to_string()),
                documentation: enum_def.and_then(|e| e.doc.clone()).map(Documentation::String),
                ..Default::default()
            });
        }
    }
}

fn add_api_completions(items: &mut Vec<CompletionItem>, api: &ApiDefinitions, prefix: &str) {
    add_api_types(items, api, prefix);

    // Add modules
    for name in api.module_names() {
        if name.starts_with(prefix) {
            let module_def = api.get_module(name);
            items.push(CompletionItem {
                label: name.to_string(),
                kind: Some(CompletionItemKind::MODULE),
                detail: Some("module".to_string()),
                documentation: module_def.and_then(|m| m.doc.clone()).map(Documentation::String),
                ..Default::default()
            });
        }
    }

    // Add global functions from API (includes stdlib)
    for name in api.function_names() {
        if name.starts_with(prefix) {
            if let Some(func) = api.get_function(name) {
                items.push(CompletionItem {
                    label: name.to_string(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    detail: Some(format!("fn {}", format_signature(func))),
                    documentation: format_documentation(func),
                    insert_text: Some(format_snippet(name, func)),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    ..Default::default()
                });
            }
        }
    }
}

fn add_module_members(
    items: &mut Vec<CompletionItem>,
    api: &ApiDefinitions,
    module: &str,
    prefix: &str,
) {
    // Check if it's a module
    if let Some(module_def) = api.get_module(module) {
        for func in &module_def.functions {
            if func.name.starts_with(prefix) {
                items.push(CompletionItem {
                    label: func.name.clone(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    detail: Some(format_signature(func)),
                    documentation: format_documentation(func),
                    insert_text: Some(format_snippet(&func.name, func)),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    ..Default::default()
                });
            }
        }
    }

    // Check if it's a type with methods (e.g., DB, Sim, Extrapolator, Signal, etc.)
    if let Some(type_def) = api.get_type(module) {
        for method in &type_def.methods {
            if method.name.starts_with(prefix) {
                items.push(CompletionItem {
                    label: method.name.clone(),
                    kind: Some(CompletionItemKind::METHOD),
                    detail: Some(format_signature(method)),
                    documentation: format_documentation(method),
                    insert_text: Some(format_snippet(&method.name, method)),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    ..Default::default()
                });
            }
        }
    }

    // Check if it's an enum
    if let Some(enum_def) = api.get_enum(module) {
        for variant in &enum_def.variants {
            if variant.name.starts_with(prefix) {
                items.push(CompletionItem {
                    label: variant.name.clone(),
                    kind: Some(CompletionItemKind::ENUM_MEMBER),
                    detail: Some(format!("{module}::{}", variant.name)),
                    documentation: variant.doc.clone().map(Documentation::String),
                    ..Default::default()
                });
            }
        }
    }
}

/// Add callback completions for user-defined structs that extend game types
fn add_struct_callbacks(
    items: &mut Vec<CompletionItem>,
    doc: &Document,
    api: &ApiDefinitions,
    struct_name: &str,
    prefix: &str,
) {
    // Check if this is a user-defined struct that extends a game type
    if let Some(extends_type) = doc.struct_extends(struct_name) {
        // Get callbacks that apply to this game type
        for callback in api.callbacks_for_type(extends_type) {
            if callback.name.starts_with(prefix) {
                // Format callback signature for this struct (replace &Self with &StructName)
                let signature = format_callback_signature(callback, struct_name);
                items.push(CompletionItem {
                    label: callback.name.clone(),
                    kind: Some(CompletionItemKind::METHOD),
                    detail: Some(format!("callback {signature}")),
                    documentation: format_callback_documentation(callback, struct_name),
                    insert_text: Some(format_callback_snippet(callback, struct_name)),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    ..Default::default()
                });
            }
        }
    }
}

/// Format callback signature with struct name instead of Self
fn format_callback_signature(func: &FunctionDef, struct_name: &str) -> String {
    let params = func.params.iter()
        .map(|p| {
            let ty = if p.ty == "&Self" {
                format!("&{struct_name}")
            } else {
                p.ty.clone()
            };
            format!("{}: {ty}", p.name)
        })
        .collect::<Vec<_>>()
        .join(", ");

    match &func.return_type {
        Some(ret) => format!("({params}) -> {ret}"),
        None => format!("({params})"),
    }
}

/// Generate callback snippet with full function signature
fn format_callback_snippet(func: &FunctionDef, struct_name: &str) -> String {
    let params = func.params.iter()
        .enumerate()
        .map(|(i, p)| {
            let ty = if p.ty == "&Self" {
                format!("&{struct_name}")
            } else {
                p.ty.clone()
            };
            format!("${{{}:{}: {ty}}}", i + 1, p.name)
        })
        .collect::<Vec<_>>()
        .join(", ");

    match &func.return_type {
        Some(ret) => format!("{}({params})$0: {ret} {{\n\t\n}}", func.name),
        None => format!("{}({params})$0 {{\n\t\n}}", func.name),
    }
}

/// Format callback documentation
fn format_callback_documentation(func: &FunctionDef, struct_name: &str) -> Option<Documentation> {
    let mut doc = func.doc.clone().unwrap_or_default();

    if !func.params.is_empty() {
        if !doc.is_empty() {
            doc.push_str("\n\n");
        }
        doc.push_str("**Parameters:**\n");
        for p in &func.params {
            let ty = if p.ty == "&Self" {
                format!("&{struct_name}")
            } else {
                p.ty.clone()
            };
            let _ = write!(doc, "- `{}`: `{ty}`", p.name);
            if let Some(param_doc) = &p.doc {
                let _ = write!(doc, " — {param_doc}");
            }
            doc.push('\n');
        }
    }

    if let Some(ret) = &func.return_type {
        let _ = write!(doc, "\n**Returns:** `{ret}`");
    }

    if doc.is_empty() {
        None
    } else {
        Some(Documentation::MarkupContent(MarkupContent {
            kind: MarkupKind::Markdown,
            value: doc,
        }))
    }
}

fn symbol_to_completion_kind(kind: SymbolKind) -> CompletionItemKind {
    match kind {
        SymbolKind::CONSTANT => CompletionItemKind::CONSTANT,
        SymbolKind::STRUCT => CompletionItemKind::STRUCT,
        SymbolKind::ENUM => CompletionItemKind::ENUM,
        SymbolKind::ENUM_MEMBER => CompletionItemKind::ENUM_MEMBER,
        SymbolKind::FUNCTION => CompletionItemKind::FUNCTION,
        SymbolKind::METHOD => CompletionItemKind::METHOD,
        SymbolKind::VARIABLE => CompletionItemKind::VARIABLE,
        SymbolKind::FIELD => CompletionItemKind::FIELD,
        _ => CompletionItemKind::VALUE,
    }
}

fn add_document_symbols(items: &mut Vec<CompletionItem>, doc: &Document, prefix: &str) {
    for symbol in doc.document_symbols() {
        if symbol.name.starts_with(prefix) {
            items.push(CompletionItem {
                label: symbol.name.clone(),
                kind: Some(symbol_to_completion_kind(symbol.kind)),
                detail: symbol.type_name.clone(),
                ..Default::default()
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn load_api() -> ApiDefinitions {
        let toml = include_str!("../../../api-definitions/nimbyrails.v1.toml");
        ApiDefinitions::load_from_str(toml).expect("should parse")
    }

    // Context detection tests

    #[test]
    fn test_context_type_after_colon() {
        // After `: ` in a type annotation
        let ctx = get_completion_context("let x: ", 7);
        assert!(matches!(ctx, CompletionContext::Type));
    }

    #[test]
    fn test_context_path_after_double_colon() {
        // After `Module::` or `Enum::`
        let ctx = get_completion_context("SignalCheck::", 13);
        assert!(matches!(ctx, CompletionContext::PathAfterColon(name) if name == "SignalCheck"));
    }

    #[test]
    fn test_context_path_with_partial() {
        // After `Module::par` (partial identifier)
        let ctx = get_completion_context("Math::ab", 8);
        assert!(matches!(ctx, CompletionContext::PathAfterColon(name) if name == "Math"));
    }

    #[test]
    fn test_context_field_after_dot() {
        let ctx = get_completion_context("foo.", 4);
        assert!(matches!(ctx, CompletionContext::FieldAccess));
    }

    #[test]
    fn test_context_general() {
        let ctx = get_completion_context("let x = ", 8);
        assert!(matches!(ctx, CompletionContext::General));
    }

    // Prefix extraction tests

    #[test]
    fn test_prefix_word_start() {
        let prefix = get_prefix("let sig", 7);
        assert_eq!(prefix, "sig");
    }

    #[test]
    fn test_prefix_mid_word() {
        let prefix = get_prefix("SignalChe", 9);
        assert_eq!(prefix, "SignalChe");
    }

    #[test]
    fn test_prefix_empty() {
        let prefix = get_prefix("let ", 4);
        assert_eq!(prefix, "");
    }

    #[test]
    fn test_prefix_after_operator() {
        let prefix = get_prefix("x + y", 5);
        assert_eq!(prefix, "y");
    }

    // Completion provider tests

    #[test]
    fn test_keyword_completions() {
        let mut items = Vec::new();
        add_keywords(&mut items, "if");
        assert!(!items.is_empty());
        assert!(items.iter().any(|i| i.label == "if"));
    }

    #[test]
    fn test_keyword_filter_by_prefix() {
        let mut items = Vec::new();
        add_keywords(&mut items, "ret");
        // Should only have "return"
        assert_eq!(items.len(), 1);
        assert_eq!(items[0].label, "return");
    }

    #[test]
    fn test_keyword_no_match() {
        let mut items = Vec::new();
        add_keywords(&mut items, "xyz");
        assert!(items.is_empty());
    }

    #[test]
    fn test_primitive_type_completions() {
        let mut items = Vec::new();
        add_primitive_types(&mut items, "i");
        // Should have i8, i16, i32, i64
        assert!(items.iter().any(|i| i.label == "i64"));
        assert!(items.iter().any(|i| i.label == "i32"));
    }

    #[test]
    fn test_id_generic_completion() {
        let mut items = Vec::new();
        add_primitive_types(&mut items, "ID");
        assert!(items.iter().any(|i| i.label == "ID"));
        // Should have snippet
        let id_item = items.iter().find(|i| i.label == "ID").expect("ID should exist");
        assert!(id_item.insert_text.as_ref().is_some_and(|t| t.contains("<")));
    }

    #[test]
    fn test_api_type_completions() {
        let api = load_api();
        let mut items = Vec::new();
        add_api_types(&mut items, &api, "Sig");
        // Should have Signal, SignalCheck, SignalAspect, etc.
        assert!(items.iter().any(|i| i.label == "Signal"));
    }

    #[test]
    fn test_type_method_completions() {
        let api = load_api();
        let mut items = Vec::new();
        add_module_members(&mut items, &api, "DB", "");
        // DB type should have methods (like view)
        assert!(!items.is_empty(), "DB should have methods");
        assert!(items.iter().any(|i| i.label == "view"), "DB should have view method");
    }

    #[test]
    fn test_enum_variant_completions() {
        let api = load_api();
        let mut items = Vec::new();
        add_module_members(&mut items, &api, "SignalCheck", "");
        // SignalCheck enum should have Pass, Stop variants
        assert!(items.iter().any(|i| i.label == "Pass"));
        assert!(items.iter().any(|i| i.label == "Stop"));
    }

    #[test]
    fn test_struct_callback_completions() {
        let api = load_api();
        let content = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct MySignal extend Signal { }
"#;
        let doc = Document::new(content.to_string(), Some(&api));
        let mut items = Vec::new();
        add_struct_callbacks(&mut items, &doc, &api, "MySignal", "");
        // Should have Signal callbacks like event_signal_check
        assert!(items.iter().any(|i| i.label == "event_signal_check"));
    }

    #[test]
    fn test_document_symbol_completions() {
        let api = load_api();
        let content = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct UserStruct { }
fn user_function() { }
"#;
        let doc = Document::new(content.to_string(), Some(&api));
        let mut items = Vec::new();
        add_document_symbols(&mut items, &doc, "");
        // Should have user-defined symbols
        assert!(items.iter().any(|i| i.label == "UserStruct"));
        assert!(items.iter().any(|i| i.label == "user_function"));
    }

    // Snippet formatting tests

    #[test]
    fn test_snippet_with_params() {
        let func = FunctionDef {
            name: "test".to_string(),
            params: vec![
                nimbyscript_analyzer::ParamDef {
                    name: "a".to_string(),
                    ty: "i64".to_string(),
                    doc: None,
                    is_mut: false,
                    is_ref: false,
                },
                nimbyscript_analyzer::ParamDef {
                    name: "b".to_string(),
                    ty: "f64".to_string(),
                    doc: None,
                    is_mut: false,
                    is_ref: false,
                },
            ],
            return_type: None,
            doc: None,
            type_params: vec![],
            for_type: None,
        };
        let snippet = format_snippet("test", &func);
        assert_eq!(snippet, "test(${1:a}, ${2:b})");
    }

    #[test]
    fn test_snippet_no_params() {
        let func = FunctionDef {
            name: "empty".to_string(),
            params: vec![],
            return_type: None,
            doc: None,
            type_params: vec![],
            for_type: None,
        };
        let snippet = format_snippet("empty", &func);
        assert_eq!(snippet, "empty()");
    }

    #[test]
    fn test_format_signature() {
        let func = FunctionDef {
            name: "add".to_string(),
            params: vec![
                nimbyscript_analyzer::ParamDef {
                    name: "a".to_string(),
                    ty: "i64".to_string(),
                    doc: None,
                    is_mut: false,
                    is_ref: false,
                },
            ],
            return_type: Some("i64".to_string()),
            doc: None,
            type_params: vec![],
            for_type: None,
        };
        let sig = format_signature(&func);
        assert_eq!(sig, "(a: i64) -> i64");
    }

    // Integration test

    #[test]
    fn test_get_completions_general_context() {
        let api = load_api();
        let content = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct MyStruct { }
"#;
        let doc = Document::new(content.to_string(), Some(&api));
        let items = get_completions(&doc, Position::new(2, 0), &api);
        // Should have keywords, types, functions, and document symbols
        assert!(items.iter().any(|i| i.kind == Some(CompletionItemKind::KEYWORD)));
        assert!(items.iter().any(|i| i.kind == Some(CompletionItemKind::STRUCT)));
    }
}
