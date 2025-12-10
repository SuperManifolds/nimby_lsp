use std::collections::HashMap;
use tower_lsp::lsp_types::Position;

use nimbyscript_analyzer::{ApiDefinitions, Diagnostic, SymbolTable};
use nimbyscript_analyzer::diagnostics::Severity;
use nimbyscript_analyzer::symbols::{Symbol, SymbolKind};
use nimbyscript_parser::{parse, has_errors, kind, Node, NodeExt, Tree};

/// Document state containing parsed content and analysis results
pub struct Document {
    pub content: String,
    pub version: i32,
    line_offsets: Vec<usize>,
    diagnostics: Vec<Diagnostic>,
    symbols: SymbolTable,
    /// Maps struct names to the game type they extend (e.g., "ProbeCheck" -> "Signal")
    struct_extends: HashMap<String, String>,
    /// The parsed tree-sitter tree (kept for incremental updates)
    _tree: Tree,
}

impl Document {
    pub fn new(content: String, version: i32) -> Self {
        Self::new_with_api(content, version, None)
    }

    pub fn new_with_api(content: String, version: i32, api: Option<&ApiDefinitions>) -> Self {
        let line_offsets = compute_line_offsets(&content);
        let tree = parse(&content);
        let (diagnostics, symbols, struct_extends) = analyze(&content, &tree, api);

        Self {
            content,
            version,
            line_offsets,
            diagnostics,
            symbols,
            struct_extends,
            _tree: tree,
        }
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    pub fn document_symbols(&self) -> Vec<Symbol> {
        self.symbols.all_globals()
    }

    pub fn symbol_at(&self, offset: usize) -> Option<Symbol> {
        self.symbols.find_at_position(offset)
    }

    /// Get the game type a struct extends (e.g., "Signal" for a struct that extends Signal)
    pub fn struct_extends(&self, struct_name: &str) -> Option<&str> {
        self.struct_extends.get(struct_name).map(|s| s.as_str())
    }

    pub fn offset_to_position(&self, offset: usize) -> Position {
        let line = self
            .line_offsets
            .iter()
            .rposition(|&o| o <= offset)
            .unwrap_or(0);
        let character = offset - self.line_offsets[line];
        Position::new(line as u32, character as u32)
    }

    pub fn position_to_offset(&self, position: Position) -> usize {
        let line = position.line as usize;
        if line < self.line_offsets.len() {
            self.line_offsets[line] + position.character as usize
        } else {
            self.content.len()
        }
    }
}

fn compute_line_offsets(content: &str) -> Vec<usize> {
    let mut offsets = vec![0];
    for (i, c) in content.char_indices() {
        if c == '\n' {
            offsets.push(i + 1);
        }
    }
    offsets
}

fn analyze(content: &str, tree: &Tree, api: Option<&ApiDefinitions>) -> (Vec<Diagnostic>, SymbolTable, HashMap<String, String>) {
    let mut diagnostics = Vec::new();
    let symbols = SymbolTable::new();
    let mut struct_extends = HashMap::new();

    let root = tree.root_node();

    // Check for parse errors
    if has_errors(tree) {
        collect_errors(root, content, &mut diagnostics);
    }

    // Extract symbols and struct extends info
    extract_symbols(root, content, &symbols, &mut struct_extends);

    // Validate meta blocks
    validate_meta_blocks(root, content, &mut diagnostics);

    // Validate public functions if API is available
    if let Some(api) = api {
        validate_public_functions(root, content, api, &mut diagnostics);
    }

    (diagnostics, symbols, struct_extends)
}

fn collect_errors(node: Node, _content: &str, diagnostics: &mut Vec<Diagnostic>) {
    if node.is_error() {
        diagnostics.push(
            Diagnostic::error(
                "Syntax error".to_string(),
                nimbyscript_parser::ast::Span::new(node.start_byte(), node.end_byte()),
            )
            .with_code("E0001"),
        );
    }
    if node.is_missing() {
        diagnostics.push(
            Diagnostic::error(
                format!("Missing {}", node.kind()),
                nimbyscript_parser::ast::Span::new(node.start_byte(), node.end_byte()),
            )
            .with_code("E0002"),
        );
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_errors(child, _content, diagnostics);
    }
}

fn extract_symbols(
    node: Node,
    content: &str,
    symbols: &SymbolTable,
    struct_extends: &mut HashMap<String, String>,
) {
    match node.kind() {
        kind::STRUCT_DEFINITION => {
            // Find the struct name and extended type
            if let Some(name_node) = node.child_by_field("name") {
                let struct_name = name_node.text(content).to_string();
                let start = node.start_byte();
                let end = node.end_byte();

                symbols.add_global(Symbol::new(
                    struct_name.clone(),
                    SymbolKind::Struct,
                    nimbyscript_parser::ast::Span::new(start, end),
                ));

                // Check for extends clause
                if let Some(extends) = node.child_by_kind(kind::EXTENDS_CLAUSE) {
                    if let Some(type_node) = extends.child_by_field("type") {
                        let extends_type = type_node.text(content).to_string();
                        struct_extends.insert(struct_name, extends_type);
                    }
                }
            }

            // Recurse for fields
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                extract_symbols(child, content, symbols, struct_extends);
            }
        }
        kind::ENUM_DEFINITION => {
            if let Some(name_node) = node.child_by_field("name") {
                let name = name_node.text(content).to_string();
                symbols.add_global(Symbol::new(
                    name,
                    SymbolKind::Enum,
                    nimbyscript_parser::ast::Span::new(node.start_byte(), node.end_byte()),
                ));
            }
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                extract_symbols(child, content, symbols, struct_extends);
            }
        }
        kind::FUNCTION_DEFINITION => {
            if let Some(name_node) = node.child_by_field("name") {
                let name = name_node.text(content).to_string();
                let kind = if name.contains("::") {
                    SymbolKind::Method
                } else {
                    SymbolKind::Function
                };
                symbols.add_global(Symbol::new(
                    name,
                    kind,
                    nimbyscript_parser::ast::Span::new(node.start_byte(), node.end_byte()),
                ));
            }
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                extract_symbols(child, content, symbols, struct_extends);
            }
        }
        kind::CONST_DECLARATION => {
            if let Some(name_node) = node.child_by_field("name") {
                let name = name_node.text(content).to_string();
                symbols.add_global(Symbol::new(
                    name,
                    SymbolKind::Const,
                    nimbyscript_parser::ast::Span::new(node.start_byte(), node.end_byte()),
                ));
            }
        }
        kind::STRUCT_FIELD => {
            if let Some(name_node) = node.child_by_field("name") {
                let name = name_node.text(content).to_string();
                symbols.add_global(Symbol::new(
                    name,
                    SymbolKind::Field,
                    nimbyscript_parser::ast::Span::new(
                        name_node.start_byte(),
                        name_node.end_byte(),
                    ),
                ));
            }
        }
        kind::ENUM_VARIANT => {
            if let Some(name_node) = node.child_by_field("name") {
                let name = name_node.text(content).to_string();
                symbols.add_global(Symbol::new(
                    name,
                    SymbolKind::EnumVariant,
                    nimbyscript_parser::ast::Span::new(
                        name_node.start_byte(),
                        name_node.end_byte(),
                    ),
                ));
            }
        }
        _ => {
            // Recurse into other nodes
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                extract_symbols(child, content, symbols, struct_extends);
            }
        }
    }
}

/// Validate public function signatures against API callbacks
fn validate_public_functions(
    node: Node,
    content: &str,
    api: &ApiDefinitions,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match node.kind() {
        kind::FUNCTION_DEFINITION => {
            validate_fn_decl(node, content, api, diagnostics);
        }
        _ => {
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                validate_public_functions(child, content, api, diagnostics);
            }
        }
    }
}

fn validate_fn_decl(
    node: Node,
    content: &str,
    api: &ApiDefinitions,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let mut is_pub = false;
    let mut fn_name_full = String::new();
    let mut fn_name_start = node.start_byte();
    let mut fn_name_end = node.end_byte();
    let mut params: Vec<(String, String)> = Vec::new();
    let mut return_type: Option<String> = None;

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            kind::VISIBILITY_MODIFIER => {
                is_pub = true;
            }
            kind::FUNCTION_NAME => {
                fn_name_full = child.text(content).to_string();
                fn_name_start = child.start_byte();
                fn_name_end = child.end_byte();
            }
            kind::PARAMETERS => {
                let mut param_cursor = child.walk();
                for param in child.children(&mut param_cursor) {
                    if param.kind() == kind::PARAMETER {
                        let mut param_name = String::new();
                        let mut param_type = String::new();
                        if let Some(name_node) = param.child_by_field("name") {
                            param_name = name_node.text(content).to_string();
                        }
                        if let Some(type_node) = param.child_by_field("type") {
                            param_type = type_node.text(content).to_string();
                        }
                        params.push((param_name, param_type));
                    }
                }
            }
            kind::TYPE => {
                // This is the return type (field name is "return_type")
                if node.child_by_field("return_type").map(|n| n.id()) == Some(child.id()) {
                    return_type = Some(child.text(content).to_string());
                }
            }
            _ => {}
        }
    }

    // Only validate public methods (functions with :: in name)
    if !is_pub {
        return;
    }

    // Extract method name (part after ::)
    let method_name = if let Some(pos) = fn_name_full.find("::") {
        &fn_name_full[pos + 2..]
    } else {
        // Public standalone function - also needs to be a valid callback
        &fn_name_full
    };

    // Check if it's a valid callback
    let callback = api.get_callback(method_name);
    if callback.is_none() {
        let valid_names: Vec<_> = api.callback_names().collect();
        diagnostics.push(
            Diagnostic::error(
                format!(
                    "'{}' is not a valid game callback. Valid callbacks are: {}",
                    method_name,
                    valid_names.join(", ")
                ),
                nimbyscript_parser::ast::Span::new(fn_name_start, fn_name_end),
            )
            .with_code("E0100"),
        );
        return;
    }

    let callback = callback.unwrap();

    // Validate parameter count
    let expected_params: Vec<_> = callback.params.iter().collect();
    if params.len() != expected_params.len() {
        diagnostics.push(
            Diagnostic::error(
                format!(
                    "'{}' expects {} parameters, but {} were provided",
                    method_name,
                    expected_params.len(),
                    params.len()
                ),
                nimbyscript_parser::ast::Span::new(fn_name_start, fn_name_end),
            )
            .with_code("E0101"),
        );
        return;
    }

    // Validate parameter names and types
    for (i, ((param_name, param_type), expected)) in params.iter().zip(expected_params.iter()).enumerate() {
        // Check parameter name (skip 'self' name check - any name is fine for self)
        let expected_name = &expected.name;
        if expected_name != "self" && param_name != expected_name {
            diagnostics.push(
                Diagnostic::error(
                    format!(
                        "Parameter {} of '{}' is named '{}', expected '{}'",
                        i + 1,
                        method_name,
                        param_name,
                        expected_name
                    ),
                    nimbyscript_parser::ast::Span::new(fn_name_start, fn_name_end),
                )
                .with_code("E0104"),
            );
        }

        // Check parameter type
        let expected_type = &expected.ty;
        let param_type_normalized = param_type.trim().replace(' ', "");
        let expected_type_normalized = expected_type.trim().replace(' ', "");

        // &Self matches any &TypeName for the self parameter
        let types_match = if expected_type_normalized == "&Self" {
            param_type_normalized.starts_with('&')
        } else {
            param_type_normalized == expected_type_normalized
        };

        if !types_match {
            diagnostics.push(
                Diagnostic::error(
                    format!(
                        "Parameter {} of '{}' has type '{}', expected '{}'",
                        i + 1,
                        method_name,
                        param_type.trim(),
                        expected_type
                    ),
                    nimbyscript_parser::ast::Span::new(fn_name_start, fn_name_end),
                )
                .with_code("E0102"),
            );
        }
    }

    // Validate return type
    let expected_return = callback.return_type.as_deref();
    match (&return_type, expected_return) {
        (Some(actual), Some(expected)) => {
            let actual_normalized = actual.trim().replace(' ', "");
            let expected_normalized = expected.trim().replace(' ', "");
            if actual_normalized != expected_normalized {
                diagnostics.push(
                    Diagnostic::error(
                        format!(
                            "'{}' should return '{}', but returns '{}'",
                            method_name, expected, actual
                        ),
                        nimbyscript_parser::ast::Span::new(fn_name_start, fn_name_end),
                    )
                    .with_code("E0103"),
                );
            }
        }
        (None, Some(expected)) => {
            diagnostics.push(
                Diagnostic::error(
                    format!("'{}' should return '{}'", method_name, expected),
                    nimbyscript_parser::ast::Span::new(fn_name_start, fn_name_end),
                )
                .with_code("E0103"),
            );
        }
        (Some(actual), None) => {
            diagnostics.push(
                Diagnostic::error(
                    format!(
                        "'{}' should not have a return type, but returns '{}'",
                        method_name, actual
                    ),
                    nimbyscript_parser::ast::Span::new(fn_name_start, fn_name_end),
                )
                .with_code("E0103"),
            );
        }
        (None, None) => {} // Both have no return type, OK
    }
}

/// Validate meta blocks according to wiki specification
fn validate_meta_blocks(
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

/// Validate script-level meta (requires lang and api)
fn validate_script_meta(node: Node, content: &str, diagnostics: &mut Vec<Diagnostic>) {
    let mut has_lang = false;
    let mut has_api = false;

    // Find the meta block within script_meta
    if let Some(meta_block) = node.child_by_kind(kind::META_BLOCK) {
        if let Some(meta_map) = meta_block.child_by_kind(kind::META_MAP) {
            let mut cursor = meta_map.walk();
            for child in meta_map.children(&mut cursor) {
                if child.kind() == kind::META_ENTRY {
                    if let Some(key_node) = child.child_by_field("key") {
                        let key = key_node.text(content);
                        match key {
                            "lang" => has_lang = true,
                            "api" => has_api = true,
                            "description" => {} // Valid optional field
                            other => {
                                diagnostics.push(
                                    Diagnostic::warning(
                                        format!("Unknown script meta field '{}'", other),
                                        nimbyscript_parser::ast::Span::new(child.start_byte(), child.end_byte()),
                                    )
                                    .with_code("E0201"),
                                );
                            }
                        }
                    }
                }
            }
        }
    }

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

/// Validate field meta based on field type
fn validate_field_meta(node: Node, content: &str, diagnostics: &mut Vec<Diagnostic>) {
    let mut field_type = String::new();
    let mut meta_node: Option<Node> = None;

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            kind::TYPE => {
                field_type = child.text(content).to_string();
            }
            kind::META_BLOCK => {
                meta_node = Some(child);
            }
            _ => {}
        }
    }

    if let Some(meta) = meta_node {
        let is_numeric = field_type == "i64" || field_type == "f64";
        let is_bool = field_type == "bool";

        if let Some(meta_map) = meta.child_by_kind(kind::META_MAP) {
            let mut cursor = meta_map.walk();
            for child in meta_map.children(&mut cursor) {
                if child.kind() == kind::META_ENTRY {
                    if let Some(key_node) = child.child_by_field("key") {
                        let key = key_node.text(content);

                        match key {
                            "label" => {} // Valid for all types
                            "min" | "max" => {
                                if !is_numeric {
                                    diagnostics.push(
                                        Diagnostic::error(
                                            format!("'{}' is only valid for numeric types (i64, f64), not '{}'", key, field_type),
                                            nimbyscript_parser::ast::Span::new(child.start_byte(), child.end_byte()),
                                        )
                                        .with_code("E0202"),
                                    );
                                }
                            }
                            "default" => {
                                // Valid for bool, i64, f64, and enum types
                                if !is_numeric && !is_bool && !field_type.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
                                    diagnostics.push(
                                        Diagnostic::warning(
                                            format!("'default' may not be valid for type '{}'", field_type),
                                            nimbyscript_parser::ast::Span::new(child.start_byte(), child.end_byte()),
                                        )
                                        .with_code("E0203"),
                                    );
                                }
                            }
                            other => {
                                diagnostics.push(
                                    Diagnostic::warning(
                                        format!("Unknown field meta key '{}'. Valid keys: label, min, max, default", other),
                                        nimbyscript_parser::ast::Span::new(child.start_byte(), child.end_byte()),
                                    )
                                    .with_code("E0201"),
                                );
                            }
                        }
                    }
                }
            }
        }
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
    if let Some(meta_map) = node.child_by_kind(kind::META_MAP) {
        let mut cursor = meta_map.walk();
        for child in meta_map.children(&mut cursor) {
            if child.kind() == kind::META_ENTRY {
                if let Some(key_node) = child.child_by_field("key") {
                    let key = key_node.text(content);
                    if !allowed.contains(&key) {
                        diagnostics.push(
                            Diagnostic::warning(
                                format!(
                                    "Unknown {} meta key '{}'. Valid keys: {}",
                                    context,
                                    key,
                                    allowed.join(", ")
                                ),
                                nimbyscript_parser::ast::Span::new(child.start_byte(), child.end_byte()),
                            )
                            .with_code("E0201"),
                        );
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_example_file_meta_validation() {
        let content = include_str!("../../../tests/fixtures/valid/example.nimbyscript");
        let doc = Document::new_with_api(content.to_string(), 1, None);

        println!("\nMeta validation diagnostics ({}):", doc.diagnostics().len());
        for d in doc.diagnostics() {
            println!("  {:?}: {}", d.severity, d.message);
        }

        // The example file should have no errors
        let errors: Vec<_> = doc.diagnostics().iter()
            .filter(|d| matches!(d.severity, Severity::Error))
            .collect();

        assert!(errors.is_empty(), "Example file should have no errors, but found: {:?}", errors);
    }

    #[test]
    fn test_struct_extends_tracking() {
        let content = include_str!("../../../tests/fixtures/valid/example.nimbyscript");
        let doc = Document::new_with_api(content.to_string(), 1, None);

        // Verify struct extends are tracked correctly
        assert_eq!(doc.struct_extends("ProbeCheck"), Some("Signal"));
        assert_eq!(doc.struct_extends("UpcomingChange"), Some("Signal"));
        assert_eq!(doc.struct_extends("YeetAtSignal"), Some("Train"));
        assert_eq!(doc.struct_extends("HateDepots"), Some("Train"));
        assert_eq!(doc.struct_extends("PasserBy"), Some("Signal"));
        assert_eq!(doc.struct_extends("MarkerReserved"), Some("Signal"));
        assert_eq!(doc.struct_extends("Globals"), Some("Script"));

        // Non-extending struct (Task) should return None
        assert_eq!(doc.struct_extends("Task"), None);
    }

    #[test]
    fn test_error_recovery_struct_extends() {
        // Test that struct extends work even with incomplete code
        let content = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal { }
pub fn Test::
"#;
        let doc = Document::new_with_api(content.to_string(), 1, None);

        // Should still track struct extends even with the incomplete function
        assert_eq!(doc.struct_extends("Test"), Some("Signal"));
    }
}
