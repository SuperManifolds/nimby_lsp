use std::collections::HashMap;
use tower_lsp::lsp_types::Position;

use nimbyscript_analyzer::{ApiDefinitions, Diagnostic, SymbolTable};
use nimbyscript_analyzer::symbols::{Symbol, SymbolKind};
use nimbyscript_parser::{parse, has_errors, kind, Node, NodeExt, Tree};

use crate::validation::{validate_meta_blocks, validate_public_functions};

/// Document state containing parsed content and analysis results
pub struct Document {
    pub content: String,
    line_offsets: Vec<usize>,
    diagnostics: Vec<Diagnostic>,
    symbols: SymbolTable,
    /// Maps struct names to the game type they extend (e.g., "ProbeCheck" -> "Signal")
    struct_extends: HashMap<String, String>,
    /// The parsed tree-sitter tree (kept for incremental updates)
    _tree: Tree,
}

impl Document {
    pub fn new(content: String, api: Option<&ApiDefinitions>) -> Self {
        let line_offsets = compute_line_offsets(&content);
        let tree = parse(&content);
        let (diagnostics, symbols, struct_extends) = analyze(&content, &tree, api);

        Self {
            content,
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
        self.struct_extends.get(struct_name).map(String::as_str)
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
        collect_errors(root, &mut diagnostics);
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

fn collect_errors(node: Node, diagnostics: &mut Vec<Diagnostic>) {
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
        collect_errors(child, diagnostics);
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
                    SymbolKind::STRUCT,
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
                    SymbolKind::ENUM,
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
                    SymbolKind::METHOD
                } else {
                    SymbolKind::FUNCTION
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
                    SymbolKind::CONSTANT,
                    nimbyscript_parser::ast::Span::new(node.start_byte(), node.end_byte()),
                ));
            }
        }
        kind::STRUCT_FIELD => {
            if let Some(name_node) = node.child_by_field("name") {
                let name = name_node.text(content).to_string();
                symbols.add_global(Symbol::new(
                    name,
                    SymbolKind::FIELD,
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
                    SymbolKind::ENUM_MEMBER,
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

#[cfg(test)]
mod tests {
    use super::*;
    use nimbyscript_analyzer::diagnostics::Severity;

    // Existing tests

    #[test]
    fn test_example_file_meta_validation() {
        let content = include_str!("../../../tests/fixtures/valid/example.nimbyscript");
        let doc = Document::new(content.to_string(), None);

        // The example file should have no errors
        let errors: Vec<_> = doc.diagnostics().iter()
            .filter(|d| matches!(d.severity, Severity::Error))
            .collect();

        assert!(errors.is_empty(), "Example file should have no errors, but found: {:?}", errors);
    }

    #[test]
    fn test_struct_extends_tracking() {
        let content = include_str!("../../../tests/fixtures/valid/example.nimbyscript");
        let doc = Document::new(content.to_string(), None);

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
        let doc = Document::new(content.to_string(), None);

        // Should still track struct extends even with the incomplete function
        assert_eq!(doc.struct_extends("Test"), Some("Signal"));
    }

    // Line offset computation tests

    #[test]
    fn test_line_offsets_empty() {
        let offsets = compute_line_offsets("");
        assert_eq!(offsets, vec![0]);
    }

    #[test]
    fn test_line_offsets_single_line() {
        let offsets = compute_line_offsets("hello world");
        assert_eq!(offsets, vec![0]);
    }

    #[test]
    fn test_line_offsets_multiple_lines() {
        let offsets = compute_line_offsets("line1\nline2\nline3");
        // line1 at 0, line2 at 6, line3 at 12
        assert_eq!(offsets, vec![0, 6, 12]);
    }

    #[test]
    fn test_line_offsets_trailing_newline() {
        let offsets = compute_line_offsets("line1\n");
        assert_eq!(offsets, vec![0, 6]);
    }

    #[test]
    fn test_line_offsets_empty_lines() {
        let offsets = compute_line_offsets("a\n\nb");
        // a at 0, empty at 2, b at 3
        assert_eq!(offsets, vec![0, 2, 3]);
    }

    // Position conversion tests

    #[test]
    fn test_offset_to_position_start() {
        let doc = Document::new("hello\nworld".to_string(), None);
        let pos = doc.offset_to_position(0);
        assert_eq!(pos.line, 0);
        assert_eq!(pos.character, 0);
    }

    #[test]
    fn test_offset_to_position_middle_first_line() {
        let doc = Document::new("hello\nworld".to_string(), None);
        let pos = doc.offset_to_position(3);
        assert_eq!(pos.line, 0);
        assert_eq!(pos.character, 3);
    }

    #[test]
    fn test_offset_to_position_second_line() {
        let doc = Document::new("hello\nworld".to_string(), None);
        // "world" starts at offset 6
        let pos = doc.offset_to_position(6);
        assert_eq!(pos.line, 1);
        assert_eq!(pos.character, 0);
    }

    #[test]
    fn test_offset_to_position_middle_second_line() {
        let doc = Document::new("hello\nworld".to_string(), None);
        // 'r' in "world" is at offset 8
        let pos = doc.offset_to_position(8);
        assert_eq!(pos.line, 1);
        assert_eq!(pos.character, 2);
    }

    #[test]
    fn test_offset_to_position_end() {
        let doc = Document::new("hello\nworld".to_string(), None);
        let pos = doc.offset_to_position(11); // end of "world"
        assert_eq!(pos.line, 1);
        assert_eq!(pos.character, 5);
    }

    #[test]
    fn test_position_to_offset_start() {
        let doc = Document::new("hello\nworld".to_string(), None);
        let offset = doc.position_to_offset(Position::new(0, 0));
        assert_eq!(offset, 0);
    }

    #[test]
    fn test_position_to_offset_second_line() {
        let doc = Document::new("hello\nworld".to_string(), None);
        let offset = doc.position_to_offset(Position::new(1, 0));
        assert_eq!(offset, 6);
    }

    #[test]
    fn test_position_to_offset_roundtrip() {
        let doc = Document::new("line1\nline2\nline3".to_string(), None);
        for offset in [0, 3, 5, 6, 10, 12, 15] {
            let pos = doc.offset_to_position(offset);
            let back = doc.position_to_offset(pos);
            assert_eq!(offset, back, "roundtrip failed for offset {offset}");
        }
    }

    #[test]
    fn test_position_to_offset_beyond_lines() {
        let doc = Document::new("hello".to_string(), None);
        // Position beyond available lines should return content length
        let offset = doc.position_to_offset(Position::new(10, 0));
        assert_eq!(offset, 5); // content length
    }

    // Symbol extraction tests

    #[test]
    fn test_extract_struct_symbol() {
        let content = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct MyStruct { }
"#;
        let doc = Document::new(content.to_string(), None);
        let symbols = doc.document_symbols();
        let struct_sym = symbols.iter().find(|s| s.name == "MyStruct");
        assert!(struct_sym.is_some(), "Should find MyStruct symbol");
        assert_eq!(struct_sym.expect("checked").kind, SymbolKind::STRUCT);
    }

    #[test]
    fn test_extract_enum_symbol() {
        let content = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub enum MyEnum { A, B, }
"#;
        let doc = Document::new(content.to_string(), None);
        let symbols = doc.document_symbols();
        let enum_sym = symbols.iter().find(|s| s.name == "MyEnum");
        assert!(enum_sym.is_some(), "Should find MyEnum symbol");
        assert_eq!(enum_sym.expect("checked").kind, SymbolKind::ENUM);
    }

    #[test]
    fn test_extract_function_symbol() {
        let content = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn my_func() { }
"#;
        let doc = Document::new(content.to_string(), None);
        let symbols = doc.document_symbols();
        let fn_sym = symbols.iter().find(|s| s.name == "my_func");
        assert!(fn_sym.is_some(), "Should find my_func symbol");
        assert_eq!(fn_sym.expect("checked").kind, SymbolKind::FUNCTION);
    }

    #[test]
    fn test_extract_method_symbol() {
        let content = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Foo { }
fn Foo::bar(&self) { }
"#;
        let doc = Document::new(content.to_string(), None);
        let symbols = doc.document_symbols();
        let method_sym = symbols.iter().find(|s| s.name == "Foo::bar");
        assert!(method_sym.is_some(), "Should find Foo::bar method");
        assert_eq!(method_sym.expect("checked").kind, SymbolKind::METHOD);
    }

    #[test]
    fn test_extract_const_symbol() {
        let content = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
const MY_CONST: i64 = 42;
"#;
        let doc = Document::new(content.to_string(), None);
        let symbols = doc.document_symbols();
        let const_sym = symbols.iter().find(|s| s.name == "MY_CONST");
        assert!(const_sym.is_some(), "Should find MY_CONST symbol");
        assert_eq!(const_sym.expect("checked").kind, SymbolKind::CONSTANT);
    }

    #[test]
    fn test_extract_field_symbol() {
        let content = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Foo {
    my_field: i64,
}
"#;
        let doc = Document::new(content.to_string(), None);
        let symbols = doc.document_symbols();
        let field_sym = symbols.iter().find(|s| s.name == "my_field");
        assert!(field_sym.is_some(), "Should find my_field symbol");
        assert_eq!(field_sym.expect("checked").kind, SymbolKind::FIELD);
    }

    #[test]
    fn test_extract_enum_variant_symbol() {
        let content = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub enum Color { Red, Green, Blue, }
"#;
        let doc = Document::new(content.to_string(), None);
        let symbols = doc.document_symbols();
        let variant = symbols.iter().find(|s| s.name == "Red");
        assert!(variant.is_some(), "Should find Red enum variant");
        assert_eq!(variant.expect("checked").kind, SymbolKind::ENUM_MEMBER);
    }

    #[test]
    fn test_symbol_at_position() {
        let content = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Foo { }
"#;
        let doc = Document::new(content.to_string(), None);

        // Find offset where "Foo" is defined
        let foo_offset = content.find("Foo").expect("Foo should be in content");
        let sym = doc.symbol_at(foo_offset);
        // Should find the struct since Foo is part of the struct definition span
        assert!(sym.is_some());
    }

    // Parse error detection tests

    #[test]
    fn test_parse_error_detected() {
        let content = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Foo {
"#; // Missing closing brace
        let doc = Document::new(content.to_string(), None);
        let errors: Vec<_> = doc.diagnostics().iter()
            .filter(|d| matches!(d.severity, Severity::Error))
            .collect();
        assert!(!errors.is_empty(), "Should detect parse error");
    }

    #[test]
    fn test_empty_document() {
        let doc = Document::new(String::new(), None);
        // Empty document should have some diagnostics (missing script meta)
        assert!(!doc.diagnostics().is_empty());
    }

    #[test]
    fn test_minimal_valid_document() {
        let content = include_str!("../../../tests/fixtures/valid/minimal.nimbyscript");
        let doc = Document::new(content.to_string(), None);
        let errors: Vec<_> = doc.diagnostics().iter()
            .filter(|d| matches!(d.severity, Severity::Error))
            .collect();
        assert!(errors.is_empty(), "Minimal file should have no errors");
    }
}
