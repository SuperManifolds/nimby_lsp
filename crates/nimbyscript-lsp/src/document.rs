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

    #[test]
    fn test_example_file_meta_validation() {
        let content = include_str!("../../../tests/fixtures/valid/example.nimbyscript");
        let doc = Document::new(content.to_string(), None);

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
}
