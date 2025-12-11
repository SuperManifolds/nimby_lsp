pub mod ast;

pub use tree_sitter::{Node, Tree, TreeCursor};
pub use tree_sitter_nimbyscript::{language, HIGHLIGHTS_QUERY};

/// Parse a NimbyScript source file
///
/// Unlike the previous Pest parser, this returns a tree-sitter Tree which:
/// - Always succeeds (tree-sitter has error recovery)
/// - Contains ERROR nodes for unparseable sections
/// - Can be incrementally updated
pub fn parse(source: &str) -> Tree {
    let mut parser = tree_sitter::Parser::new();
    parser.set_language(&language()).expect("Failed to load NimbyScript grammar");
    parser.parse(source, None).expect("Parser returned None")
}

/// Check if the parsed tree has any errors
pub fn has_errors(tree: &Tree) -> bool {
    has_errors_in_node(tree.root_node())
}

fn has_errors_in_node(node: Node) -> bool {
    if node.is_error() || node.is_missing() {
        return true;
    }
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if has_errors_in_node(child) {
            return true;
        }
    }
    false
}

/// Tree-sitter node kinds that map to the old Pest Rule names.
/// Use these constants with `node.kind()` for pattern matching.
pub mod kind {
    // Top-level
    pub const SOURCE_FILE: &str = "source_file";
    pub const SCRIPT_META: &str = "script_meta";
    pub const STRUCT_DEFINITION: &str = "struct_definition";
    pub const ENUM_DEFINITION: &str = "enum_definition";
    pub const FUNCTION_DEFINITION: &str = "function_definition";
    pub const CONST_DECLARATION: &str = "const_declaration";

    // Struct parts
    pub const EXTENDS_CLAUSE: &str = "extends_clause";
    pub const STRUCT_FIELD: &str = "struct_field";
    pub const VISIBILITY_MODIFIER: &str = "visibility_modifier";

    // Enum parts
    pub const ENUM_VARIANT: &str = "enum_variant";

    // Function parts
    pub const FUNCTION_NAME: &str = "function_name";
    pub const PARAMETERS: &str = "parameters";
    pub const PARAMETER: &str = "parameter";
    pub const BLOCK: &str = "block";

    // Meta
    pub const META_BLOCK: &str = "meta_block";
    pub const META_ENTRY: &str = "meta_entry";
    pub const META_NAME: &str = "meta_name";
    pub const META_VALUE: &str = "meta_value";
    pub const META_MAP: &str = "meta_map";
    pub const META_ARRAY: &str = "meta_array";

    // Types
    pub const TYPE: &str = "type";
    pub const TYPE_IDENTIFIER: &str = "type_identifier";
    pub const STORAGE_MODIFIER: &str = "storage_modifier";
    pub const MUTABILITY_MODIFIER: &str = "mutability_modifier";
    pub const GENERIC_ARGUMENTS: &str = "generic_arguments";

    // Expressions
    pub const IDENTIFIER: &str = "identifier";
    pub const PATH_EXPRESSION: &str = "path_expression";
    pub const CALL_EXPRESSION: &str = "call_expression";
    pub const FIELD_ACCESS: &str = "field_access";
    pub const BINARY_EXPRESSION: &str = "binary_expression";
    pub const UNARY_EXPRESSION: &str = "unary_expression";
    pub const PARENTHESIZED_EXPRESSION: &str = "parenthesized_expression";

    // Literals
    pub const NUMBER: &str = "number";
    pub const BOOLEAN: &str = "boolean";
    pub const STRING_LITERAL: &str = "string_literal";
    pub const TIME_LITERAL: &str = "time_literal";

    // Statements
    pub const LET_STATEMENT: &str = "let_statement";
    pub const LET_ELSE_STATEMENT: &str = "let_else_statement";
    pub const ASSIGNMENT_STATEMENT: &str = "assignment_statement";
    pub const IF_STATEMENT: &str = "if_statement";
    pub const IF_LET_STATEMENT: &str = "if_let_statement";
    pub const ELSE_CLAUSE: &str = "else_clause";
    pub const FOR_STATEMENT: &str = "for_statement";
    pub const RETURN_STATEMENT: &str = "return_statement";
    pub const BREAK_STATEMENT: &str = "break_statement";
    pub const CONTINUE_STATEMENT: &str = "continue_statement";
    pub const LOG_STATEMENT: &str = "log_statement";
    pub const EXPRESSION_STATEMENT: &str = "expression_statement";

    // Other
    pub const COMMENT: &str = "comment";
    pub const ERROR: &str = "ERROR";
}

/// Helper extension trait for tree-sitter Node
pub trait NodeExt<'a> {
    /// Get the text content of this node from the source
    fn text(&self, source: &'a str) -> &'a str;

    /// Find first child with the given kind
    fn child_by_kind(&self, kind: &str) -> Option<Node<'a>>;

    /// Find child with the given field name
    fn child_by_field(&self, field: &str) -> Option<Node<'a>>;

    /// Iterate over all children with the given kind
    fn children_by_kind(&self, kind: &str) -> Vec<Node<'a>>;
}

impl<'a> NodeExt<'a> for Node<'a> {
    fn text(&self, source: &'a str) -> &'a str {
        self.utf8_text(source.as_bytes()).unwrap_or("")
    }

    #[allow(clippy::manual_find)]
    fn child_by_kind(&self, kind: &str) -> Option<Node<'a>> {
        let mut cursor = self.walk();
        for child in self.children(&mut cursor) {
            if child.kind() == kind {
                return Some(child);
            }
        }
        None
    }

    fn child_by_field(&self, field: &str) -> Option<Node<'a>> {
        self.child_by_field_name(field)
    }

    fn children_by_kind(&self, kind: &str) -> Vec<Node<'a>> {
        let mut cursor = self.walk();
        self.children(&mut cursor)
            .filter(|child| child.kind() == kind)
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_meta_block() {
        let source = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }";
        let tree = parse(source);
        assert_eq!(tree.root_node().kind(), "source_file");
        assert!(!has_errors(&tree));
    }

    #[test]
    fn test_parse_struct() {
        let source = r"
            pub struct TestStruct extend Signal {
                field1: i64,
                field2: f64,
            }
        ";
        let tree = parse(source);
        assert!(!has_errors(&tree));
    }

    #[test]
    fn test_parse_enum() {
        let source = r"
            enum TestEnum {
                Option1,
                Option2,
                Option3,
            }
        ";
        let tree = parse(source);
        assert!(!has_errors(&tree));
    }

    #[test]
    fn test_parse_function() {
        let source = r"
            fn test_function(x: i64, y: f64): bool {
                let result = x + y;
                return result > 0;
            }
        ";
        let tree = parse(source);
        assert!(!has_errors(&tree));
    }

    #[test]
    fn test_parse_method() {
        let source = r"
            pub fn TestStruct::method_name(self: &TestStruct, ctx: &Context): bool {
                return true;
            }
        ";
        let tree = parse(source);
        assert!(!has_errors(&tree));
    }

    #[test]
    fn test_error_recovery() {
        // Incomplete function should still allow struct to be parsed
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal { }
pub fn Test::
";
        let tree = parse(source);

        // Tree should have errors but still parsed the struct
        let root = tree.root_node();
        let mut found_struct = false;
        let mut cursor = root.walk();
        for child in root.children(&mut cursor) {
            if child.kind() == kind::STRUCT_DEFINITION {
                found_struct = true;
                // Verify we can get the struct name
                if let Some(name_node) = child.child_by_field("name") {
                    assert_eq!(name_node.text(source), "Test");
                }
                // Verify we can get the extends clause
                if let Some(extends) = child.child_by_kind(kind::EXTENDS_CLAUSE) {
                    let type_node = extends.child_by_field("type");
                    assert_eq!(type_node.map(|n| n.text(source)), Some("Signal"));
                }
            }
        }
        assert!(found_struct, "Struct should be parsed despite error");
    }

    #[test]
    fn test_parse_minimal_file() {
        let source = include_str!("../../../tests/fixtures/valid/minimal.nimbyscript");
        let tree = parse(source);
        assert!(!has_errors(&tree), "minimal.nimbyscript should parse without errors");
    }

    #[test]
    fn test_parse_example_file() {
        let source = include_str!("../../../tests/fixtures/valid/example.nimbyscript");
        let tree = parse(source);
        assert!(!has_errors(&tree), "example.nimbyscript should parse without errors");
    }
}
