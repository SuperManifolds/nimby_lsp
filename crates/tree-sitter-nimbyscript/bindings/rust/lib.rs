//! Tree-sitter grammar for NimbyScript

use tree_sitter::Language;

extern "C" {
    fn tree_sitter_nimbyscript() -> Language;
}

/// Returns the tree-sitter Language for NimbyScript.
pub fn language() -> Language {
    unsafe { tree_sitter_nimbyscript() }
}

/// The tree-sitter query for syntax highlighting.
pub const HIGHLIGHTS_QUERY: &str = include_str!("../../queries/highlights.scm");

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_can_load_grammar() {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&language())
            .expect("Failed to load NimbyScript grammar");
    }

    #[test]
    fn test_parse_simple() {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&language())
            .expect("Failed to load grammar");

        let source = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }";
        let tree = parser.parse(source, None).expect("Failed to parse");

        assert_eq!(tree.root_node().kind(), "source_file");
        assert!(!tree.root_node().has_error());
    }

    #[test]
    fn test_error_recovery() {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&language())
            .expect("Failed to load grammar");

        // Incomplete code - struct should still parse
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal { }
pub fn Test::
";
        let tree = parser.parse(source, None).expect("Failed to parse");
        let root = tree.root_node();

        // Should have parsed the struct despite the incomplete function
        let mut found_struct = false;
        let mut cursor = root.walk();
        for child in root.children(&mut cursor) {
            if child.kind() == "struct_definition" {
                found_struct = true;
            }
        }
        assert!(found_struct, "Struct should be parsed despite error");
    }
}
