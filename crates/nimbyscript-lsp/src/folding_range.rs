//! Folding range support for NimbyScript.
//!
//! Provides folding ranges for:
//! - Struct definitions
//! - Enum definitions
//! - Function bodies
//! - Control flow blocks (if, for)
//! - Script meta blocks
//! - Meta maps

use nimbyscript_parser::{kind, Node};
use tower_lsp::lsp_types::FoldingRange;

use crate::document::Document;

/// Get all folding ranges for a document.
pub fn get_folding_ranges(doc: &Document) -> Vec<FoldingRange> {
    let mut ranges = Vec::new();
    let root = doc.tree().root_node();
    collect_folding_ranges(root, doc, &mut ranges);
    ranges
}

/// Recursively collect folding ranges from the tree.
fn collect_folding_ranges(node: Node, doc: &Document, ranges: &mut Vec<FoldingRange>) {
    match node.kind() {
        kind::SCRIPT_META | kind::STRUCT_DEFINITION | kind::ENUM_DEFINITION => {
            // Fold from opening brace to closing brace
            if let Some(range) = find_brace_fold_range(node, doc) {
                ranges.push(range);
            }
        }
        kind::FUNCTION_DEFINITION => {
            // Fold the function body block
            if let Some(body) = node.child_by_field_name("body") {
                if let Some(range) = node_to_fold_range(body, doc) {
                    ranges.push(range);
                }
            }
        }
        kind::BLOCK => {
            // Fold standalone blocks (but skip if parent already handles it)
            let parent_kind = node.parent().map(|p| p.kind());
            // Skip blocks that are direct children of constructs we handle specially
            if !matches!(
                parent_kind,
                Some(
                    kind::FUNCTION_DEFINITION
                        | kind::IF_STATEMENT
                        | kind::IF_LET_STATEMENT
                        | kind::FOR_STATEMENT
                        | kind::LET_ELSE_STATEMENT
                        | kind::ELSE_CLAUSE
                )
            ) {
                if let Some(range) = node_to_fold_range(node, doc) {
                    ranges.push(range);
                }
            }
        }
        kind::IF_STATEMENT | kind::IF_LET_STATEMENT => {
            // Fold the consequence block
            if let Some(consequence) = node.child_by_field_name("consequence") {
                if let Some(range) = node_to_fold_range(consequence, doc) {
                    ranges.push(range);
                }
            }
            // Fold else clause if present
            if let Some(else_clause) = node.child_by_field_name("alternative") {
                // The else clause contains a block
                if let Some(block) = else_clause.child_by_field_name("body") {
                    if let Some(range) = node_to_fold_range(block, doc) {
                        ranges.push(range);
                    }
                }
            }
        }
        kind::FOR_STATEMENT => {
            // Fold the loop body
            if let Some(body) = node.child_by_field_name("body") {
                if let Some(range) = node_to_fold_range(body, doc) {
                    ranges.push(range);
                }
            }
        }
        kind::LET_ELSE_STATEMENT => {
            // Fold the else block
            if let Some(else_block) = node.child_by_field_name("else_block") {
                if let Some(range) = node_to_fold_range(else_block, doc) {
                    ranges.push(range);
                }
            }
        }
        kind::META_MAP => {
            // Fold multi-line meta maps
            if let Some(range) = node_to_fold_range(node, doc) {
                ranges.push(range);
            }
        }
        _ => {}
    }

    // Recurse into children
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_folding_ranges(child, doc, ranges);
    }
}

/// Find the fold range for a node that uses braces (struct, enum, script_meta).
/// Returns a fold range from the opening brace line to the closing brace line.
fn find_brace_fold_range(node: Node, doc: &Document) -> Option<FoldingRange> {
    // Find opening and closing braces in the node
    let mut open_brace: Option<Node> = None;
    let mut close_brace: Option<Node> = None;

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == "{" {
            open_brace = Some(child);
        } else if child.kind() == "}" {
            close_brace = Some(child);
        }
    }

    let open = open_brace?;
    let close = close_brace?;

    let start_pos = doc.offset_to_position(open.start_byte());
    let end_pos = doc.offset_to_position(close.start_byte());

    // Only fold if spans multiple lines
    if start_pos.line >= end_pos.line {
        return None;
    }

    Some(FoldingRange {
        start_line: start_pos.line,
        start_character: Some(start_pos.character),
        end_line: end_pos.line,
        end_character: Some(end_pos.character),
        kind: None,
        collapsed_text: None,
    })
}

/// Convert a node (typically a block) to a fold range.
fn node_to_fold_range(node: Node, doc: &Document) -> Option<FoldingRange> {
    let start_pos = doc.offset_to_position(node.start_byte());
    let end_pos = doc.offset_to_position(node.end_byte().saturating_sub(1)); // End before closing brace

    // Only fold if spans multiple lines
    if start_pos.line >= end_pos.line {
        return None;
    }

    Some(FoldingRange {
        start_line: start_pos.line,
        start_character: Some(start_pos.character),
        end_line: end_pos.line,
        end_character: Some(end_pos.character),
        kind: None,
        collapsed_text: None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::make_doc;

    #[test]
    fn test_struct_folding() {
        let source = r"pub struct Test extend Signal {
    field1: i64,
    field2: f64,
}";
        let doc = make_doc(source);
        let ranges = get_folding_ranges(&doc);

        assert_eq!(ranges.len(), 1);
        assert_eq!(ranges[0].start_line, 0);
        assert_eq!(ranges[0].end_line, 3);
    }

    #[test]
    fn test_function_folding() {
        let source = r"fn test(): i64 {
    let x = 1;
    return x;
}";
        let doc = make_doc(source);
        let ranges = get_folding_ranges(&doc);

        assert_eq!(ranges.len(), 1);
        assert_eq!(ranges[0].start_line, 0);
        assert_eq!(ranges[0].end_line, 3);
    }

    #[test]
    fn test_enum_folding() {
        let source = r"enum Status {
    Active,
    Inactive,
}";
        let doc = make_doc(source);
        let ranges = get_folding_ranges(&doc);

        assert_eq!(ranges.len(), 1);
        assert_eq!(ranges[0].start_line, 0);
        assert_eq!(ranges[0].end_line, 3);
    }

    #[test]
    fn test_nested_blocks_folding() {
        let source = r"fn test(): i64 {
    if true {
        return 1;
    }
    return 0;
}";
        let doc = make_doc(source);
        let ranges = get_folding_ranges(&doc);

        // Function body + if block
        assert_eq!(ranges.len(), 2);
    }

    #[test]
    fn test_single_line_no_fold() {
        let source = "pub struct Empty extend Signal { }";
        let doc = make_doc(source);
        let ranges = get_folding_ranges(&doc);

        // Single-line structs should not fold
        assert!(ranges.is_empty());
    }

    #[test]
    fn test_script_meta_folding() {
        let source = r"script meta {
    lang: nimbyscript.v1,
    api: nimbyrails.v1,
}";
        let doc = make_doc(source);
        let ranges = get_folding_ranges(&doc);

        assert_eq!(ranges.len(), 1);
        assert_eq!(ranges[0].start_line, 0);
        assert_eq!(ranges[0].end_line, 3);
    }

    #[test]
    fn test_for_loop_folding() {
        let source = r"fn test() {
    for x in items {
        log x;
    }
}";
        let doc = make_doc(source);
        let ranges = get_folding_ranges(&doc);

        // Function body + for loop body
        assert_eq!(ranges.len(), 2);
    }
}
