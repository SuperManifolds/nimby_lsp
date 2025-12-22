//! Selection range support for NimbyScript.
//!
//! Provides smart selection expansion through syntactic units:
//! identifier → expression → statement → block → definition → file

use nimbyscript_parser::Node;
use tower_lsp::lsp_types::{Position, Range, SelectionRange};

use crate::document::Document;
use crate::type_inference::{find_deepest_node_at, node_to_range};

/// Get selection ranges for a list of positions.
///
/// For each position, returns a linked list of `SelectionRange` objects
/// from smallest (innermost) to largest (outermost) enclosing range.
pub fn get_selection_ranges(doc: &Document, positions: Vec<Position>) -> Vec<SelectionRange> {
    positions
        .into_iter()
        .map(|pos| build_selection_range(doc, pos))
        .collect()
}

/// Build a selection range chain for a single position.
fn build_selection_range(doc: &Document, position: Position) -> SelectionRange {
    let offset = doc.position_to_offset(position);
    let root = doc.tree().root_node();

    // Find the deepest node at the cursor position
    let deepest = find_deepest_node_at(root, offset);

    match deepest {
        Some(node) => build_range_chain(doc, node),
        None => {
            // Fallback to entire document if no node found
            SelectionRange {
                range: node_to_range(doc, root),
                parent: None,
            }
        }
    }
}

/// Build a linked list of selection ranges from a node up to the root.
///
/// Walks up the parent chain, creating nested SelectionRange objects.
/// Skips nodes with identical ranges to avoid redundant expansion steps.
fn build_range_chain(doc: &Document, node: Node) -> SelectionRange {
    let range = node_to_range(doc, node);

    // Walk up parent chain to build the linked list
    let parent = build_parent_chain(doc, node.parent(), range);

    SelectionRange { range, parent }
}

/// Recursively build the parent chain, skipping nodes with identical ranges.
fn build_parent_chain(
    doc: &Document,
    parent_node: Option<Node>,
    child_range: Range,
) -> Option<Box<SelectionRange>> {
    let parent = parent_node?;
    let parent_range = node_to_range(doc, parent);

    // Skip this parent if it has the same range as the child
    // (avoids redundant expansion steps)
    if parent_range == child_range {
        return build_parent_chain(doc, parent.parent(), child_range);
    }

    // Build the parent's chain recursively
    let grandparent = build_parent_chain(doc, parent.parent(), parent_range);

    Some(Box::new(SelectionRange {
        range: parent_range,
        parent: grandparent,
    }))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::make_doc;

    #[test]
    fn test_single_position() {
        let source = r"fn test() {
    let x = 42;
}";
        let doc = make_doc(source);
        // Position inside "42"
        let position = Position::new(1, 12);
        let ranges = get_selection_ranges(&doc, vec![position]);

        assert_eq!(ranges.len(), 1);
        // Should have a chain of ranges
        let first = &ranges[0];
        assert!(first.parent.is_some(), "Should have parent ranges");
    }

    #[test]
    fn test_multiple_positions() {
        let source = r"fn test() {
    let x = 1;
    let y = 2;
}";
        let doc = make_doc(source);
        let positions = vec![Position::new(1, 12), Position::new(2, 12)];
        let ranges = get_selection_ranges(&doc, positions);

        assert_eq!(ranges.len(), 2);
    }

    #[test]
    fn test_expansion_hierarchy() {
        let source = r"fn test() {
    let x = 42;
}";
        let doc = make_doc(source);
        // Position on "x"
        let position = Position::new(1, 8);
        let ranges = get_selection_ranges(&doc, vec![position]);

        // Count the chain depth
        let mut depth = 1;
        let mut current = &ranges[0];
        while let Some(ref parent) = current.parent {
            depth += 1;
            current = parent;
        }

        // Should expand through: identifier -> binding -> let_statement -> block -> function -> source_file
        assert!(depth >= 4, "Expected at least 4 levels, got {depth}");
    }

    #[test]
    fn test_struct_field() {
        let source = r"pub struct Test extend Signal {
    field1: i64,
}";
        let doc = make_doc(source);
        // Position on "field1"
        let position = Position::new(1, 4);
        let ranges = get_selection_ranges(&doc, vec![position]);

        assert!(!ranges.is_empty());
        // Should have parent chain up to struct and source file
        assert!(ranges[0].parent.is_some());
    }

    #[test]
    fn test_no_duplicate_ranges() {
        let source = r"fn test() {
    return 42;
}";
        let doc = make_doc(source);
        let position = Position::new(1, 11);
        let ranges = get_selection_ranges(&doc, vec![position]);

        // Verify no adjacent ranges are identical
        let mut current = &ranges[0];
        while let Some(ref parent) = current.parent {
            assert_ne!(
                current.range, parent.range,
                "Adjacent ranges should not be identical"
            );
            current = parent;
        }
    }

    #[test]
    fn test_outermost_is_source_file() {
        let source = r"fn test() {
    let x = 1;
}";
        let doc = make_doc(source);
        let position = Position::new(1, 8);
        let ranges = get_selection_ranges(&doc, vec![position]);

        // Walk to outermost
        let mut current = &ranges[0];
        while let Some(ref parent) = current.parent {
            current = parent;
        }

        // Outermost should cover entire document
        assert_eq!(current.range.start.line, 0);
        assert_eq!(current.range.start.character, 0);
    }
}
