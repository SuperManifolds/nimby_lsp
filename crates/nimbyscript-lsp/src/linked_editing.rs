//! Linked editing range support for NimbyScript.
//!
//! Provides simultaneous editing of related symbol occurrences,
//! primarily for struct-method pairs where methods use `StructName::method_name` syntax.

use tower_lsp::lsp_types::{LinkedEditingRanges, Position, Range};

use nimbyscript_parser::{kind, Node, NodeExt};

use crate::document::Document;
use crate::type_inference::{find_ancestor_of_kind, find_deepest_node_at, node_to_range};

/// Get linked editing ranges for a position.
///
/// Returns ranges that should be edited together, primarily for:
/// - Struct definition name
/// - Method name prefixes (`StructName::method_name`)
/// - Self parameter type annotations (`self: &StructName`)
pub fn get_linked_editing_ranges(
    doc: &Document,
    position: Position,
) -> Option<LinkedEditingRanges> {
    let offset = doc.position_to_offset(position);
    let root = doc.tree().root_node();
    let content = &doc.content;

    // Find the node at the cursor position
    let node = find_deepest_node_at(root, offset)?;

    // Try to find a struct name to link
    let struct_name = find_struct_name_at_cursor(node, content)?;

    // Collect all ranges where this struct name appears
    let ranges = collect_struct_name_ranges(root, &struct_name, doc);

    if ranges.len() <= 1 {
        // No point in linked editing for a single occurrence
        return None;
    }

    Some(LinkedEditingRanges {
        ranges,
        word_pattern: None,
    })
}

/// Find the struct name if the cursor is on one.
///
/// Handles:
/// - Struct definition name
/// - Method name prefix (the `Foo` in `Foo::method`)
/// - Type identifier in type annotations
fn find_struct_name_at_cursor<'a>(node: Node<'a>, content: &'a str) -> Option<String> {
    // Case 1: Cursor is on struct definition name
    if node.kind() == kind::IDENTIFIER || node.kind() == kind::TYPE_IDENTIFIER {
        if let Some(struct_name) = try_get_struct_definition_name(node, content) {
            return Some(struct_name);
        }
    }

    // Case 2: Cursor is on function name with struct prefix (e.g., "Foo::bar")
    if node.kind() == kind::FUNCTION_NAME {
        let name = node.text(content);
        if let Some(pos) = name.find("::") {
            return Some(name[..pos].to_string());
        }
    }

    // Case 3: Cursor is inside a function_name node on the struct part
    if let Some(func_name_node) = find_ancestor_of_kind(node, kind::FUNCTION_NAME) {
        let name = func_name_node.text(content);
        if let Some(pos) = name.find("::") {
            let struct_part = &name[..pos];
            // Check if cursor is actually on the struct part
            let struct_end = func_name_node.start_byte() + pos;
            if node.start_byte() < struct_end {
                return Some(struct_part.to_string());
            }
        }
    }

    // Case 4: Cursor is on a type identifier (e.g., in `self: &StructName`)
    // The type_identifier node contains an identifier child, so check both
    if node.kind() == kind::TYPE_IDENTIFIER || node.kind() == kind::IDENTIFIER {
        // Check if this node or its parent is a type_identifier
        let is_type_context = node.kind() == kind::TYPE_IDENTIFIER
            || node
                .parent()
                .is_some_and(|p| p.kind() == kind::TYPE_IDENTIFIER);

        if is_type_context {
            let type_name = node.text(content);
            // Check if this is a user-defined struct (not a built-in type)
            if type_name.chars().next().is_some_and(char::is_uppercase) {
                return Some(type_name.to_string());
            }
        }
    }

    None
}

/// Try to extract struct name if this node is the name of a struct definition.
fn try_get_struct_definition_name<'a>(node: Node<'a>, content: &'a str) -> Option<String> {
    let parent = node.parent()?;
    if parent.kind() != kind::STRUCT_DEFINITION {
        return None;
    }
    let name_node = parent.child_by_field("name")?;
    if name_node.id() == node.id() {
        return Some(node.text(content).to_string());
    }
    None
}

/// Collect all ranges where the struct name appears in linkable positions.
fn collect_struct_name_ranges(root: Node, struct_name: &str, doc: &Document) -> Vec<Range> {
    let mut ranges = Vec::new();
    collect_struct_name_ranges_recursive(root, struct_name, doc, &mut ranges);
    ranges
}

fn collect_struct_name_ranges_recursive(
    node: Node,
    struct_name: &str,
    doc: &Document,
    ranges: &mut Vec<Range>,
) {
    let content = &doc.content;

    match node.kind() {
        kind::STRUCT_DEFINITION => {
            // Struct definition name
            if let Some(name_node) = node.child_by_field("name") {
                if name_node.text(content) == struct_name {
                    ranges.push(node_to_range(doc, name_node));
                }
            }
        }
        kind::FUNCTION_DEFINITION => {
            // Method name prefix (Foo::method_name -> just the "Foo" part)
            if let Some(name_node) = node.child_by_field("name") {
                let name = name_node.text(content);
                if let Some(pos) = name.find("::") {
                    if &name[..pos] == struct_name {
                        // Create a range for just the struct name part
                        let start = doc.offset_to_position(name_node.start_byte());
                        let end = doc.offset_to_position(name_node.start_byte() + pos);
                        ranges.push(Range::new(start, end));
                    }
                }
            }
        }
        kind::TYPE_IDENTIFIER => {
            // Type references (e.g., in parameters like `self: &StructName`)
            if node.text(content) == struct_name {
                ranges.push(node_to_range(doc, node));
            }
        }
        _ => {}
    }

    // Recurse into children
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_struct_name_ranges_recursive(child, struct_name, doc, ranges);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::make_doc;

    #[test]
    fn test_struct_definition_links_to_methods() {
        let source = r"pub struct Test extend Signal {
    field: i64,
}

pub fn Test::method1(self: &Test): i64 {
    return 0;
}

pub fn Test::method2(self: &Test): bool {
    return true;
}";
        let doc = make_doc(source);
        // Position on "Test" in struct definition
        let position = Position::new(0, 11);
        let result = get_linked_editing_ranges(&doc, position);

        let ranges = result.expect("should find linked ranges").ranges;
        // Should find: struct def, method1 prefix, method1 self type, method2 prefix, method2 self type
        assert_eq!(ranges.len(), 5);
    }

    #[test]
    fn test_method_prefix_links_to_struct() {
        let source = r"pub struct Foo extend Signal { }

pub fn Foo::bar(self: &Foo): i64 {
    return 0;
}";
        let doc = make_doc(source);
        // Position on "Foo" in method name
        let position = Position::new(2, 7);
        let result = get_linked_editing_ranges(&doc, position);

        let ranges = result.expect("should find linked ranges").ranges;
        // struct def + method prefix + self type = 3
        assert_eq!(ranges.len(), 3);
    }

    #[test]
    fn test_self_type_links_to_struct() {
        let source = r"pub struct Bar extend Signal { }

pub fn Bar::method(self: &Bar): i64 {
    return 0;
}";
        let doc = make_doc(source);
        // Position on "Bar" in self parameter type (after '&')
        // Line: "pub fn Bar::method(self: &Bar): i64 {"
        // The 'B' in '&Bar' is at character 26
        let position = Position::new(2, 26);
        let result = get_linked_editing_ranges(&doc, position);

        let ranges = result.expect("should find linked ranges").ranges;
        // struct def + method prefix + self type = 3
        assert_eq!(ranges.len(), 3);
    }

    #[test]
    fn test_no_links_for_single_occurrence() {
        let source = r"pub struct Lonely extend Signal { }";
        let doc = make_doc(source);
        let position = Position::new(0, 11);
        let result = get_linked_editing_ranges(&doc, position);

        // Single occurrence should return None
        assert!(result.is_none());
    }

    #[test]
    fn test_no_links_for_non_struct_identifier() {
        let source = r"fn test() {
    let x = 42;
    return x;
}";
        let doc = make_doc(source);
        // Position on "x"
        let position = Position::new(1, 8);
        let result = get_linked_editing_ranges(&doc, position);

        // Local variables don't get linked editing
        assert!(result.is_none());
    }
}
