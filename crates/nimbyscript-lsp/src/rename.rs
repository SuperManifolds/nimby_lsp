//! Rename support for NimbyScript LSP.
//!
//! Provides symbol renaming across the document for:
//! - Local variables and function parameters
//! - User-defined struct names (including method prefixes and type annotations)
//! - User-defined enum names and variants
//! - Function names
//! - Struct fields

use std::collections::HashMap;

use tower_lsp::lsp_types::{Position, PrepareRenameResponse, Range, TextEdit, Url, WorkspaceEdit};

use nimbyscript_analyzer::{collect_declarations, ApiDefinitions, SemanticContext};
use nimbyscript_parser::{kind, Node, NodeExt};

use crate::document::Document;
use crate::type_inference::{
    cursor_in_node, find_ancestor_of_kind, find_deepest_node_at, find_enclosing_function,
    node_to_range,
};

// ============================================================================
// Public API
// ============================================================================

/// Validate that rename is possible and return the range to rename.
///
/// Returns `None` if the symbol at the position cannot be renamed (e.g., API symbols).
pub fn prepare_rename(
    doc: &Document,
    position: Position,
    api: &ApiDefinitions,
) -> Option<PrepareRenameResponse> {
    let engine = RenameEngine::new(doc, api, position);
    engine.prepare_rename()
}

/// Perform the rename, returning all text edits.
///
/// Returns `None` if the symbol cannot be renamed.
pub fn rename(
    doc: &Document,
    position: Position,
    uri: &Url,
    new_name: &str,
    api: &ApiDefinitions,
) -> Option<WorkspaceEdit> {
    let engine = RenameEngine::new(doc, api, position);
    engine.rename(uri, new_name)
}

// ============================================================================
// Rename Engine
// ============================================================================

/// Engine for computing rename edits.
struct RenameEngine<'a> {
    doc: &'a Document,
    content: &'a str,
    api: &'a ApiDefinitions,
    offset: usize,
    /// User-defined structs (name -> extends type)
    user_structs: HashMap<String, Option<String>>,
    /// User-defined enums (name -> variants)
    user_enums: HashMap<String, Vec<String>>,
    /// User-defined functions (name -> return type)
    user_functions: HashMap<String, Option<nimbyscript_analyzer::TypeInfo>>,
}

impl<'a> RenameEngine<'a> {
    fn new(doc: &'a Document, api: &'a ApiDefinitions, position: Position) -> Self {
        let offset = doc.position_to_offset(position);

        // Build semantic context to collect declarations
        let mut ctx = SemanticContext::new(&doc.content, doc.tree(), api);
        collect_declarations(&mut ctx);

        Self {
            doc,
            content: &doc.content,
            api,
            offset,
            user_structs: ctx.user_structs.clone(),
            user_enums: ctx.user_enums.clone(),
            user_functions: ctx.user_functions.clone(),
        }
    }

    /// Check if rename is possible and return the range/placeholder.
    fn prepare_rename(&self) -> Option<PrepareRenameResponse> {
        let root = self.doc.tree().root_node();
        let node = find_deepest_node_at(root, self.offset)?;

        // Get the symbol info at cursor
        let (name, range) = self.get_renameable_symbol(node)?;

        // Check if it's an API symbol (cannot rename)
        if self.is_api_symbol(&name) {
            return None;
        }

        Some(PrepareRenameResponse::RangeWithPlaceholder {
            range,
            placeholder: name,
        })
    }

    /// Perform the rename and return workspace edits.
    fn rename(&self, uri: &Url, new_name: &str) -> Option<WorkspaceEdit> {
        let root = self.doc.tree().root_node();
        let node = find_deepest_node_at(root, self.offset)?;

        // Get the symbol info at cursor
        let (name, _) = self.get_renameable_symbol(node)?;

        // Check if it's an API symbol (cannot rename)
        if self.is_api_symbol(&name) {
            return None;
        }

        // Collect all ranges to rename
        let ranges = self.collect_rename_ranges(&name, node);

        if ranges.is_empty() {
            return None;
        }

        // Convert ranges to text edits
        let edits: Vec<TextEdit> = ranges
            .into_iter()
            .map(|range| TextEdit {
                range,
                new_text: new_name.to_string(),
            })
            .collect();

        let mut changes = HashMap::new();
        changes.insert(uri.clone(), edits);

        Some(WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        })
    }

    /// Get the renameable symbol at cursor, returning (name, range).
    fn get_renameable_symbol(&self, node: Node) -> Option<(String, Range)> {
        // Case 1: Regular identifier
        if node.kind() == kind::IDENTIFIER {
            let name = node.text(self.content).to_string();
            let range = node_to_range(self.doc, node);
            return Some((name, range));
        }

        // Case 2: Struct definition name
        if let Some(result) = self.try_get_definition_name(node, "struct_definition") {
            return Some(result);
        }

        // Case 3: Enum definition name
        if let Some(result) = self.try_get_definition_name(node, "enum_definition") {
            return Some(result);
        }

        // Case 4: Function name (handle method names specially)
        if let Some(result) = self.try_get_function_name(node) {
            return Some(result);
        }

        // Case 5: Type identifier (in type annotations)
        if node.kind() == kind::TYPE_IDENTIFIER
            || (node.kind() == kind::IDENTIFIER
                && node
                    .parent()
                    .is_some_and(|p| p.kind() == kind::TYPE_IDENTIFIER))
        {
            let name = node.text(self.content).to_string();
            let range = node_to_range(self.doc, node);
            return Some((name, range));
        }

        None
    }

    /// Try to get the name from a struct/enum definition.
    fn try_get_definition_name(&self, node: Node, kind_name: &str) -> Option<(String, Range)> {
        let def_node = find_ancestor_of_kind(node, kind_name)?;
        let name_node = def_node.child_by_field("name")?;
        if !cursor_in_node(self.offset, name_node) {
            return None;
        }
        let name = name_node.text(self.content).to_string();
        let range = node_to_range(self.doc, name_node);
        Some((name, range))
    }

    /// Try to get the function/method name, handling struct::method syntax.
    fn try_get_function_name(&self, node: Node) -> Option<(String, Range)> {
        let func_node = find_ancestor_of_kind(node, kind::FUNCTION_DEFINITION)?;
        let name_node = func_node.child_by_field("name")?;
        if !cursor_in_node(self.offset, name_node) {
            return None;
        }

        let full_name = name_node.text(self.content);
        let Some(pos) = full_name.find("::") else {
            // Regular function name
            let name = full_name.to_string();
            let range = node_to_range(self.doc, name_node);
            return Some((name, range));
        };

        // Method name: Foo::bar
        let struct_end = name_node.start_byte() + pos;
        if self.offset < struct_end {
            // Cursor is on struct part
            let struct_name = full_name[..pos].to_string();
            let start = self.doc.offset_to_position(name_node.start_byte());
            let end = self.doc.offset_to_position(struct_end);
            Some((struct_name, Range::new(start, end)))
        } else {
            // Cursor is on method part
            let method_name = full_name[pos + 2..].to_string();
            let method_start = name_node.start_byte() + pos + 2;
            let start = self.doc.offset_to_position(method_start);
            let end = self.doc.offset_to_position(name_node.end_byte());
            Some((method_name, Range::new(start, end)))
        }
    }

    /// Check if a symbol is from the API (cannot be renamed).
    fn is_api_symbol(&self, name: &str) -> bool {
        self.api.get_type(name).is_some()
            || self.api.get_function(name).is_some()
            || self.api.get_enum(name).is_some()
            || self.api.get_module(name).is_some()
    }

    /// Collect all ranges where the symbol should be renamed.
    fn collect_rename_ranges(&self, name: &str, node: Node) -> Vec<Range> {
        let root = self.doc.tree().root_node();

        // Check if this is a global symbol
        let is_struct = self.user_structs.contains_key(name);
        let is_enum = self.user_enums.contains_key(name);
        let is_function = self.user_functions.contains_key(name);

        if is_struct {
            // For structs, collect all occurrences including method prefixes and type annotations
            self.collect_struct_name_ranges(root, name)
        } else if is_enum || is_function {
            // For enums and functions, collect identifier references
            let mut ranges = Vec::new();
            self.collect_identifier_ranges(root, name, &mut ranges);
            ranges
        } else {
            // For local variables, only collect within the enclosing function
            let mut ranges = Vec::new();
            if let Some(func_node) = find_enclosing_function(root, self.offset) {
                self.collect_identifier_ranges(func_node, name, &mut ranges);
            } else {
                // Try at root level for parameters or other cases
                self.collect_identifier_ranges(node, name, &mut ranges);
            }
            ranges
        }
    }

    /// Collect all ranges where a struct name appears.
    fn collect_struct_name_ranges(&self, root: Node, struct_name: &str) -> Vec<Range> {
        let mut ranges = Vec::new();
        self.collect_struct_name_ranges_recursive(root, struct_name, &mut ranges);
        ranges
    }

    fn collect_struct_name_ranges_recursive(
        &self,
        node: Node,
        struct_name: &str,
        ranges: &mut Vec<Range>,
    ) {
        match node.kind() {
            kind::STRUCT_DEFINITION => {
                // Collect the struct name
                self.collect_struct_def_name(node, struct_name, ranges);
                // Recurse into children, but skip the name node
                let name_node_id = node.child_by_field("name").map(|n| n.id());
                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    if Some(child.id()) != name_node_id {
                        self.collect_struct_name_ranges_recursive(child, struct_name, ranges);
                    }
                }
                return;
            }
            kind::FUNCTION_DEFINITION => {
                // Collect method prefix if applicable, and recurse but skip the name node
                self.collect_method_prefix(node, struct_name, ranges);
                let name_node_id = node.child_by_field("name").map(|n| n.id());
                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    if Some(child.id()) != name_node_id {
                        self.collect_struct_name_ranges_recursive(child, struct_name, ranges);
                    }
                }
                return;
            }
            kind::TYPE_IDENTIFIER => {
                // Type references (e.g., in parameters like `self: &StructName`)
                // Don't recurse into children since the identifier inside would be a duplicate
                if node.text(self.content) == struct_name {
                    ranges.push(node_to_range(self.doc, node));
                }
                return;
            }
            kind::IDENTIFIER => {
                // For identifiers, only collect if not already handled by type_identifier parent
                self.collect_identifier_if_matches(node, struct_name, ranges);
                // No children to recurse into
                return;
            }
            _ => {}
        }

        // Recurse into children
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.collect_struct_name_ranges_recursive(child, struct_name, ranges);
        }
    }

    /// Collect struct definition name if it matches.
    fn collect_struct_def_name(&self, node: Node, struct_name: &str, ranges: &mut Vec<Range>) {
        let Some(name_node) = node.child_by_field("name") else {
            return;
        };
        if name_node.text(self.content) == struct_name {
            ranges.push(node_to_range(self.doc, name_node));
        }
    }

    /// Collect method prefix (the struct name part of Foo::method_name).
    fn collect_method_prefix(&self, node: Node, struct_name: &str, ranges: &mut Vec<Range>) {
        let Some(name_node) = node.child_by_field("name") else {
            return;
        };
        let name = name_node.text(self.content);
        let Some(pos) = name.find("::") else {
            return;
        };
        if &name[..pos] != struct_name {
            return;
        }
        let start = self.doc.offset_to_position(name_node.start_byte());
        let end = self.doc.offset_to_position(name_node.start_byte() + pos);
        ranges.push(Range::new(start, end));
    }

    /// Collect identifier if it matches (but not if it's inside a type_identifier).
    fn collect_identifier_if_matches(
        &self,
        node: Node,
        struct_name: &str,
        ranges: &mut Vec<Range>,
    ) {
        if node.text(self.content) != struct_name {
            return;
        }
        let parent_is_type_id = node
            .parent()
            .is_some_and(|p| p.kind() == kind::TYPE_IDENTIFIER);
        if !parent_is_type_id {
            ranges.push(node_to_range(self.doc, node));
        }
    }

    /// Collect all identifier ranges with the given name.
    fn collect_identifier_ranges(&self, node: Node, name: &str, ranges: &mut Vec<Range>) {
        if node.kind() == kind::IDENTIFIER && node.text(self.content) == name {
            ranges.push(node_to_range(self.doc, node));
        }

        // Also check function names
        if node.kind() == kind::FUNCTION_NAME {
            let func_name = node.text(self.content);
            if func_name == name {
                ranges.push(node_to_range(self.doc, node));
            }
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.collect_identifier_ranges(child, name, ranges);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::{load_api, make_doc, make_uri};

    #[test]
    fn test_prepare_rename_local_variable() {
        let source = r"fn test() {
    let x = 42;
    return x;
}";
        let doc = make_doc(source);
        let api = load_api();
        // Position on "x" in let statement
        let position = Position::new(1, 8);
        let result = prepare_rename(&doc, position, &api);

        assert!(result.is_some());
        if let Some(PrepareRenameResponse::RangeWithPlaceholder { placeholder, .. }) = result {
            assert_eq!(placeholder, "x");
        } else {
            panic!("Expected RangeWithPlaceholder");
        }
    }

    #[test]
    fn test_rename_local_variable() {
        let source = r"fn test() {
    let x = 42;
    return x;
}";
        let doc = make_doc(source);
        let api = load_api();
        let uri = make_uri("test");
        // Position on "x" in let statement
        let position = Position::new(1, 8);
        let result = rename(&doc, position, &uri, "newName", &api);

        assert!(result.is_some());
        let edit = result.expect("should have edit");
        let changes = edit.changes.expect("should have changes");
        let edits = changes.get(&uri).expect("should have edits for uri");
        // Should have 2 edits: declaration and usage
        assert_eq!(edits.len(), 2);
        assert!(edits.iter().all(|e| e.new_text == "newName"));
    }

    #[test]
    fn test_rename_struct() {
        let source = r"pub struct Foo extend Signal { }

pub fn Foo::method(self: &Foo): i64 {
    return 0;
}";
        let doc = make_doc(source);
        let api = load_api();
        let uri = make_uri("test");
        // Position on "Foo" in struct definition
        let position = Position::new(0, 11);
        let result = rename(&doc, position, &uri, "Bar", &api);

        assert!(result.is_some());
        let edit = result.expect("should have edit");
        let changes = edit.changes.expect("should have changes");
        let edits = changes.get(&uri).expect("should have edits for uri");
        // Should have 3 edits: struct def, method prefix, self type
        assert_eq!(edits.len(), 3);
        assert!(edits.iter().all(|e| e.new_text == "Bar"));
    }

    #[test]
    fn test_cannot_rename_api_type() {
        let source = r"fn test(s: &Signal) { }";
        let doc = make_doc(source);
        let api = load_api();
        // Position on "Signal"
        let position = Position::new(0, 12);
        let result = prepare_rename(&doc, position, &api);

        // Should return None - cannot rename API types
        assert!(result.is_none());
    }

    #[test]
    fn test_rename_function_parameter() {
        let source = r"fn test(value: i64): i64 {
    return value + 1;
}";
        let doc = make_doc(source);
        let api = load_api();
        let uri = make_uri("test");
        // Position on "value" in parameter
        let position = Position::new(0, 8);
        let result = rename(&doc, position, &uri, "x", &api);

        assert!(result.is_some());
        let edit = result.expect("should have edit");
        let changes = edit.changes.expect("should have changes");
        let edits = changes.get(&uri).expect("should have edits for uri");
        // Should have 2 edits: parameter and usage
        assert_eq!(edits.len(), 2);
    }
}
