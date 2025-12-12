//! Type hierarchy provider for NimbyScript LSP.
//!
//! Provides type hierarchy navigation for structs and their supertypes/subtypes.

use std::collections::HashMap;

use serde::{Deserialize, Serialize};
use tower_lsp::lsp_types::*;

use nimbyscript_analyzer::{collect_declarations, ApiDefinitions, SemanticContext};
use nimbyscript_parser::{kind, Node, NodeExt};

use crate::document::Document;

/// Data stored in TypeHierarchyItem.data field for later lookups.
#[derive(Debug, Serialize, Deserialize)]
struct TypeHierarchyData {
    /// The type name (e.g., "ProbeCheck" or "Signal")
    name: String,
    /// Whether this is a user-defined type or a game API type
    is_user_type: bool,
}

// ============================================================================
// Public API
// ============================================================================

/// Prepare type hierarchy for a position in the document.
/// Returns the type(s) at the given position that can be used for hierarchy navigation.
pub fn prepare_type_hierarchy(
    doc: &Document,
    position: Position,
    uri: &Url,
    api: &ApiDefinitions,
) -> Option<Vec<TypeHierarchyItem>> {
    let engine = TypeHierarchyEngine::new(doc, api, position);
    engine.prepare(uri)
}

/// Get supertypes for a type hierarchy item.
pub fn get_supertypes(
    item: &TypeHierarchyItem,
    doc: &Document,
    api: &ApiDefinitions,
) -> Option<Vec<TypeHierarchyItem>> {
    let data: TypeHierarchyData = item.data.as_ref()?.clone().try_into().ok()?;

    if data.is_user_type {
        // User-defined struct - get its extends type
        let extends = doc.struct_extends(&data.name)?;

        // The supertype is a game type
        let type_def = api.get_type(extends)?;

        Some(vec![TypeHierarchyItem {
            name: extends.to_string(),
            kind: SymbolKind::STRUCT,
            tags: None,
            detail: type_def.doc.clone(),
            uri: item.uri.clone(),
            range: Range::default(),
            selection_range: Range::default(),
            data: Some(
                serde_json::to_value(TypeHierarchyData {
                    name: extends.to_string(),
                    is_user_type: false,
                })
                .ok()?,
            ),
        }])
    } else {
        // Game type - check if it has an extends in the API
        let type_def = api.get_type(&data.name)?;
        let extends = type_def.extends.as_ref()?;
        let parent_def = api.get_type(extends)?;

        Some(vec![TypeHierarchyItem {
            name: extends.clone(),
            kind: SymbolKind::STRUCT,
            tags: None,
            detail: parent_def.doc.clone(),
            uri: item.uri.clone(),
            range: Range::default(),
            selection_range: Range::default(),
            data: Some(
                serde_json::to_value(TypeHierarchyData {
                    name: extends.clone(),
                    is_user_type: false,
                })
                .ok()?,
            ),
        }])
    }
}

/// Get subtypes for a type hierarchy item.
pub fn get_subtypes(
    item: &TypeHierarchyItem,
    doc: &Document,
    api: &ApiDefinitions,
) -> Option<Vec<TypeHierarchyItem>> {
    let data: TypeHierarchyData = item.data.as_ref()?.clone().try_into().ok()?;

    // Build semantic context to get user_structs
    let mut ctx = SemanticContext::new(&doc.content, doc.tree(), api);
    collect_declarations(&mut ctx);

    let mut subtypes = Vec::new();

    // Find all user structs that extend this type
    for (struct_name, extends) in &ctx.user_structs {
        if let Some(ext) = extends {
            if ext == &data.name {
                // Find the struct in the AST to get its range
                if let Some((range, selection_range)) = find_struct_ranges(doc, struct_name) {
                    subtypes.push(TypeHierarchyItem {
                        name: struct_name.clone(),
                        kind: SymbolKind::STRUCT,
                        tags: None,
                        detail: Some(format!("extends {}", data.name)),
                        uri: item.uri.clone(),
                        range,
                        selection_range,
                        data: Some(
                            serde_json::to_value(TypeHierarchyData {
                                name: struct_name.clone(),
                                is_user_type: true,
                            })
                            .ok()?,
                        ),
                    });
                }
            }
        }
    }

    if subtypes.is_empty() {
        None
    } else {
        Some(subtypes)
    }
}

// ============================================================================
// Type Hierarchy Engine
// ============================================================================

/// Engine for computing type hierarchy information.
struct TypeHierarchyEngine<'a> {
    doc: &'a Document,
    content: &'a str,
    api: &'a ApiDefinitions,
    offset: usize,
    /// User-defined structs (name -> extends type)
    user_structs: HashMap<String, Option<String>>,
    /// User-defined enums (name -> variants)
    user_enums: HashMap<String, Vec<String>>,
}

impl<'a> TypeHierarchyEngine<'a> {
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
            user_structs: ctx.user_structs,
            user_enums: ctx.user_enums,
        }
    }

    /// Prepare type hierarchy items for the current position.
    fn prepare(&self, uri: &Url) -> Option<Vec<TypeHierarchyItem>> {
        let root = self.doc.tree().root_node();
        let node = self.find_deepest_node_at(root)?;

        // Try various detection strategies
        self.detect_struct_definition(node, uri)
            .or_else(|| self.detect_enum_definition(node, uri))
            .or_else(|| self.detect_type_reference(node, uri))
    }

    /// Find the deepest AST node containing the cursor position.
    fn find_deepest_node_at<'b>(&self, node: Node<'b>) -> Option<Node<'b>> {
        let start = node.start_byte();
        let end = node.end_byte();

        if self.offset < start || self.offset >= end {
            return None;
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(deeper) = self.find_deepest_node_at(child) {
                return Some(deeper);
            }
        }

        Some(node)
    }

    /// Detect if cursor is on a struct definition.
    fn detect_struct_definition(&self, node: Node, uri: &Url) -> Option<Vec<TypeHierarchyItem>> {
        let struct_node = Self::find_ancestor_of_kind(node, kind::STRUCT_DEFINITION)?;
        let name_node = struct_node.child_by_field("name")?;

        // Only trigger if cursor is on the struct name
        if !self.cursor_in_node(name_node) {
            return None;
        }

        let name = name_node.text(self.content).to_string();
        let extends = self.doc.struct_extends(&name);

        let range = self.node_to_range(struct_node);
        let selection_range = self.node_to_range(name_node);

        Some(vec![TypeHierarchyItem {
            name: name.clone(),
            kind: SymbolKind::STRUCT,
            tags: None,
            detail: extends.map(|e| format!("extends {e}")),
            uri: uri.clone(),
            range,
            selection_range,
            data: Some(
                serde_json::to_value(TypeHierarchyData {
                    name,
                    is_user_type: true,
                })
                .ok()?,
            ),
        }])
    }

    /// Detect if cursor is on an enum definition.
    fn detect_enum_definition(&self, node: Node, uri: &Url) -> Option<Vec<TypeHierarchyItem>> {
        let enum_node = Self::find_ancestor_of_kind(node, kind::ENUM_DEFINITION)?;
        let name_node = enum_node.child_by_field("name")?;

        // Only trigger if cursor is on the enum name
        if !self.cursor_in_node(name_node) {
            return None;
        }

        let name = name_node.text(self.content).to_string();

        let range = self.node_to_range(enum_node);
        let selection_range = self.node_to_range(name_node);

        // Enums don't have hierarchy, but we still return them for consistency
        Some(vec![TypeHierarchyItem {
            name: name.clone(),
            kind: SymbolKind::ENUM,
            tags: None,
            detail: None,
            uri: uri.clone(),
            range,
            selection_range,
            data: Some(
                serde_json::to_value(TypeHierarchyData {
                    name,
                    is_user_type: true,
                })
                .ok()?,
            ),
        }])
    }

    /// Detect if cursor is on a type reference (in type annotations, extends clauses, etc.).
    fn detect_type_reference(&self, node: Node, uri: &Url) -> Option<Vec<TypeHierarchyItem>> {
        // Check if we're in a type_identifier context
        let type_id_node = Self::find_ancestor_of_kind(node, kind::TYPE_IDENTIFIER)?;

        // Get the base type name
        let base_type_name = if node.kind() == kind::IDENTIFIER {
            node.text(self.content)
        } else {
            let mut cursor = type_id_node.walk();
            let identifier_node = type_id_node
                .children(&mut cursor)
                .find(|c| c.kind() == kind::IDENTIFIER)?;
            identifier_node.text(self.content)
        };

        // Check if it's an API type
        if let Some(type_def) = self.api.get_type(base_type_name) {
            let selection_range = self.node_to_range(node);
            return Some(vec![TypeHierarchyItem {
                name: base_type_name.to_string(),
                kind: SymbolKind::STRUCT,
                tags: None,
                detail: type_def.doc.clone(),
                uri: uri.clone(),
                range: selection_range,
                selection_range,
                data: Some(
                    serde_json::to_value(TypeHierarchyData {
                        name: base_type_name.to_string(),
                        is_user_type: false,
                    })
                    .ok()?,
                ),
            }]);
        }

        // Check if it's an API enum
        if let Some(enum_def) = self.api.get_enum(base_type_name) {
            let selection_range = self.node_to_range(node);
            return Some(vec![TypeHierarchyItem {
                name: base_type_name.to_string(),
                kind: SymbolKind::ENUM,
                tags: None,
                detail: enum_def.doc.clone(),
                uri: uri.clone(),
                range: selection_range,
                selection_range,
                data: Some(
                    serde_json::to_value(TypeHierarchyData {
                        name: base_type_name.to_string(),
                        is_user_type: false,
                    })
                    .ok()?,
                ),
            }]);
        }

        // Check if it's a user-defined struct
        if self.user_structs.contains_key(base_type_name) {
            let extends = self.doc.struct_extends(base_type_name);
            let selection_range = self.node_to_range(node);

            // Try to find the actual struct definition for the full range
            let (range, _) = find_struct_ranges(self.doc, base_type_name)
                .unwrap_or((selection_range, selection_range));

            return Some(vec![TypeHierarchyItem {
                name: base_type_name.to_string(),
                kind: SymbolKind::STRUCT,
                tags: None,
                detail: extends.map(|e| format!("extends {e}")),
                uri: uri.clone(),
                range,
                selection_range,
                data: Some(
                    serde_json::to_value(TypeHierarchyData {
                        name: base_type_name.to_string(),
                        is_user_type: true,
                    })
                    .ok()?,
                ),
            }]);
        }

        // Check if it's a user-defined enum
        if self.user_enums.contains_key(base_type_name) {
            let selection_range = self.node_to_range(node);
            return Some(vec![TypeHierarchyItem {
                name: base_type_name.to_string(),
                kind: SymbolKind::ENUM,
                tags: None,
                detail: None,
                uri: uri.clone(),
                range: selection_range,
                selection_range,
                data: Some(
                    serde_json::to_value(TypeHierarchyData {
                        name: base_type_name.to_string(),
                        is_user_type: true,
                    })
                    .ok()?,
                ),
            }]);
        }

        None
    }

    // ========================================================================
    // Helpers
    // ========================================================================

    fn cursor_in_node(&self, node: Node) -> bool {
        self.offset >= node.start_byte() && self.offset <= node.end_byte()
    }

    fn find_ancestor_of_kind<'b>(node: Node<'b>, target_kind: &str) -> Option<Node<'b>> {
        let mut current = Some(node);
        while let Some(n) = current {
            if n.kind() == target_kind {
                return Some(n);
            }
            current = n.parent();
        }
        None
    }

    fn node_to_range(&self, node: Node) -> Range {
        let start = self.doc.offset_to_position(node.start_byte());
        let end = self.doc.offset_to_position(node.end_byte());
        Range::new(start, end)
    }
}

/// Find the range of a struct definition by name.
fn find_struct_ranges(doc: &Document, struct_name: &str) -> Option<(Range, Range)> {
    let root = doc.tree().root_node();
    find_struct_in_node(root, struct_name, doc)
}

fn find_struct_in_node(node: Node, struct_name: &str, doc: &Document) -> Option<(Range, Range)> {
    if node.kind() == kind::STRUCT_DEFINITION {
        if let Some(name_node) = node.child_by_field("name") {
            if name_node.text(&doc.content) == struct_name {
                let range = Range::new(
                    doc.offset_to_position(node.start_byte()),
                    doc.offset_to_position(node.end_byte()),
                );
                let selection_range = Range::new(
                    doc.offset_to_position(name_node.start_byte()),
                    doc.offset_to_position(name_node.end_byte()),
                );
                return Some((range, selection_range));
            }
        }
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if let Some(result) = find_struct_in_node(child, struct_name, doc) {
            return Some(result);
        }
    }

    None
}

// ============================================================================
// serde_json::Value conversion for TypeHierarchyData
// ============================================================================

impl TryFrom<serde_json::Value> for TypeHierarchyData {
    type Error = serde_json::Error;

    fn try_from(value: serde_json::Value) -> Result<Self, Self::Error> {
        serde_json::from_value(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_api() -> ApiDefinitions {
        let api_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("should have parent dir")
            .parent()
            .expect("should have grandparent dir")
            .join("api-definitions/nimbyrails.v1.toml");
        ApiDefinitions::load_from_file(&api_path).expect("Failed to load API")
    }

    fn make_uri() -> Url {
        Url::parse("file:///test.nimbyscript").expect("valid url")
    }

    #[test]
    fn test_prepare_on_struct_definition() {
        let api = make_api();
        let uri = make_uri();

        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct ProbeCheck extend Signal { }
";
        let doc = Document::new(code.to_string(), Some(&api));

        // Find position of "ProbeCheck" in the struct definition
        let offset = code.find("ProbeCheck").expect("should find ProbeCheck");
        let position = doc.offset_to_position(offset);

        let result = prepare_type_hierarchy(&doc, position, &uri, &api);
        assert!(result.is_some(), "Should have type hierarchy for struct");

        let items = result.expect("checked");
        assert_eq!(items.len(), 1);
        assert_eq!(items[0].name, "ProbeCheck");
        assert_eq!(items[0].kind, SymbolKind::STRUCT);
        assert!(items[0]
            .detail
            .as_ref()
            .expect("should have detail")
            .contains("Signal"));
    }

    #[test]
    fn test_supertypes_for_user_struct() {
        let api = make_api();
        let uri = make_uri();

        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct ProbeCheck extend Signal { }
";
        let doc = Document::new(code.to_string(), Some(&api));

        let offset = code.find("ProbeCheck").expect("should find ProbeCheck");
        let position = doc.offset_to_position(offset);

        let items = prepare_type_hierarchy(&doc, position, &uri, &api).expect("should prepare");
        let supertypes = get_supertypes(&items[0], &doc, &api);

        assert!(supertypes.is_some(), "Should have supertypes");
        let supertypes = supertypes.expect("checked");
        assert_eq!(supertypes.len(), 1);
        assert_eq!(supertypes[0].name, "Signal");
    }

    #[test]
    fn test_subtypes_for_game_type() {
        let api = make_api();
        let uri = make_uri();

        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct ProbeCheck extend Signal { }
pub struct AnotherSignal extend Signal { }
pub struct MyTrain extend Train { }
";
        let doc = Document::new(code.to_string(), Some(&api));

        // Create a TypeHierarchyItem for Signal
        let signal_item = TypeHierarchyItem {
            name: "Signal".to_string(),
            kind: SymbolKind::STRUCT,
            tags: None,
            detail: None,
            uri: uri.clone(),
            range: Range::default(),
            selection_range: Range::default(),
            data: Some(
                serde_json::to_value(TypeHierarchyData {
                    name: "Signal".to_string(),
                    is_user_type: false,
                })
                .expect("valid json"),
            ),
        };

        let subtypes = get_subtypes(&signal_item, &doc, &api);
        assert!(subtypes.is_some(), "Should have subtypes");

        let subtypes = subtypes.expect("checked");
        assert_eq!(subtypes.len(), 2, "Signal should have 2 subtypes");

        let names: Vec<_> = subtypes.iter().map(|s| s.name.as_str()).collect();
        assert!(names.contains(&"ProbeCheck"));
        assert!(names.contains(&"AnotherSignal"));
        assert!(!names.contains(&"MyTrain")); // MyTrain extends Train, not Signal
    }

    #[test]
    fn test_prepare_on_type_reference() {
        let api = make_api();
        let uri = make_uri();

        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal {
    owner: ID<Train>,
}
";
        let doc = Document::new(code.to_string(), Some(&api));

        // Find position of "Train" in the type annotation
        let offset = code.find("Train").expect("should find Train");
        let position = doc.offset_to_position(offset);

        let result = prepare_type_hierarchy(&doc, position, &uri, &api);
        assert!(
            result.is_some(),
            "Should have type hierarchy for type reference"
        );

        let items = result.expect("checked");
        assert_eq!(items.len(), 1);
        assert_eq!(items[0].name, "Train");
    }

    #[test]
    fn test_no_supertypes_for_struct_without_extends() {
        let api = make_api();
        let uri = make_uri();

        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Task { }
";
        let doc = Document::new(code.to_string(), Some(&api));

        let offset = code.find("Task").expect("should find Task");
        let position = doc.offset_to_position(offset);

        let items = prepare_type_hierarchy(&doc, position, &uri, &api).expect("should prepare");

        // Task doesn't extend anything, so supertypes should be None
        let supertypes = get_supertypes(&items[0], &doc, &api);
        assert!(
            supertypes.is_none(),
            "Struct without extends should have no supertypes"
        );
    }

    #[test]
    fn test_enum_has_no_hierarchy() {
        let api = make_api();
        let uri = make_uri();

        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub enum Color { Red, Green, Blue, }
";
        let doc = Document::new(code.to_string(), Some(&api));

        let offset = code.find("Color").expect("should find Color");
        let position = doc.offset_to_position(offset);

        let items = prepare_type_hierarchy(&doc, position, &uri, &api).expect("should prepare");
        assert_eq!(items[0].kind, SymbolKind::ENUM);

        // Enums don't have hierarchy
        let supertypes = get_supertypes(&items[0], &doc, &api);
        assert!(supertypes.is_none(), "Enum should have no supertypes");

        let subtypes = get_subtypes(&items[0], &doc, &api);
        assert!(subtypes.is_none(), "Enum should have no subtypes");
    }
}
