//! Call hierarchy provider for NimbyScript LSP.
//!
//! Provides call hierarchy navigation for functions and methods,
//! showing incoming calls (who calls this) and outgoing calls (what this calls).

use std::collections::HashMap;

use serde::{Deserialize, Serialize};
use tower_lsp::lsp_types::*;

use nimbyscript_parser::{kind, Node, NodeExt};

use crate::document::Document;
use crate::type_inference::{find_ancestor_of_kind, find_deepest_node_at, node_to_range};

/// Data stored in CallHierarchyItem.data field for later lookups.
#[derive(Debug, Serialize, Deserialize)]
struct CallHierarchyData {
    /// The full function name (e.g., "foo" or "MyStruct::method")
    name: String,
}

// ============================================================================
// Public API
// ============================================================================

/// Prepare call hierarchy for a position in the document.
/// Returns the function(s) at the given position that can be used for hierarchy navigation.
pub fn prepare_call_hierarchy(
    doc: &Document,
    position: Position,
    uri: &Url,
) -> Option<Vec<CallHierarchyItem>> {
    let offset = doc.position_to_offset(position);
    let root = doc.tree().root_node();
    let node = find_deepest_node_at(root, offset)?;

    detect_function_definition(doc, node, uri, offset)
}

/// Get incoming calls for a call hierarchy item.
/// Returns all places that call this function.
pub fn get_incoming_calls(
    doc: &Document,
    item: &CallHierarchyItem,
    uri: &Url,
) -> Option<Vec<CallHierarchyIncomingCall>> {
    let data: CallHierarchyData = item.data.as_ref()?.clone().try_into().ok()?;
    let target_name = &data.name;

    // Parse method name: "Struct::method" -> "method", "foo" -> "foo"
    let method_name = target_name
        .split("::")
        .last()
        .unwrap_or(target_name.as_str());

    let root = doc.tree().root_node();
    let content = &doc.content;

    // Find all call sites to this function, grouped by containing function
    let mut calls_by_function: HashMap<String, Vec<Range>> = HashMap::new();

    collect_calls_to_function(root, content, method_name, doc, &mut calls_by_function);

    if calls_by_function.is_empty() {
        return None;
    }

    // Convert to CallHierarchyIncomingCall
    let mut results = Vec::new();
    for (caller_name, ranges) in calls_by_function {
        // Find the caller function definition
        if let Some((func_range, selection_range)) = find_function_ranges(doc, &caller_name) {
            results.push(CallHierarchyIncomingCall {
                from: CallHierarchyItem {
                    name: caller_name.clone(),
                    kind: SymbolKind::FUNCTION,
                    tags: None,
                    detail: None,
                    uri: uri.clone(),
                    range: func_range,
                    selection_range,
                    data: Some(serde_json::to_value(CallHierarchyData { name: caller_name }).ok()?),
                },
                from_ranges: ranges,
            });
        }
    }

    if results.is_empty() {
        None
    } else {
        Some(results)
    }
}

/// Get outgoing calls for a call hierarchy item.
/// Returns all functions called from within this function.
pub fn get_outgoing_calls(
    doc: &Document,
    item: &CallHierarchyItem,
    uri: &Url,
) -> Option<Vec<CallHierarchyOutgoingCall>> {
    let data: CallHierarchyData = item.data.as_ref()?.clone().try_into().ok()?;
    let target_name = &data.name;

    let root = doc.tree().root_node();
    let content = &doc.content;

    // Find the function definition
    let func_node = find_function_node(root, content, target_name)?;
    let body = func_node.child_by_field("body")?;

    // Collect all calls within this function, grouped by callee
    let mut calls_by_callee: HashMap<String, Vec<Range>> = HashMap::new();
    collect_outgoing_calls(body, content, doc, &mut calls_by_callee);

    if calls_by_callee.is_empty() {
        return None;
    }

    // Convert to CallHierarchyOutgoingCall
    let mut results = Vec::new();
    for (callee_name, ranges) in calls_by_callee {
        // Try to find the callee function definition (only works for user-defined functions)
        let (func_range, selection_range) =
            find_function_ranges(doc, &callee_name).unwrap_or((Range::default(), Range::default()));

        results.push(CallHierarchyOutgoingCall {
            to: CallHierarchyItem {
                name: callee_name.clone(),
                kind: SymbolKind::FUNCTION,
                tags: None,
                detail: None,
                uri: uri.clone(),
                range: func_range,
                selection_range,
                data: Some(serde_json::to_value(CallHierarchyData { name: callee_name }).ok()?),
            },
            from_ranges: ranges,
        });
    }

    if results.is_empty() {
        None
    } else {
        Some(results)
    }
}

// ============================================================================
// Detection
// ============================================================================

/// Detect if cursor is on a function definition.
fn detect_function_definition(
    doc: &Document,
    node: Node,
    uri: &Url,
    offset: usize,
) -> Option<Vec<CallHierarchyItem>> {
    let func_node = find_ancestor_of_kind(node, kind::FUNCTION_DEFINITION)?;

    // Get the function name
    let name_node = func_node.child_by_field("name")?;
    let content = &doc.content;

    // Only trigger if cursor is on or near the function name
    // Check if cursor is on name node or in the visibility/fn keywords area
    if offset > name_node.end_byte() {
        return None;
    }

    let name = name_node.text(content).to_string();
    let range = node_to_range(doc, func_node);
    let selection_range = node_to_range(doc, name_node);

    // Determine if it's a method (has ::)
    let kind = if name.contains("::") {
        SymbolKind::METHOD
    } else {
        SymbolKind::FUNCTION
    };

    Some(vec![CallHierarchyItem {
        name: name.clone(),
        kind,
        tags: None,
        detail: None,
        uri: uri.clone(),
        range,
        selection_range,
        data: Some(serde_json::to_value(CallHierarchyData { name }).ok()?),
    }])
}

// ============================================================================
// Call Collection
// ============================================================================

/// Collect all calls to a specific function, grouped by the containing function.
fn collect_calls_to_function(
    node: Node,
    content: &str,
    target_method: &str,
    doc: &Document,
    calls: &mut HashMap<String, Vec<Range>>,
) {
    if node.kind() == kind::CALL_EXPRESSION {
        if let Some(callee_name) = extract_callee_name(node, content) {
            // Check if this call matches our target
            // For method calls, just match the method name part
            let call_method = callee_name.split("::").last().unwrap_or(&callee_name);
            if call_method == target_method {
                // Find the containing function
                if let Some(containing_func) = find_containing_function(node, content) {
                    let call_range = node_to_range(doc, node);
                    calls.entry(containing_func).or_default().push(call_range);
                }
            }
        }
    }

    // Recurse into children
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_calls_to_function(child, content, target_method, doc, calls);
    }
}

/// Collect all outgoing calls from a node.
fn collect_outgoing_calls(
    node: Node,
    content: &str,
    doc: &Document,
    calls: &mut HashMap<String, Vec<Range>>,
) {
    if node.kind() == kind::CALL_EXPRESSION {
        if let Some(callee_name) = extract_callee_name(node, content) {
            let call_range = node_to_range(doc, node);
            calls.entry(callee_name).or_default().push(call_range);
        }
    }

    // Recurse into children
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_outgoing_calls(child, content, doc, calls);
    }
}

/// Extract the callee name from a call expression.
fn extract_callee_name(call_node: Node, content: &str) -> Option<String> {
    let callee = call_node.child_by_field("function")?;

    match callee.kind() {
        kind::IDENTIFIER => Some(callee.text(content).to_string()),
        kind::PATH_EXPRESSION => {
            // Handle paths like Module::func or Type::method
            Some(callee.text(content).to_string())
        }
        kind::FIELD_ACCESS => {
            // Method call: obj.method()
            // Extract just the method name
            let method_node = callee.child_by_field("field")?;
            Some(method_node.text(content).to_string())
        }
        _ => None,
    }
}

/// Find the containing function name for a node.
fn find_containing_function(node: Node, content: &str) -> Option<String> {
    let func_node = find_ancestor_of_kind(node, kind::FUNCTION_DEFINITION)?;
    let name_node = func_node.child_by_field("name")?;
    Some(name_node.text(content).to_string())
}

// ============================================================================
// Function Finding
// ============================================================================

/// Find the ranges of a function definition by name.
fn find_function_ranges(doc: &Document, func_name: &str) -> Option<(Range, Range)> {
    let root = doc.tree().root_node();
    find_function_in_node(root, &doc.content, func_name, doc)
}

fn find_function_in_node(
    node: Node,
    content: &str,
    func_name: &str,
    doc: &Document,
) -> Option<(Range, Range)> {
    if node.kind() == kind::FUNCTION_DEFINITION {
        if let Some(name_node) = node.child_by_field("name") {
            let name = name_node.text(content);
            // Match full name or just the method part for method calls
            let method_part = name.split("::").last().unwrap_or(name);
            let search_method = func_name.split("::").last().unwrap_or(func_name);
            if name == func_name || method_part == search_method {
                let range = node_to_range(doc, node);
                let selection_range = node_to_range(doc, name_node);
                return Some((range, selection_range));
            }
        }
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if let Some(result) = find_function_in_node(child, content, func_name, doc) {
            return Some(result);
        }
    }

    None
}

/// Find the function definition node by name.
fn find_function_node<'a>(root: Node<'a>, content: &str, func_name: &str) -> Option<Node<'a>> {
    find_function_node_recursive(root, content, func_name)
}

fn find_function_node_recursive<'a>(
    node: Node<'a>,
    content: &str,
    func_name: &str,
) -> Option<Node<'a>> {
    if node.kind() == kind::FUNCTION_DEFINITION {
        if let Some(name_node) = node.child_by_field("name") {
            if name_node.text(content) == func_name {
                return Some(node);
            }
        }
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if let Some(result) = find_function_node_recursive(child, content, func_name) {
            return Some(result);
        }
    }

    None
}

// ============================================================================
// serde_json::Value conversion for CallHierarchyData
// ============================================================================

impl TryFrom<serde_json::Value> for CallHierarchyData {
    type Error = serde_json::Error;

    fn try_from(value: serde_json::Value) -> Result<Self, Self::Error> {
        serde_json::from_value(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::{load_api, make_uri};

    fn make_doc(source: &str) -> Document {
        let api = load_api();
        Document::new(source.to_string(), Some(&api))
    }

    #[test]
    fn test_prepare_on_function_definition() {
        let uri = make_uri("test");
        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn helper() { }
";
        let doc = make_doc(code);

        // Find position of "helper" in the function definition
        let offset = code.find("helper").expect("should find helper");
        let position = doc.offset_to_position(offset);

        let result = prepare_call_hierarchy(&doc, position, &uri);
        assert!(result.is_some(), "Should have call hierarchy for function");

        let items = result.expect("checked");
        assert_eq!(items.len(), 1);
        assert_eq!(items[0].name, "helper");
        assert_eq!(items[0].kind, SymbolKind::FUNCTION);
    }

    #[test]
    fn test_prepare_on_method_definition() {
        let uri = make_uri("test");
        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct MyStruct extend Signal { }
pub fn MyStruct::do_stuff(self: &MyStruct) { }
";
        let doc = make_doc(code);

        let offset = code.find("MyStruct::do_stuff").expect("should find method");
        let position = doc.offset_to_position(offset);

        let result = prepare_call_hierarchy(&doc, position, &uri);
        assert!(result.is_some(), "Should have call hierarchy for method");

        let items = result.expect("checked");
        assert_eq!(items.len(), 1);
        assert_eq!(items[0].name, "MyStruct::do_stuff");
        assert_eq!(items[0].kind, SymbolKind::METHOD);
    }

    #[test]
    fn test_incoming_calls() {
        let uri = make_uri("test");
        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn helper() { }

fn caller1() {
    helper();
}

fn caller2() {
    helper();
    helper();
}
";
        let doc = make_doc(code);

        // Get call hierarchy for helper
        let offset = code.find("fn helper").expect("should find helper") + 3;
        let position = doc.offset_to_position(offset);
        let items = prepare_call_hierarchy(&doc, position, &uri).expect("should prepare");

        let incoming = get_incoming_calls(&doc, &items[0], &uri);
        assert!(incoming.is_some(), "Should have incoming calls");

        let incoming = incoming.expect("checked");
        assert_eq!(incoming.len(), 2, "Should have 2 callers");

        let caller_names: Vec<_> = incoming.iter().map(|c| c.from.name.as_str()).collect();
        assert!(caller_names.contains(&"caller1"));
        assert!(caller_names.contains(&"caller2"));

        // caller2 calls helper twice
        let caller2 = incoming
            .iter()
            .find(|c| c.from.name == "caller2")
            .expect("should find caller2");
        assert_eq!(
            caller2.from_ranges.len(),
            2,
            "caller2 should have 2 call sites"
        );
    }

    #[test]
    fn test_outgoing_calls() {
        let uri = make_uri("test");
        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn helper1() { }
fn helper2() { }

fn main_func() {
    helper1();
    helper2();
    helper1();
}
";
        let doc = make_doc(code);

        // Get call hierarchy for main_func
        let offset = code.find("fn main_func").expect("should find main_func") + 3;
        let position = doc.offset_to_position(offset);
        let items = prepare_call_hierarchy(&doc, position, &uri).expect("should prepare");

        let outgoing = get_outgoing_calls(&doc, &items[0], &uri);
        assert!(outgoing.is_some(), "Should have outgoing calls");

        let outgoing = outgoing.expect("checked");
        assert_eq!(outgoing.len(), 2, "Should call 2 different functions");

        let callee_names: Vec<_> = outgoing.iter().map(|c| c.to.name.as_str()).collect();
        assert!(callee_names.contains(&"helper1"));
        assert!(callee_names.contains(&"helper2"));

        // helper1 is called twice
        let helper1 = outgoing
            .iter()
            .find(|c| c.to.name == "helper1")
            .expect("should find helper1");
        assert_eq!(
            helper1.from_ranges.len(),
            2,
            "helper1 should be called twice"
        );
    }

    #[test]
    fn test_no_incoming_calls() {
        let uri = make_uri("test");
        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn unused() { }
";
        let doc = make_doc(code);

        let offset = code.find("fn unused").expect("should find unused") + 3;
        let position = doc.offset_to_position(offset);
        let items = prepare_call_hierarchy(&doc, position, &uri).expect("should prepare");

        let incoming = get_incoming_calls(&doc, &items[0], &uri);
        assert!(incoming.is_none(), "Should have no incoming calls");
    }

    #[test]
    fn test_no_outgoing_calls() {
        let uri = make_uri("test");
        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn empty() { }
";
        let doc = make_doc(code);

        let offset = code.find("fn empty").expect("should find empty") + 3;
        let position = doc.offset_to_position(offset);
        let items = prepare_call_hierarchy(&doc, position, &uri).expect("should prepare");

        let outgoing = get_outgoing_calls(&doc, &items[0], &uri);
        assert!(outgoing.is_none(), "Should have no outgoing calls");
    }
}
