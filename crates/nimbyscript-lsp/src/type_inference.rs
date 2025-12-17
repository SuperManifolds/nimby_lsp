//! Shared type inference and utilities for LSP features.
//!
//! This module consolidates type-related functionality used across multiple LSP
//! features (completions, hover, navigation, inlay hints) to avoid code duplication.

use std::collections::HashMap;

use nimbyscript_analyzer::{ApiDefinitions, FunctionDef, TypeInfo};
use nimbyscript_parser::{kind, Node, NodeExt};
use tower_lsp::lsp_types::*;

use crate::document::Document;

// Re-export from analyzer - never duplicate
pub use nimbyscript_analyzer::types::parse_type_string;

// ============================================================================
// Type Context
// ============================================================================

/// Context for type inference operations.
///
/// Bundles the API definitions and user-defined type information needed for
/// inferring types from AST nodes.
pub struct TypeContext<'a> {
    pub content: &'a str,
    pub api: &'a ApiDefinitions,
    pub struct_fields: &'a HashMap<String, HashMap<String, TypeInfo>>,
    pub user_structs: &'a HashMap<String, Option<String>>,
}

// ============================================================================
// Type Inference
// ============================================================================

/// Infer the type of an AST node.
///
/// This is the main entry point for type inference. It handles identifiers,
/// path expressions, field access, call expressions, literals, and expressions.
pub fn infer_node_type(
    ctx: &TypeContext,
    node: Node,
    local_types: &HashMap<String, TypeInfo>,
    enclosing_struct: Option<&str>,
) -> Option<TypeInfo> {
    match node.kind() {
        kind::IDENTIFIER => {
            let name = node.text(ctx.content);
            local_types.get(name).cloned()
        }
        kind::PATH_EXPRESSION => {
            let text = node.text(ctx.content);
            if text.contains("::") {
                // Path like Foo::bar - enum variant or static, not inferrable directly
                None
            } else {
                local_types.get(text).cloned()
            }
        }
        kind::FIELD_ACCESS => infer_field_access(ctx, node, local_types, enclosing_struct),
        kind::CALL_EXPRESSION => infer_call_expression(ctx, node, local_types, enclosing_struct),
        kind::NUMBER => Some(infer_number_type(ctx, node)),
        kind::BOOLEAN => Some(TypeInfo::Bool),
        kind::STRING_LITERAL => Some(TypeInfo::String),
        kind::UNARY_EXPRESSION => node
            .child_by_field("operand")
            .and_then(|op| infer_node_type(ctx, op, local_types, enclosing_struct)),
        kind::BINARY_EXPRESSION => {
            // Binary expressions don't have named fields, get first named child (left operand)
            let mut cursor = node.walk();
            let left = node.children(&mut cursor).find(Node::is_named);
            left.and_then(|l| infer_node_type(ctx, l, local_types, enclosing_struct))
        }
        _ => None,
    }
}

/// Infer the type of a field access expression (e.g., `obj.field`).
fn infer_field_access(
    ctx: &TypeContext,
    node: Node,
    local_types: &HashMap<String, TypeInfo>,
    enclosing_struct: Option<&str>,
) -> Option<TypeInfo> {
    let object = node.child_by_field("object")?;
    let field = node.child_by_field("field")?;
    let object_type = infer_node_type(ctx, object, local_types, enclosing_struct)?;
    let type_name = resolve_type_name(&object_type, enclosing_struct)?;
    let field_name = field.text(ctx.content);

    // Check user struct fields first
    if let Some(fields) = ctx.struct_fields.get(&type_name) {
        if let Some(field_type) = fields.get(field_name) {
            return Some(field_type.clone());
        }
    }

    // Check API type fields
    if let Some(type_def) = ctx.api.get_type(&type_name) {
        if let Some(field_def) = type_def.fields.get(field_name) {
            return Some(parse_type_string(&field_def.ty));
        }
    }

    None
}

/// Infer the return type of a call expression.
fn infer_call_expression(
    ctx: &TypeContext,
    node: Node,
    local_types: &HashMap<String, TypeInfo>,
    enclosing_struct: Option<&str>,
) -> Option<TypeInfo> {
    let callee = node.child_by_field("function")?;

    match callee.kind() {
        kind::IDENTIFIER => {
            // Global function call: foo(...)
            let name = callee.text(ctx.content);
            let func = ctx.api.get_function(name)?;
            func.return_type.as_ref().map(|t| parse_type_string(t))
        }
        kind::PATH_EXPRESSION => {
            infer_path_call_expression(ctx, callee, local_types, enclosing_struct)
        }
        kind::FIELD_ACCESS => {
            infer_method_call_expression(ctx, node, callee, local_types, enclosing_struct)
        }
        _ => None,
    }
}

/// Check if a default struct method is static (no self parameter).
fn is_static_method(method: &FunctionDef) -> bool {
    method
        .params
        .first()
        .is_none_or(|p| p.ty != "&Self" && p.ty != "Self")
}

/// Infer return type for path-based calls (global functions, module functions, type methods).
fn infer_path_call_expression(
    ctx: &TypeContext,
    callee: Node,
    _local_types: &HashMap<String, TypeInfo>,
    _enclosing_struct: Option<&str>,
) -> Option<TypeInfo> {
    let text = callee.text(ctx.content);

    // Simple path without :: is a global function
    if !text.contains("::") {
        let func = ctx.api.get_function(text)?;
        return func.return_type.as_ref().map(|t| parse_type_string(t));
    }

    // Module function or type method: Math::abs(...) or Type::method(...)
    let parts: Vec<&str> = text.split("::").collect();
    if parts.len() != 2 {
        return None;
    }
    let prefix = parts[0];
    let name = parts[1];

    // Try module function
    if let Some(module) = ctx.api.get_module(prefix) {
        if let Some(func) = module.functions.iter().find(|f| f.name == name) {
            return func.return_type.as_ref().map(|t| parse_type_string(t));
        }
    }

    // Try type static method
    if let Some(type_def) = ctx.api.get_type(prefix) {
        if let Some(method) = type_def.methods.iter().find(|m| m.name == name) {
            return method.return_type.as_ref().map(|t| parse_type_string(t));
        }
    }

    // Check if prefix is a user-defined struct with static default methods (e.g., Task::new())
    if ctx.user_structs.contains_key(prefix) {
        if let Some(default_method) = ctx.api.get_default_struct_method(name) {
            if is_static_method(default_method) {
                return infer_default_method_return_type(ctx, default_method, prefix, prefix);
            }
        }
    }

    None
}

/// Infer return type for method calls (obj.method(...)).
fn infer_method_call_expression(
    ctx: &TypeContext,
    call_node: Node,
    callee: Node,
    local_types: &HashMap<String, TypeInfo>,
    enclosing_struct: Option<&str>,
) -> Option<TypeInfo> {
    let object = callee.child_by_field("object")?;
    let method = callee.child_by_field("field")?;
    let object_type = infer_node_type(ctx, object, local_types, enclosing_struct)?;
    let type_name = resolve_type_name(&object_type, enclosing_struct)?;
    let method_name = method.text(ctx.content);
    let base_type = base_type_name(&type_name);

    // Check for default instance methods (e.g., clone on private structs)
    if let Some(default_method) = ctx.api.get_default_struct_method(method_name) {
        if !is_static_method(default_method) {
            return infer_default_method_return_type(ctx, default_method, &type_name, &base_type);
        }
    }

    // Look up method in API type
    let type_def = ctx.api.get_type(&base_type)?;
    let method_def = type_def.methods.iter().find(|m| m.name == method_name)?;
    let return_type = method_def
        .return_type
        .as_ref()
        .map(|t| parse_type_string(t))?;

    // Handle generic type parameters
    let mut generic_args = extract_explicit_type_args(callee, ctx.content, ctx.api);

    // If no explicit type args, try to get from object type (e.g., std::optional<T>.get())
    if generic_args.is_empty() {
        generic_args = extract_generic_args(&object_type);
    }

    // If still no args and method has type_params, try to infer from arguments
    if generic_args.is_empty() && !method_def.type_params.is_empty() {
        generic_args = infer_type_params_from_args(ctx, call_node, method_def, local_types);
    }

    Some(substitute_type_params(return_type, &generic_args))
}

/// Infer the type of a number literal.
fn infer_number_type(ctx: &TypeContext, node: Node) -> TypeInfo {
    let text = node.text(ctx.content);
    if text.contains('.') {
        TypeInfo::F64
    } else {
        TypeInfo::I64
    }
}

/// Infer return type for a default struct method (new, clone).
fn infer_default_method_return_type(
    ctx: &TypeContext,
    method: &FunctionDef,
    type_name: &str,
    base_type_name: &str,
) -> Option<TypeInfo> {
    let ret = method.return_type.as_ref()?;
    if ret == "Self" {
        Some(TypeInfo::Struct {
            name: type_name.to_string(),
            extends: ctx.user_structs.get(base_type_name).cloned().flatten(),
        })
    } else {
        Some(parse_type_string(ret))
    }
}

// ============================================================================
// Type Name Resolution
// ============================================================================

/// Extract base type name from TypeInfo, unwrapping references/pointers.
///
/// For complex types, builds a string representation including generic args.
pub fn unwrap_to_type_name(ty: &TypeInfo) -> Option<String> {
    match ty {
        TypeInfo::Struct { name, .. } | TypeInfo::Enum { name } => Some(name.clone()),
        TypeInfo::Reference { inner, .. } | TypeInfo::Pointer { inner, .. } => {
            unwrap_to_type_name(inner)
        }
        TypeInfo::Generic { name, args } => {
            if args.is_empty() {
                Some(name.clone())
            } else {
                let arg_names: Vec<_> = args.iter().filter_map(unwrap_to_type_name).collect();
                Some(format!("{}<{}>", name, arg_names.join(", ")))
            }
        }
        _ => None,
    }
}

/// Resolve type name, handling Self substitution.
///
/// If the type is `Self`, substitutes the enclosing struct name.
pub fn resolve_type_name(ty: &TypeInfo, enclosing_struct: Option<&str>) -> Option<String> {
    let name = unwrap_to_type_name(ty)?;
    if name == "Self" {
        enclosing_struct.map(ToString::to_string)
    } else {
        Some(name)
    }
}

/// Extract base type name without generic args (e.g., "ID<Signal>" -> "ID").
pub fn base_type_name(type_name: &str) -> String {
    type_name.split('<').next().unwrap_or(type_name).to_string()
}

// ============================================================================
// Generic Type Handling
// ============================================================================

/// Extract generic type arguments from a type, unwrapping references/pointers.
///
/// For `std::optional<Motion::Drive>`, returns `[Motion::Drive]`.
pub fn extract_generic_args(ty: &TypeInfo) -> Vec<TypeInfo> {
    match ty {
        TypeInfo::Generic { args, .. } => args.clone(),
        TypeInfo::Reference { inner, .. } | TypeInfo::Pointer { inner, .. } => {
            extract_generic_args(inner)
        }
        _ => Vec::new(),
    }
}

/// Extract explicit type arguments from a field_access node's type_arguments field.
///
/// For `view<Hitcher>`, returns `[Hitcher]`.
pub fn extract_explicit_type_args(
    field_access: Node,
    content: &str,
    api: &ApiDefinitions,
) -> Vec<TypeInfo> {
    let Some(type_args_node) = field_access.child_by_field("type_arguments") else {
        return Vec::new();
    };

    let mut result = Vec::new();
    let mut cursor = type_args_node.walk();
    for child in type_args_node.children(&mut cursor) {
        if child.kind() == kind::TYPE_IDENTIFIER {
            let type_name = child.text(content);
            // Resolve the type - check if it's a known type
            if api.get_type(type_name).is_some() {
                result.push(TypeInfo::Struct {
                    name: type_name.to_string(),
                    extends: None,
                });
            } else {
                // Could be a user-defined type
                result.push(TypeInfo::Struct {
                    name: type_name.to_string(),
                    extends: None,
                });
            }
        }
    }
    result
}

/// Infer type parameters from method arguments.
///
/// For `DB.view(id: ID<T>) -> *T`, if called with `ID<Signal>`, returns `[Signal]`.
fn infer_type_params_from_args(
    ctx: &TypeContext,
    call_node: Node,
    method_def: &FunctionDef,
    local_types: &HashMap<String, TypeInfo>,
) -> Vec<TypeInfo> {
    let Some(args_node) = call_node.child_by_kind("arguments") else {
        return Vec::new();
    };

    let mut cursor = args_node.walk();
    let args: Vec<Node> = args_node
        .children(&mut cursor)
        .filter(|c| c.kind() != "," && c.kind() != "(" && c.kind() != ")")
        .collect();

    method_def
        .type_params
        .iter()
        .map(|type_param| {
            find_type_param_in_args(ctx, type_param, &args, method_def, local_types)
                .unwrap_or(TypeInfo::Unknown)
        })
        .collect()
}

/// Find a type parameter value by matching against argument types.
fn find_type_param_in_args(
    ctx: &TypeContext,
    type_param: &str,
    args: &[Node],
    method_def: &FunctionDef,
    local_types: &HashMap<String, TypeInfo>,
) -> Option<TypeInfo> {
    for (i, param_def) in method_def.params.iter().enumerate() {
        if i >= args.len() {
            break;
        }

        // Check if param type contains this type parameter (e.g., "ID<T>")
        if !param_def.ty.contains(type_param) {
            continue;
        }

        // Infer the argument type
        let arg_type = infer_node_type(ctx, args[i], local_types, None)?;

        // Extract the generic argument that corresponds to T
        if let Some(t) = extract_type_param_from_arg(&arg_type, &param_def.ty, type_param) {
            return Some(t);
        }
    }
    None
}

/// Extract a type parameter value from an argument type given the parameter pattern.
///
/// For arg_type `ID<Signal>`, param_pattern `ID<T>`, type_param `T`, returns `Signal`.
fn extract_type_param_from_arg(
    arg_type: &TypeInfo,
    param_pattern: &str,
    type_param: &str,
) -> Option<TypeInfo> {
    let pattern_type = parse_type_string(param_pattern);

    let TypeInfo::Generic {
        name: pattern_name,
        args: pattern_args,
    } = &pattern_type
    else {
        return None;
    };

    // Find which position has the type parameter
    let position = pattern_args.iter().position(
        |pattern_arg| matches!(pattern_arg, TypeInfo::Struct { name, .. } if name == type_param),
    )?;

    // Extract the position-th arg from the actual arg_type
    let actual_args = extract_generic_args(arg_type);
    let actual_arg = actual_args.get(position)?;

    // Verify the base types match
    let TypeInfo::Generic {
        name: actual_name, ..
    } = arg_type
    else {
        return None;
    };

    if actual_name == pattern_name {
        Some(actual_arg.clone())
    } else {
        None
    }
}

/// Substitute type parameter `T` with actual types from generic arguments.
///
/// For example, `*T` with args `[Motion::Drive]` becomes `*Motion::Drive`.
pub fn substitute_type_params(ty: TypeInfo, generic_args: &[TypeInfo]) -> TypeInfo {
    match ty {
        TypeInfo::Struct { ref name, .. } if name == "T" => {
            generic_args.first().cloned().unwrap_or(ty)
        }
        TypeInfo::Reference { inner, is_mut } => TypeInfo::Reference {
            inner: Box::new(substitute_type_params(*inner, generic_args)),
            is_mut,
        },
        TypeInfo::Pointer { inner, is_mut } => TypeInfo::Pointer {
            inner: Box::new(substitute_type_params(*inner, generic_args)),
            is_mut,
        },
        TypeInfo::Generic { name, args } => TypeInfo::Generic {
            name,
            args: args
                .into_iter()
                .map(|a| substitute_type_params(a, generic_args))
                .collect(),
        },
        TypeInfo::Array { element, size } => TypeInfo::Array {
            element: Box::new(substitute_type_params(*element, generic_args)),
            size,
        },
        other => other,
    }
}

// ============================================================================
// Parameter/Binding Extraction
// ============================================================================

/// Extract a parameter's name and type from a PARAMETER node.
pub fn extract_param(param_node: Node, content: &str) -> Option<(String, TypeInfo)> {
    let name = param_node.child_by_field("name")?.text(content).to_string();
    let ty = param_node.child_by_field("type")?;
    let type_info = parse_type_string(ty.text(content));
    Some((name, type_info))
}

/// Extract a parameter's name and type as strings from a PARAMETER node.
pub fn extract_param_strings(param_node: Node, content: &str) -> Option<(String, String)> {
    let name = param_node.child_by_field("name")?;
    let ty = param_node.child_by_field("type")?;
    Some((name.text(content).to_string(), ty.text(content).to_string()))
}

/// Extract all parameters from a PARAMETERS node.
pub fn extract_params(params_node: Node, content: &str) -> Vec<(String, TypeInfo)> {
    let mut cursor = params_node.walk();
    params_node
        .children(&mut cursor)
        .filter(|n| n.kind() == kind::PARAMETER)
        .filter_map(|p| extract_param(p, content))
        .collect()
}

/// Extract all parameters from a PARAMETERS node as (name, type) string pairs.
pub fn extract_params_strings(params_node: Node, content: &str) -> Vec<(String, String)> {
    let mut cursor = params_node.walk();
    params_node
        .children(&mut cursor)
        .filter(|c| c.kind() == kind::PARAMETER)
        .filter_map(|c| extract_param_strings(c, content))
        .collect()
}

/// Extract binding name and type from a LET_STATEMENT node.
///
/// Returns the binding name and its explicit type annotation (if present).
pub fn extract_binding(let_node: Node, content: &str) -> Option<(String, TypeInfo)> {
    let binding = let_node.child_by_kind(kind::BINDING)?;
    let name = binding.child_by_field("name")?.text(content).to_string();
    let ty = binding.child_by_field("type")?;
    let type_info = parse_type_string(ty.text(content));
    Some((name, type_info))
}

// ============================================================================
// Local Type Collection
// ============================================================================

/// Find the enclosing function for a given offset.
pub fn find_enclosing_function(root: Node, offset: usize) -> Option<Node> {
    fn search(node: Node, offset: usize) -> Option<Node> {
        if node.kind() == kind::FUNCTION_DEFINITION
            && node.start_byte() <= offset
            && offset <= node.end_byte()
        {
            return Some(node);
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(found) = search(child, offset) {
                return Some(found);
            }
        }
        None
    }
    search(root, offset)
}

/// Collect local variable types from a function node.
///
/// Collects parameter types and let binding types. If `cursor_offset` is provided,
/// only collects bindings declared before that offset.
pub fn collect_local_types_in_function(
    func_node: Node,
    content: &str,
    cursor_offset: Option<usize>,
) -> HashMap<String, TypeInfo> {
    let mut types = HashMap::new();

    // Parameters
    if let Some(params) = func_node.child_by_kind(kind::PARAMETERS) {
        for (name, ty) in extract_params(params, content) {
            types.insert(name, ty);
        }
    }

    // Let bindings
    let body = func_node
        .child_by_field("body")
        .or_else(|| func_node.child_by_kind(kind::BLOCK));
    if let Some(body) = body {
        collect_bindings_recursive(body, content, cursor_offset, &mut types);
    }

    types
}

/// Recursively collect let bindings from a node.
fn collect_bindings_recursive(
    node: Node,
    content: &str,
    cursor_offset: Option<usize>,
    types: &mut HashMap<String, TypeInfo>,
) {
    // Skip if past cursor
    if let Some(offset) = cursor_offset {
        if node.start_byte() > offset {
            return;
        }
    }

    let is_let = node.kind() == kind::LET_STATEMENT || node.kind() == kind::LET_ELSE_STATEMENT;
    if is_let {
        if let Some((name, ty)) = extract_binding(node, content) {
            types.insert(name, ty);
        }
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_bindings_recursive(child, content, cursor_offset, types);
    }
}

// ============================================================================
// Signature Formatting
// ============================================================================

/// Format a function signature for display.
pub fn format_signature(func: &FunctionDef) -> String {
    let params = func
        .params
        .iter()
        .map(|p| format!("{}: {}", p.name, p.ty))
        .collect::<Vec<_>>()
        .join(", ");

    match &func.return_type {
        Some(ret) => format!("({params}) -> {ret}"),
        None => format!("({params})"),
    }
}

/// Format callback signature with Self substitution.
pub fn format_callback_signature(func: &FunctionDef, struct_name: &str) -> String {
    let params = func
        .params
        .iter()
        .map(|p| {
            let ty = if p.ty == "&Self" {
                format!("&{struct_name}")
            } else {
                p.ty.clone()
            };
            format!("{}: {ty}", p.name)
        })
        .collect::<Vec<_>>()
        .join(", ");

    match &func.return_type {
        Some(ret) => format!("({params}) -> {ret}"),
        None => format!("({params})"),
    }
}

// ============================================================================
// AST Position Helpers
// ============================================================================

/// Check if a cursor offset is within a node's byte range (inclusive end).
pub fn cursor_in_node(offset: usize, node: Node) -> bool {
    offset >= node.start_byte() && offset <= node.end_byte()
}

/// Convert a tree-sitter node to an LSP Range.
pub fn node_to_range(doc: &Document, node: Node) -> Range {
    let start = doc.offset_to_position(node.start_byte());
    let end = doc.offset_to_position(node.end_byte());
    Range::new(start, end)
}

// ============================================================================
// AST Traversal Helpers
// ============================================================================

/// Find the deepest (most specific) node containing the given offset.
///
/// Recursively walks down the AST tree to find the smallest node that
/// contains the cursor position.
pub fn find_deepest_node_at(node: Node, offset: usize) -> Option<Node> {
    let start = node.start_byte();
    let end = node.end_byte();

    if offset < start || offset >= end {
        return None;
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if let Some(deeper) = find_deepest_node_at(child, offset) {
            return Some(deeper);
        }
    }

    Some(node)
}

/// Find an ancestor of a specific kind by walking up the tree.
pub fn find_ancestor_of_kind<'a>(node: Node<'a>, target_kind: &str) -> Option<Node<'a>> {
    let mut current = Some(node);
    while let Some(n) = current {
        if n.kind() == target_kind {
            return Some(n);
        }
        current = n.parent();
    }
    None
}

/// Get the struct name from a method's function name (e.g., "Foo::bar" -> "Foo")
pub fn get_enclosing_struct_name(func_node: Node, content: &str) -> Option<String> {
    let name_node = func_node.child_by_field("name")?;
    let name = name_node.text(content);
    let pos = name.find("::")?;
    Some(name[..pos].to_string())
}
