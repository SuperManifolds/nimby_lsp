//! Type inference for NimbyScript.
//!
//! This module provides unified type inference used by both semantic analysis
//! passes and LSP features. It infers types from AST nodes using API definitions
//! and user-defined type information.

use std::collections::HashMap;

use nimbyscript_parser::{kind, Node, NodeExt};

use crate::api::{ApiDefinitions, FunctionDef};
use crate::types::{parse_type_string, TypeInfo};

// ============================================================================
// Type Inference
// ============================================================================

/// Infer the type of an AST node.
///
/// This is the main entry point for type inference. It handles identifiers,
/// path expressions, field access, call expressions, literals, and expressions.
///
/// # Arguments
/// * `node` - The AST node to infer the type of
/// * `source` - The source code
/// * `api` - API definitions for game types
/// * `struct_fields` - User-defined struct fields: struct_name -> (field_name -> type)
/// * `user_structs` - User-defined structs: name -> extends type
/// * `local_types` - Local variable types in scope
/// * `enclosing_struct` - The name of the enclosing struct (for Self resolution)
pub fn infer_type(
    node: Node,
    source: &str,
    api: &ApiDefinitions,
    struct_fields: &HashMap<String, HashMap<String, TypeInfo>>,
    user_structs: &HashMap<String, Option<String>>,
    local_types: &HashMap<String, TypeInfo>,
    enclosing_struct: Option<&str>,
) -> Option<TypeInfo> {
    match node.kind() {
        kind::IDENTIFIER => {
            let name = node.text(source);
            local_types.get(name).cloned()
        }
        kind::PATH_EXPRESSION => {
            let text = node.text(source);
            if text.contains("::") {
                // Path like Foo::bar - enum variant or static, not inferrable directly
                None
            } else {
                local_types.get(text).cloned()
            }
        }
        kind::FIELD_ACCESS => infer_field_access(
            node,
            source,
            api,
            struct_fields,
            user_structs,
            local_types,
            enclosing_struct,
        ),
        kind::CALL_EXPRESSION => infer_call_expression(
            node,
            source,
            api,
            struct_fields,
            user_structs,
            local_types,
            enclosing_struct,
        ),
        kind::NUMBER => Some(infer_number_type(node, source)),
        kind::BOOLEAN => Some(TypeInfo::Bool),
        kind::STRING_LITERAL => Some(TypeInfo::String),
        kind::UNARY_EXPRESSION => node.child_by_field("operand").and_then(|op| {
            infer_type(
                op,
                source,
                api,
                struct_fields,
                user_structs,
                local_types,
                enclosing_struct,
            )
        }),
        kind::BINARY_EXPRESSION => {
            // Binary expressions don't have named fields, get first named child (left operand)
            let mut cursor = node.walk();
            let left = node.children(&mut cursor).find(Node::is_named);
            left.and_then(|l| {
                infer_type(
                    l,
                    source,
                    api,
                    struct_fields,
                    user_structs,
                    local_types,
                    enclosing_struct,
                )
            })
        }
        _ => None,
    }
}

/// Infer the type of a field access expression (e.g., `obj.field`).
fn infer_field_access(
    node: Node,
    source: &str,
    api: &ApiDefinitions,
    struct_fields: &HashMap<String, HashMap<String, TypeInfo>>,
    user_structs: &HashMap<String, Option<String>>,
    local_types: &HashMap<String, TypeInfo>,
    enclosing_struct: Option<&str>,
) -> Option<TypeInfo> {
    let object = node.child_by_field("object")?;
    let field = node.child_by_field("field")?;
    let object_type = infer_type(
        object,
        source,
        api,
        struct_fields,
        user_structs,
        local_types,
        enclosing_struct,
    )?;
    let type_name = resolve_type_name(&object_type, enclosing_struct)?;
    let field_name = field.text(source);

    // Check user struct fields first
    if let Some(fields) = struct_fields.get(&type_name) {
        if let Some(field_type) = fields.get(field_name) {
            return Some(field_type.clone());
        }
    }

    // Check API type fields
    if let Some(type_def) = api.get_type(&type_name) {
        if let Some(field_def) = type_def.fields.get(field_name) {
            return Some(parse_type_string(&field_def.ty));
        }
    }

    None
}

/// Infer the return type of a call expression.
fn infer_call_expression(
    node: Node,
    source: &str,
    api: &ApiDefinitions,
    struct_fields: &HashMap<String, HashMap<String, TypeInfo>>,
    user_structs: &HashMap<String, Option<String>>,
    local_types: &HashMap<String, TypeInfo>,
    enclosing_struct: Option<&str>,
) -> Option<TypeInfo> {
    let callee = node.child_by_field("function")?;

    match callee.kind() {
        kind::IDENTIFIER => {
            // Global function call: foo(...)
            let name = callee.text(source);
            let func = api.get_function(name)?;
            func.return_type.as_ref().map(|t| parse_type_string(t))
        }
        kind::PATH_EXPRESSION => {
            infer_path_call_expression(callee, source, api, user_structs, enclosing_struct)
        }
        kind::FIELD_ACCESS => infer_method_call_expression(
            node,
            callee,
            source,
            api,
            struct_fields,
            user_structs,
            local_types,
            enclosing_struct,
        ),
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
    callee: Node,
    source: &str,
    api: &ApiDefinitions,
    user_structs: &HashMap<String, Option<String>>,
    _enclosing_struct: Option<&str>,
) -> Option<TypeInfo> {
    let text = callee.text(source);

    // Simple path without :: is a global function
    if !text.contains("::") {
        let func = api.get_function(text)?;
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
    if let Some(module) = api.get_module(prefix) {
        if let Some(func) = module.functions.iter().find(|f| f.name == name) {
            return func.return_type.as_ref().map(|t| parse_type_string(t));
        }
    }

    // Try type static method
    if let Some(type_def) = api.get_type(prefix) {
        if let Some(method) = type_def.methods.iter().find(|m| m.name == name) {
            return method.return_type.as_ref().map(|t| parse_type_string(t));
        }
    }

    // Check if prefix is a user-defined struct with static default methods (e.g., Task::new())
    if user_structs.contains_key(prefix) {
        if let Some(default_method) = api.get_default_struct_method(name) {
            if is_static_method(default_method) {
                return infer_default_method_return_type(
                    default_method,
                    prefix,
                    prefix,
                    user_structs,
                );
            }
        }
    }

    None
}

/// Infer return type for method calls (obj.method(...)).
fn infer_method_call_expression(
    call_node: Node,
    callee: Node,
    source: &str,
    api: &ApiDefinitions,
    struct_fields: &HashMap<String, HashMap<String, TypeInfo>>,
    user_structs: &HashMap<String, Option<String>>,
    local_types: &HashMap<String, TypeInfo>,
    enclosing_struct: Option<&str>,
) -> Option<TypeInfo> {
    let object = callee.child_by_field("object")?;
    let method = callee.child_by_field("field")?;

    let object_type = infer_type(
        object,
        source,
        api,
        struct_fields,
        user_structs,
        local_types,
        enclosing_struct,
    )?;

    let type_name = resolve_type_name(&object_type, enclosing_struct)?;
    let method_name = method.text(source);
    let base_type = base_type_name(&type_name);

    // Check for default instance methods (e.g., clone on private structs)
    if let Some(default_method) = api.get_default_struct_method(method_name) {
        if !is_static_method(default_method) {
            return infer_default_method_return_type(
                default_method,
                &type_name,
                &base_type,
                user_structs,
            );
        }
    }

    // Look up method in API type
    let type_def = api.get_type(&base_type)?;
    let method_def = type_def.methods.iter().find(|m| m.name == method_name)?;
    let return_type = method_def
        .return_type
        .as_ref()
        .map(|t| parse_type_string(t))?;

    // Handle generic type parameters
    let mut generic_args = extract_explicit_type_args(callee, source, api);

    // If no explicit type args, try to get from object type (e.g., std::optional<T>.get())
    if generic_args.is_empty() {
        generic_args = extract_generic_args(&object_type);
    }

    // If still no args and method has type_params, try to infer from arguments
    if generic_args.is_empty() && !method_def.type_params.is_empty() {
        generic_args = infer_type_params_from_args(
            call_node,
            method_def,
            source,
            api,
            struct_fields,
            user_structs,
            local_types,
        );
    }

    Some(substitute_type_params(return_type, &generic_args))
}

/// Infer the type of a number literal.
fn infer_number_type(node: Node, source: &str) -> TypeInfo {
    let text = node.text(source);
    if text.contains('.') {
        TypeInfo::F64
    } else {
        TypeInfo::I64
    }
}

/// Infer return type for a default struct method (new, clone).
fn infer_default_method_return_type(
    method: &FunctionDef,
    type_name: &str,
    base_type_name: &str,
    user_structs: &HashMap<String, Option<String>>,
) -> Option<TypeInfo> {
    let ret = method.return_type.as_ref()?;
    if ret == "Self" {
        Some(TypeInfo::Struct {
            name: type_name.to_string(),
            extends: user_structs.get(base_type_name).cloned().flatten(),
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
// Binding Operator Type Transformation
// ============================================================================

/// Transform a type based on the binding operator used.
///
/// When binding a pointer type with `&=` or `&mut=`, the result is a reference.
/// - `let x = *T`      → x has type `*T` (pointer, kept as-is)
/// - `let x &= *T`     → x has type `&T` (immutable reference)
/// - `let x &mut= *T`  → x has type `&mut T` (mutable reference)
/// - `let x mut= *T`   → x has type `*T` (mutable pointer binding)
///
/// For non-pointer types, the binding operator doesn't change the type.
pub fn transform_type_by_binding_operator(ty: TypeInfo, operator: &str) -> TypeInfo {
    match operator {
        "&=" => {
            // Convert pointer to immutable reference
            if let TypeInfo::Pointer { inner, .. } = ty {
                TypeInfo::Reference {
                    is_mut: false,
                    inner,
                }
            } else {
                ty
            }
        }
        "&mut=" => {
            // Convert pointer to mutable reference
            if let TypeInfo::Pointer { inner, .. } = ty {
                TypeInfo::Reference {
                    is_mut: true,
                    inner,
                }
            } else {
                ty
            }
        }
        // "=" and "mut=" keep the type as-is
        _ => ty,
    }
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
    source: &str,
    api: &ApiDefinitions,
) -> Vec<TypeInfo> {
    let Some(type_args_node) = field_access.child_by_field("type_arguments") else {
        return Vec::new();
    };

    let mut result = Vec::new();
    let mut cursor = type_args_node.walk();
    for child in type_args_node.children(&mut cursor) {
        if child.kind() == kind::TYPE_IDENTIFIER {
            let type_name = child.text(source);
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
    call_node: Node,
    method_def: &FunctionDef,
    source: &str,
    api: &ApiDefinitions,
    struct_fields: &HashMap<String, HashMap<String, TypeInfo>>,
    user_structs: &HashMap<String, Option<String>>,
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
            find_type_param_in_args(
                type_param,
                &args,
                method_def,
                source,
                api,
                struct_fields,
                user_structs,
                local_types,
            )
            .unwrap_or(TypeInfo::Unknown)
        })
        .collect()
}

/// Find a type parameter value by matching against argument types.
fn find_type_param_in_args(
    type_param: &str,
    args: &[Node],
    method_def: &FunctionDef,
    source: &str,
    api: &ApiDefinitions,
    struct_fields: &HashMap<String, HashMap<String, TypeInfo>>,
    user_structs: &HashMap<String, Option<String>>,
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
        let arg_type = infer_type(
            args[i],
            source,
            api,
            struct_fields,
            user_structs,
            local_types,
            None,
        )?;

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
pub fn extract_param(param_node: Node, source: &str) -> Option<(String, TypeInfo)> {
    let name = param_node.child_by_field("name")?.text(source).to_string();
    let ty = param_node.child_by_field("type")?;
    let type_info = parse_type_string(ty.text(source));
    Some((name, type_info))
}

/// Extract a parameter's name and type as strings from a PARAMETER node.
pub fn extract_param_strings(param_node: Node, source: &str) -> Option<(String, String)> {
    let name = param_node.child_by_field("name")?;
    let ty = param_node.child_by_field("type")?;
    Some((name.text(source).to_string(), ty.text(source).to_string()))
}

/// Extract all parameters from a PARAMETERS node.
pub fn extract_params(params_node: Node, source: &str) -> Vec<(String, TypeInfo)> {
    let mut cursor = params_node.walk();
    params_node
        .children(&mut cursor)
        .filter(|n| n.kind() == kind::PARAMETER)
        .filter_map(|p| extract_param(p, source))
        .collect()
}

/// Extract all parameters from a PARAMETERS node as (name, type) string pairs.
pub fn extract_params_strings(params_node: Node, source: &str) -> Vec<(String, String)> {
    let mut cursor = params_node.walk();
    params_node
        .children(&mut cursor)
        .filter(|c| c.kind() == kind::PARAMETER)
        .filter_map(|c| extract_param_strings(c, source))
        .collect()
}

/// Extract binding name and type from a LET_STATEMENT, LET_ELSE_STATEMENT, or IF_LET_STATEMENT node.
///
/// Returns the binding name and its type annotation if present, or Unknown if not.
pub fn extract_binding(let_node: Node, source: &str) -> Option<(String, TypeInfo)> {
    let binding = let_node.child_by_kind(kind::BINDING)?;
    let name = binding.child_by_field("name")?.text(source).to_string();
    // Check for type_pattern (which is how the grammar stores type annotations in bindings)
    let type_info = binding
        .child_by_kind("type_pattern")
        .map_or(TypeInfo::Unknown, |ty| parse_type_string(ty.text(source)));
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

/// Collect local variable types from a function node with type inference.
///
/// Collects parameter types and let binding types. If `cursor_offset` is provided,
/// only collects bindings declared before that offset.
pub fn collect_local_types_with_inference(
    func_node: Node,
    source: &str,
    api: &ApiDefinitions,
    struct_fields: &HashMap<String, HashMap<String, TypeInfo>>,
    user_structs: &HashMap<String, Option<String>>,
    cursor_offset: Option<usize>,
) -> HashMap<String, TypeInfo> {
    let mut types = HashMap::new();

    // Parameters
    if let Some(params) = func_node.child_by_kind(kind::PARAMETERS) {
        for (name, ty) in extract_params(params, source) {
            types.insert(name, ty);
        }
    }

    // Let bindings with inference
    let body = func_node
        .child_by_field("body")
        .or_else(|| func_node.child_by_kind(kind::BLOCK));
    if let Some(body) = body {
        collect_bindings_with_inference(
            body,
            source,
            api,
            struct_fields,
            user_structs,
            cursor_offset,
            &mut types,
        );
    }

    types
}

/// Recursively collect let bindings with type inference.
fn collect_bindings_with_inference(
    node: Node,
    source: &str,
    api: &ApiDefinitions,
    struct_fields: &HashMap<String, HashMap<String, TypeInfo>>,
    user_structs: &HashMap<String, Option<String>>,
    cursor_offset: Option<usize>,
    types: &mut HashMap<String, TypeInfo>,
) {
    // Skip if past cursor
    if let Some(offset) = cursor_offset {
        if node.start_byte() > offset {
            return;
        }
    }

    let is_binding_stmt = node.kind() == kind::LET_STATEMENT
        || node.kind() == kind::LET_ELSE_STATEMENT
        || node.kind() == kind::IF_LET_STATEMENT;

    if is_binding_stmt {
        if let Some(binding) = node.child_by_kind(kind::BINDING) {
            if let Some(name_node) = binding.child_by_field("name") {
                let name = name_node.text(source).to_string();

                // Get the binding operator to determine type transformation
                let operator = binding
                    .child_by_field("operator")
                    .map_or("=", |op| op.text(source));

                // First check for explicit type annotation (type_pattern)
                let type_info = if let Some(t) = binding.child_by_kind("type_pattern") {
                    parse_type_string(t.text(source))
                } else if let Some(value_node) = binding.child_by_field("value") {
                    // Infer type from the value expression
                    let inferred = infer_type(
                        value_node,
                        source,
                        api,
                        struct_fields,
                        user_structs,
                        types,
                        None,
                    )
                    .unwrap_or(TypeInfo::Unknown);

                    // Transform pointer to reference based on binding operator
                    transform_type_by_binding_operator(inferred, operator)
                } else {
                    TypeInfo::Unknown
                };

                types.insert(name, type_info);
            }
        }
    }

    // Recurse into children
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_bindings_with_inference(
            child,
            source,
            api,
            struct_fields,
            user_structs,
            cursor_offset,
            types,
        );
    }
}

/// Collect local variable types from a function node (without inference).
///
/// Only uses explicit type annotations.
pub fn collect_local_types_in_function(
    func_node: Node,
    source: &str,
    cursor_offset: Option<usize>,
) -> HashMap<String, TypeInfo> {
    let mut types = HashMap::new();

    // Parameters
    if let Some(params) = func_node.child_by_kind(kind::PARAMETERS) {
        for (name, ty) in extract_params(params, source) {
            types.insert(name, ty);
        }
    }

    // Let bindings (no inference)
    let body = func_node
        .child_by_field("body")
        .or_else(|| func_node.child_by_kind(kind::BLOCK));
    if let Some(body) = body {
        collect_bindings_recursive(body, source, cursor_offset, &mut types);
    }

    types
}

/// Recursively collect let bindings from a node (no inference).
fn collect_bindings_recursive(
    node: Node,
    source: &str,
    cursor_offset: Option<usize>,
    types: &mut HashMap<String, TypeInfo>,
) {
    if let Some(offset) = cursor_offset {
        if node.start_byte() > offset {
            return;
        }
    }

    let is_binding_stmt = node.kind() == kind::LET_STATEMENT
        || node.kind() == kind::LET_ELSE_STATEMENT
        || node.kind() == kind::IF_LET_STATEMENT;
    if is_binding_stmt {
        if let Some((name, ty)) = extract_binding(node, source) {
            types.insert(name, ty);
        }
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_bindings_recursive(child, source, cursor_offset, types);
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
pub fn get_enclosing_struct_name(func_node: Node, source: &str) -> Option<String> {
    let name_node = func_node.child_by_field("name")?;
    let name = name_node.text(source);
    let pos = name.find("::")?;
    Some(name[..pos].to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::api::ApiDefinitions;
    use nimbyscript_parser::parse;

    fn load_api() -> ApiDefinitions {
        let toml = include_str!("../../../api-definitions/nimbyrails.v1.toml");
        ApiDefinitions::load_from_str(toml).expect("should parse")
    }

    fn find_node<'a>(
        node: nimbyscript_parser::Node<'a>,
        kind: &str,
    ) -> Option<nimbyscript_parser::Node<'a>> {
        if node.kind() == kind {
            return Some(node);
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(found) = find_node(child, kind) {
                return Some(found);
            }
        }
        None
    }

    #[test]
    fn test_infer_db_view_returns_pointer() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal {
    probe: ID<Signal>,
}
fn Test::event_signal_pass_by(self: &Test, ctx: &EventCtx, train: &Train, motion: &Motion) {
    let sig = ctx.db.view(self.probe) else { return; };
}
";
        let tree = parse(source);
        let api = load_api();
        let mut struct_fields = HashMap::new();
        let mut test_fields = HashMap::new();
        test_fields.insert(
            "probe".to_string(),
            TypeInfo::Generic {
                name: "ID".to_string(),
                args: vec![TypeInfo::Struct {
                    name: "Signal".to_string(),
                    extends: None,
                }],
            },
        );
        struct_fields.insert("Test".to_string(), test_fields);
        let user_structs = HashMap::from([("Test".to_string(), Some("Signal".to_string()))]);

        let root = tree.root_node();
        let func = find_node(root, "function_definition").expect("function");

        // Build local types from parameters
        let local_types = collect_local_types_with_inference(
            func,
            source,
            &api,
            &struct_fields,
            &user_structs,
            None,
        );

        // Find the value expression (ctx.db.view(self.probe))
        let let_else = find_node(root, "let_else_statement").expect("let_else");
        let binding = let_else.child_by_kind("binding").expect("binding");
        let value = binding.child_by_field("value").expect("value");

        let value_type = infer_type(
            value,
            source,
            &api,
            &struct_fields,
            &user_structs,
            &local_types,
            Some("Test"),
        );

        let ty = value_type.expect("Should infer type");
        assert!(
            ty.is_pointer(),
            "ctx.db.view() should return a pointer type, got: {ty}"
        );
    }

    #[test]
    fn test_infer_number_types() {
        let source = "42";
        let tree = parse(source);
        let root = tree.root_node();
        let num = find_deepest_node_at(root, 0).expect("number");

        let api = load_api();
        let ty = infer_type(
            num,
            source,
            &api,
            &HashMap::new(),
            &HashMap::new(),
            &HashMap::new(),
            None,
        );
        assert_eq!(ty, Some(TypeInfo::I64));

        let source = "3.14";
        let tree = parse(source);
        let root = tree.root_node();
        let num = find_deepest_node_at(root, 0).expect("number");

        let ty = infer_type(
            num,
            source,
            &api,
            &HashMap::new(),
            &HashMap::new(),
            &HashMap::new(),
            None,
        );
        assert_eq!(ty, Some(TypeInfo::F64));
    }

    #[test]
    fn test_binding_operator_ref_converts_pointer_to_reference() {
        // Test that &= converts pointer to reference
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal {
    probe: ID<Signal>,
}
pub fn Test::tick(self: &Test, ctx: &EventCtx) {
    let sig &= ctx.db.view(self.probe) else { return; };
}
";
        let tree = parse(source);
        let api = load_api();
        let mut struct_fields = HashMap::new();
        let mut test_fields = HashMap::new();
        test_fields.insert(
            "probe".to_string(),
            TypeInfo::Generic {
                name: "ID".to_string(),
                args: vec![TypeInfo::Struct {
                    name: "Signal".to_string(),
                    extends: None,
                }],
            },
        );
        struct_fields.insert("Test".to_string(), test_fields);
        let user_structs = HashMap::from([("Test".to_string(), Some("Signal".to_string()))]);

        let root = tree.root_node();
        let func = find_node(root, "function_definition").expect("function");

        // Collect local types - this should transform the pointer to a reference
        let local_types = collect_local_types_with_inference(
            func,
            source,
            &api,
            &struct_fields,
            &user_structs,
            None,
        );

        // The variable 'sig' should be a reference, not a pointer
        let sig_type = local_types
            .get("sig")
            .expect("sig should be in local_types");
        assert!(
            sig_type.is_reference(),
            "sig should be a reference type when using &=, got: {sig_type}"
        );
        assert!(
            !sig_type.is_pointer(),
            "sig should NOT be a pointer when using &="
        );
    }

    #[test]
    fn test_binding_operator_equals_keeps_pointer() {
        // Test that = keeps pointer as pointer
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal {
    probe: ID<Signal>,
}
pub fn Test::tick(self: &Test, ctx: &EventCtx) {
    let sig = ctx.db.view(self.probe) else { return; };
}
";
        let tree = parse(source);
        let api = load_api();
        let mut struct_fields = HashMap::new();
        let mut test_fields = HashMap::new();
        test_fields.insert(
            "probe".to_string(),
            TypeInfo::Generic {
                name: "ID".to_string(),
                args: vec![TypeInfo::Struct {
                    name: "Signal".to_string(),
                    extends: None,
                }],
            },
        );
        struct_fields.insert("Test".to_string(), test_fields);
        let user_structs = HashMap::from([("Test".to_string(), Some("Signal".to_string()))]);

        let root = tree.root_node();
        let func = find_node(root, "function_definition").expect("function");

        // Collect local types - this should keep the pointer as-is
        let local_types = collect_local_types_with_inference(
            func,
            source,
            &api,
            &struct_fields,
            &user_structs,
            None,
        );

        // The variable 'sig' should remain a pointer
        let sig_type = local_types
            .get("sig")
            .expect("sig should be in local_types");
        assert!(
            sig_type.is_pointer(),
            "sig should be a pointer type when using =, got: {sig_type}"
        );
    }
}
