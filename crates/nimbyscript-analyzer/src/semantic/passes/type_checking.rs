//! Type checking pass (E04xx).
//!
//! Validates type compatibility:
//! - E0400: Type mismatch (general)
//! - E0401: Cannot assign type to variable
//! - E0402: Cannot mix i64 and f64 in arithmetic
//! - E0403: Function argument type mismatch
//! - E0404: Binary operator type incompatibility
//! - E0405: Integer division forbidden (use zdiv)
//! - E0406: Integer remainder forbidden (use zmod)
//! - E0407: Cannot compare types
//! - E0408: Return type mismatch
//! - E0409: Condition must be bool

use std::collections::HashMap;

use nimbyscript_parser::ast::Span;
use nimbyscript_parser::{kind, Node, NodeExt};

use crate::semantic::{SemanticContext, SemanticPass};
use crate::types::TypeInfo;
use crate::Diagnostic;

/// Pass that validates type compatibility
pub struct TypeCheckingPass;

impl SemanticPass for TypeCheckingPass {
    fn name(&self) -> &'static str {
        "type_checking"
    }

    fn run(&self, ctx: &mut SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
        let root = ctx.tree.root_node();
        check_types(root, ctx, diagnostics);
    }
}

fn check_types(node: Node, ctx: &SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    if node.kind() == kind::FUNCTION_DEFINITION {
        // Enter a new local scope for this function
        let mut func_local_types: HashMap<String, TypeInfo> = HashMap::new();

        // Add parameters to local scope
        if let Some(params) = node.child_by_kind(kind::PARAMETERS) {
            collect_params_types(params, ctx, &mut func_local_types);
        }

        // Check the function body with the local scope
        if let Some(body) = node.child_by_kind(kind::BLOCK) {
            check_block(body, ctx, diagnostics, &mut func_local_types);
        }
    } else {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            check_types(child, ctx, diagnostics);
        }
    }
}

fn collect_params_types(
    params: Node,
    ctx: &SemanticContext,
    local_types: &mut HashMap<String, TypeInfo>,
) {
    let mut cursor = params.walk();
    for param in params.children(&mut cursor) {
        if param.kind() != kind::PARAMETER {
            continue;
        }
        let Some(name_node) = param.child_by_field("name") else {
            continue;
        };
        let Some(type_node) = param.child_by_field("type") else {
            continue;
        };
        let param_name = name_node.text(ctx.source).to_string();
        let param_type = ctx.resolve_type(type_node.text(ctx.source));
        local_types.insert(param_name, param_type);
    }
}

fn check_block(
    node: Node,
    ctx: &SemanticContext,
    diagnostics: &mut Vec<Diagnostic>,
    local_types: &mut HashMap<String, TypeInfo>,
) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            kind::LET_STATEMENT | kind::LET_ELSE_STATEMENT => {
                // The let_statement has a binding child which contains name, type, and value
                if let Some(binding) = child.child_by_kind("binding") {
                    // First check the value expression
                    if let Some(value) = binding.child_by_field("value") {
                        check_expression(value, ctx, diagnostics, local_types);
                    }

                    // Add the variable to local scope
                    if let Some(name_node) = binding.child_by_field("name") {
                        let var_name = name_node.text(ctx.source).to_string();
                        // Look for type_pattern child (the type annotation)
                        let var_type = binding
                            .child_by_kind("type_pattern")
                            .map_or(TypeInfo::Unknown, |t| ctx.resolve_type(t.text(ctx.source)));
                        local_types.insert(var_name, var_type);
                    }
                }
            }
            kind::IF_STATEMENT | kind::IF_LET_STATEMENT => {
                check_condition(child, ctx, diagnostics, local_types);
            }
            kind::FOR_STATEMENT => {
                // Add loop variable to scope and check body
                let mut loop_types = local_types.clone();
                if let Some(var_node) = child.child_by_field("variable") {
                    let var_name = var_node.text(ctx.source).to_string();
                    loop_types.insert(var_name, TypeInfo::Unknown);
                }
                if let Some(body) = child.child_by_kind(kind::BLOCK) {
                    check_block(body, ctx, diagnostics, &mut loop_types);
                }
            }
            kind::BLOCK => {
                // Nested block - create new scope
                let mut block_types = local_types.clone();
                check_block(child, ctx, diagnostics, &mut block_types);
            }
            kind::RETURN_STATEMENT => {
                check_return(child, ctx, diagnostics, local_types);
            }
            kind::EXPRESSION_STATEMENT => {
                check_expression_statement(child, ctx, diagnostics, local_types);
            }
            _ => {
                check_expression(child, ctx, diagnostics, local_types);
            }
        }
    }
}

fn check_expression(
    node: Node,
    ctx: &SemanticContext,
    diagnostics: &mut Vec<Diagnostic>,
    local_types: &HashMap<String, TypeInfo>,
) {
    match node.kind() {
        kind::BINARY_EXPRESSION => check_binary_expr(node, ctx, diagnostics, local_types),
        kind::CALL_EXPRESSION => check_call_expression(node, ctx, diagnostics, local_types),
        kind::FIELD_ACCESS => check_field_access(node, ctx, diagnostics, local_types),
        _ => {
            // Recurse into children
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                check_expression(child, ctx, diagnostics, local_types);
            }
        }
    }
}

fn check_call_expression(
    node: Node,
    ctx: &SemanticContext,
    diagnostics: &mut Vec<Diagnostic>,
    local_types: &HashMap<String, TypeInfo>,
) {
    // Get the function name
    let func_name = if let Some(callee) = node.child_by_field("function") {
        let name = callee.text(ctx.source);
        // For method calls like module::func, we'll skip validation for now
        if name.contains("::") {
            // Recurse into arguments and return
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                check_expression(child, ctx, diagnostics, local_types);
            }
            return;
        }
        name.to_string()
    } else {
        return;
    };

    // Count arguments
    let mut arg_count = 0;
    let mut args_node = None;
    if let Some(args) = node.child_by_kind("arguments") {
        args_node = Some(args);
        let mut cursor = args.walk();
        for child in args.named_children(&mut cursor) {
            // Count expression children (skip punctuation)
            if child.is_named() && child.kind() != "," {
                arg_count += 1;
            }
        }
    }

    // Get function parameter info
    if let Some(func_info) = ctx.get_function_params(&func_name) {
        // Check argument count
        if arg_count < func_info.min_params {
            let span = args_node.map_or_else(
                || Span::new(node.start_byte(), node.end_byte()),
                |a| Span::new(a.start_byte(), a.end_byte()),
            );

            diagnostics.push(
                Diagnostic::error(
                    format!(
                        "Function '{}' expects {} argument(s), but {} provided",
                        func_name, func_info.min_params, arg_count
                    ),
                    span,
                )
                .with_code("E0403"),
            );
        } else if arg_count > func_info.max_params {
            let span = args_node.map_or_else(
                || Span::new(node.start_byte(), node.end_byte()),
                |a| Span::new(a.start_byte(), a.end_byte()),
            );

            diagnostics.push(
                Diagnostic::error(
                    format!(
                        "Function '{}' expects {} argument(s), but {} provided",
                        func_name, func_info.max_params, arg_count
                    ),
                    span,
                )
                .with_code("E0403"),
            );
        }

        // Check argument types
        if let Some(args) = args_node {
            let mut cursor = args.walk();
            let arg_exprs: Vec<_> = args
                .named_children(&mut cursor)
                .filter(|c| c.is_named() && c.kind() != ",")
                .collect();

            for (i, arg_expr) in arg_exprs.iter().enumerate() {
                if i >= func_info.param_types.len() {
                    break; // Extra args already handled by count check
                }

                let expected_type_str = &func_info.param_types[i];
                let expected_type = ctx.resolve_type(expected_type_str);

                // Skip if expected type is unknown or generic
                if expected_type.is_unknown() || expected_type_str.contains('T') {
                    continue;
                }

                let actual_type = infer_expr_type(Some(*arg_expr), ctx, local_types);

                // Skip if actual type is unknown (avoid cascading errors)
                if actual_type.is_unknown() {
                    continue;
                }

                if !actual_type.is_assignable_to(&expected_type) {
                    diagnostics.push(
                        Diagnostic::error(
                            format!(
                                "Type mismatch: expected '{expected_type}', found '{actual_type}'"
                            ),
                            Span::new(arg_expr.start_byte(), arg_expr.end_byte()),
                        )
                        .with_code("E0404"),
                    );
                }
            }
        }
    }

    // Recurse into arguments to check their expressions
    // Skip the "function" child if it's a field_access (method call) to avoid
    // false positives from field validation on method names
    let func_child = node.child_by_field("function");
    let skip_func = func_child.is_some_and(|f| f.kind() == kind::FIELD_ACCESS);

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        // Skip function child if it's a method call (field_access)
        if skip_func && Some(child) == func_child {
            // Validate the method call
            check_method_call(child, node, ctx, diagnostics, local_types);
            continue;
        }
        check_expression(child, ctx, diagnostics, local_types);
    }
}

fn check_method_call(
    field_access: Node,
    call_node: Node,
    ctx: &SemanticContext,
    diagnostics: &mut Vec<Diagnostic>,
    local_types: &HashMap<String, TypeInfo>,
) {
    // 1. Get the object and method name
    let Some(object) = field_access.child_by_field("object") else {
        return;
    };
    let Some(method_node) = field_access.child_by_field("field") else {
        return;
    };
    let method_name = method_node.text(ctx.source);

    // 2. Check the object expression recursively
    check_expression(object, ctx, diagnostics, local_types);

    // 3. Infer the object's type
    let object_type = infer_expr_type(Some(object), ctx, local_types);
    let base_type = object_type.unwrap_ref();

    // 4. Skip if type is unknown (avoid cascading errors)
    if base_type.is_unknown() {
        return;
    }

    // 5. Get the type name - handle both Struct and Generic types
    // For generics, we need different strategies:
    // - ID<Signal>: lookup methods on "ID<Signal>" in API
    // - std::optional<T>: lookup methods on "std::optional", fallback to T
    let (base_type_name, full_type_name, inner_type) = match &base_type {
        TypeInfo::Struct { name, .. } => (name.as_str(), None, None),
        TypeInfo::Generic { name, args } => {
            // Build full type name for display/API lookup (e.g., "ID<Signal>")
            let full_name = args.first().and_then(|t| {
                if let TypeInfo::Struct { name: arg_name, .. } = t {
                    Some(format!("{name}<{arg_name}>"))
                } else {
                    None
                }
            });

            // For std::optional<T>, we can fall back to methods on T
            let inner = if name == "std::optional" {
                args.first().and_then(|t| {
                    if let TypeInfo::Struct { name, .. } = t {
                        Some(name.as_str())
                    } else {
                        None
                    }
                })
            } else {
                None
            };
            (name.as_str(), full_name, inner)
        }
        _ => return, // Can't call methods on non-struct types
    };

    // Determine lookup strategy based on type:
    // - For std::optional<T>: look up on "std::optional" first, then fallback to T
    // - For other generics like ID<T>: look up on "ID<T>" (full type name)
    // - For plain structs: look up on the struct name
    let is_optional = base_type_name == "std::optional";
    let lookup_type = if is_optional {
        base_type_name // Look up on "std::optional"
    } else {
        full_type_name.as_deref().unwrap_or(base_type_name) // Look up on "ID<Signal>" or plain name
    };

    // 6. Check if method exists on the type (or inner type for optionals)
    let method_def = ctx.get_type_method(lookup_type, method_name).or_else(|| {
        // If not found on the outer type, try the inner type (for optionals)
        inner_type.and_then(|inner| ctx.get_type_method(inner, method_name))
    });

    // For error messages, use the full type name for generics
    let display_type = full_type_name.as_deref().unwrap_or(base_type_name);

    // Determine effective type for error messages - which type did we find the method on?
    let effective_type = if ctx.get_type_method(lookup_type, method_name).is_some() {
        display_type
    } else {
        inner_type.unwrap_or(display_type)
    };

    // Check for default struct methods on private user structs
    let is_private_struct =
        ctx.is_user_struct(effective_type) && !ctx.is_pub_struct(effective_type);
    if method_def.is_none() && is_private_struct && ctx.is_default_struct_method(method_name) {
        // Valid default method on a private struct (e.g., clone), no further validation needed
        return;
    }

    if let Some(method_def) = method_def {
        // Method exists - validate argument count
        let expected_params = method_def.params.len();
        let actual_args = count_arguments(call_node);

        if actual_args != expected_params {
            let args_node = call_node.child_by_kind("arguments");
            let span = args_node.map_or_else(
                || Span::new(call_node.start_byte(), call_node.end_byte()),
                |a| Span::new(a.start_byte(), a.end_byte()),
            );

            diagnostics.push(
                Diagnostic::error(
                    format!(
                        "Method '{method_name}' on type '{effective_type}' expects {expected_params} argument(s), but {actual_args} provided"
                    ),
                    span,
                )
                .with_code("E0403"),
            );
        }
    } else {
        // Method doesn't exist - check if it might be a field
        if ctx
            .get_struct_fields(effective_type)
            .is_some_and(|f| f.contains_key(method_name))
        {
            // It's a field, not a method - different error
            diagnostics.push(
                Diagnostic::error(
                    format!("'{method_name}' is a field, not a method, on type '{effective_type}'"),
                    Span::new(method_node.start_byte(), method_node.end_byte()),
                )
                .with_code("E0305"),
            );
        } else {
            // Get available methods for hint
            let available: Vec<_> = ctx
                .get_type_methods(lookup_type)
                .iter()
                .map(|(name, _)| *name)
                .take(5)
                .collect();
            let hint = if available.is_empty() {
                String::new()
            } else {
                format!(". Available methods: {}", available.join(", "))
            };

            diagnostics.push(
                Diagnostic::error(
                    format!("Undefined method '{method_name}' on type '{effective_type}'{hint}"),
                    Span::new(method_node.start_byte(), method_node.end_byte()),
                )
                .with_code("E0305"),
            );
        }
    }
}

fn count_arguments(call_node: Node) -> usize {
    if let Some(args) = call_node.child_by_kind("arguments") {
        let mut cursor = args.walk();
        args.named_children(&mut cursor)
            .filter(|c| c.is_named() && c.kind() != ",")
            .count()
    } else {
        0
    }
}

fn check_expression_statement(
    node: Node,
    ctx: &SemanticContext,
    diagnostics: &mut Vec<Diagnostic>,
    local_types: &HashMap<String, TypeInfo>,
) {
    // Find the expression inside the expression_statement
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        // Skip semicolons and other punctuation
        if !child.is_named() {
            continue;
        }

        // Check if this expression has side effects
        // Only function calls have side effects; field access, identifiers, literals,
        // and binary expressions without assignment have no side effects
        let has_side_effects = child.kind() == kind::CALL_EXPRESSION;

        if !has_side_effects {
            diagnostics.push(
                Diagnostic::warning(
                    "Expression statement has no effect",
                    Span::new(child.start_byte(), child.end_byte()),
                )
                .with_code("W0400"),
            );
        }

        // Still check the expression for other errors
        check_expression(child, ctx, diagnostics, local_types);
        return;
    }
}

fn check_field_access(
    node: Node,
    ctx: &SemanticContext,
    diagnostics: &mut Vec<Diagnostic>,
    local_types: &HashMap<String, TypeInfo>,
) {
    // 1. Get and check the object
    let Some(object) = node.child_by_field("object") else {
        return;
    };
    check_expression(object, ctx, diagnostics, local_types);

    // 2. Infer the object's type
    let object_type = infer_expr_type(Some(object), ctx, local_types);
    let base_type = object_type.unwrap_ref();

    // 3. Get the field node
    let Some(field_node) = node.child_by_field("field") else {
        return;
    };
    let field_name = field_node.text(ctx.source);

    // 4. Skip validation if type is unknown (avoid cascading errors)
    if base_type.is_unknown() {
        return;
    }

    // 5. Check field exists
    if let TypeInfo::Struct { name, .. } = &base_type {
        if let Some(fields) = ctx.get_struct_fields(name) {
            if !fields.contains_key(field_name) {
                let available: Vec<_> = fields.keys().take(5).cloned().collect();
                let hint = if available.is_empty() {
                    String::new()
                } else {
                    format!(". Available fields: {}", available.join(", "))
                };

                diagnostics.push(
                    Diagnostic::error(
                        format!("Undefined field '{field_name}' on type '{name}'{hint}"),
                        Span::new(field_node.start_byte(), field_node.end_byte()),
                    )
                    .with_code("E0303"),
                );
            }
        } else {
            // Type has no known fields
            diagnostics.push(
                Diagnostic::error(
                    format!("Type '{name}' has no fields"),
                    Span::new(field_node.start_byte(), field_node.end_byte()),
                )
                .with_code("E0303"),
            );
        }
    } else {
        // Not a struct type
        diagnostics.push(
            Diagnostic::error(
                format!("Cannot access field '{field_name}' on non-struct type '{base_type}'"),
                Span::new(node.start_byte(), node.end_byte()),
            )
            .with_code("E0303"),
        );
    }
}

fn check_binary_expr(
    node: Node,
    ctx: &SemanticContext,
    diagnostics: &mut Vec<Diagnostic>,
    local_types: &HashMap<String, TypeInfo>,
) {
    // Get the operator
    let op_text = get_binary_operator(node, ctx.source);

    // Check for integer division/remainder
    if op_text == "/" || op_text == "%" {
        check_integer_division(node, &op_text, ctx, diagnostics, local_types);
    }

    // Check arithmetic type mixing
    if matches!(op_text.as_str(), "+" | "-" | "*" | "/" | "%") {
        check_arithmetic_types(node, &op_text, ctx, diagnostics, local_types);
    }

    // Recurse into operands
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        check_expression(child, ctx, diagnostics, local_types);
    }
}

fn get_binary_operator(node: Node, source: &str) -> String {
    // Find operator token between left and right operands
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        let text = child.text(source);
        if matches!(
            text,
            "+" | "-" | "*" | "/" | "%" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "&&" | "||"
        ) {
            return text.to_string();
        }
    }
    String::new()
}

fn check_integer_division(
    node: Node,
    op: &str,
    ctx: &SemanticContext,
    diagnostics: &mut Vec<Diagnostic>,
    local_types: &HashMap<String, TypeInfo>,
) {
    // Binary expression children: [left operand, operator, right operand]
    let left_type = infer_expr_type(node.child(0), ctx, local_types);
    let right_type = infer_expr_type(node.child(2), ctx, local_types);

    // If either operand is an integer, warn about integer division
    if left_type.is_integer() || right_type.is_integer() {
        if op == "/" {
            diagnostics.push(
                Diagnostic::error(
                    "Integer division is forbidden in NimbyScript. Use zdiv() for integer division.".to_string(),
                    Span::new(node.start_byte(), node.end_byte()),
                )
                .with_code("E0405"),
            );
        } else if op == "%" {
            diagnostics.push(
                Diagnostic::error(
                    "Integer remainder is forbidden in NimbyScript. Use zmod() for integer remainder.".to_string(),
                    Span::new(node.start_byte(), node.end_byte()),
                )
                .with_code("E0406"),
            );
        }
    }
}

fn check_arithmetic_types(
    node: Node,
    op: &str,
    ctx: &SemanticContext,
    diagnostics: &mut Vec<Diagnostic>,
    local_types: &HashMap<String, TypeInfo>,
) {
    // Binary expression children: [left operand, operator, right operand]
    let left_type = infer_expr_type(node.child(0), ctx, local_types);
    let right_type = infer_expr_type(node.child(2), ctx, local_types);

    // Check if types are compatible for arithmetic
    if !left_type.can_arithmetic_with(&right_type) {
        // Only report if both types are known and numeric
        if (left_type.is_integer() && right_type.is_float())
            || (left_type.is_float() && right_type.is_integer())
        {
            diagnostics.push(
                Diagnostic::error(
                    format!(
                        "Cannot mix i64 and f64 in arithmetic operation '{op}'. Use explicit conversion."
                    ),
                    Span::new(node.start_byte(), node.end_byte()),
                )
                .with_code("E0402"),
            );
        }
    }
}

fn check_condition(
    node: Node,
    ctx: &SemanticContext,
    diagnostics: &mut Vec<Diagnostic>,
    local_types: &mut HashMap<String, TypeInfo>,
) {
    // Check that condition is bool (for if statements)
    if node.kind() == kind::IF_STATEMENT {
        if let Some(cond) = node.child_by_field("condition") {
            // Check the condition expression for method/field errors
            check_expression(cond, ctx, diagnostics, local_types);

            let cond_type = infer_expr_type(Some(cond), ctx, local_types);
            if !cond_type.is_bool() && !cond_type.is_unknown() {
                diagnostics.push(
                    Diagnostic::error(
                        format!("Condition must be bool, found '{cond_type}'"),
                        Span::new(cond.start_byte(), cond.end_byte()),
                    )
                    .with_code("E0409"),
                );
            }
        }
    }

    // Check then block
    if let Some(then_block) = node.child_by_field("consequence") {
        let mut then_types = local_types.clone();
        check_block(then_block, ctx, diagnostics, &mut then_types);
    }

    // Check else clause
    if let Some(else_clause) = node.child_by_kind(kind::ELSE_CLAUSE) {
        let mut cursor = else_clause.walk();
        for child in else_clause.children(&mut cursor) {
            if child.kind() == kind::BLOCK {
                let mut else_types = local_types.clone();
                check_block(child, ctx, diagnostics, &mut else_types);
            } else if child.kind() == kind::IF_STATEMENT || child.kind() == kind::IF_LET_STATEMENT {
                check_condition(child, ctx, diagnostics, local_types);
            }
        }
    }
}

fn check_return(
    node: Node,
    ctx: &SemanticContext,
    diagnostics: &mut Vec<Diagnostic>,
    local_types: &HashMap<String, TypeInfo>,
) {
    // Get the expected return type from enclosing function
    let expected_type = ctx.scopes.enclosing_return_type().cloned();

    // Get the actual return value type
    let actual_type = node.child_by_field("value").map_or(TypeInfo::Void, |v| {
        infer_expr_type(Some(v), ctx, local_types)
    });

    // Check compatibility
    if let Some(expected) = expected_type {
        if !actual_type.is_assignable_to(&expected)
            && !actual_type.is_unknown()
            && !expected.is_unknown()
        {
            diagnostics.push(
                Diagnostic::error(
                    format!("Return type mismatch: expected '{expected}', found '{actual_type}'"),
                    Span::new(node.start_byte(), node.end_byte()),
                )
                .with_code("E0408"),
            );
        }
    }
}

/// Infer the return type of a method call, handling generic type parameters.
/// For example, `ctx.db.view<Hitcher>(id)` returns `*Hitcher`.
fn infer_method_call_type(
    field_access: Node,
    ctx: &SemanticContext,
    local_types: &HashMap<String, TypeInfo>,
) -> TypeInfo {
    // 1. Get the object and method name
    let Some(object) = field_access.child_by_field("object") else {
        return TypeInfo::Unknown;
    };
    let method_name = field_access
        .child_by_field("field")
        .map_or("", |f| f.text(ctx.source));

    // 2. Infer the object's type
    let object_type = infer_expr_type(Some(object), ctx, local_types);
    let base_type = object_type.unwrap_ref();

    // 3. Get the type name
    let type_name = match &base_type {
        TypeInfo::Struct { name, .. } | TypeInfo::Generic { name, .. } => name.as_str(),
        _ => return TypeInfo::Unknown,
    };

    // 4. Look up the method
    let Some(method_def) = ctx.get_type_method(type_name, method_name) else {
        return TypeInfo::Unknown;
    };

    // 5. Get the return type
    let Some(return_type_str) = &method_def.return_type else {
        return TypeInfo::Void;
    };

    // 6. If the method has type parameters, substitute them with provided type arguments
    if !method_def.type_params.is_empty() {
        // Extract type arguments from the field_access node (e.g., <Hitcher>)
        if let Some(type_args_node) = field_access.child_by_field("type_arguments") {
            // Parse the type arguments
            let mut type_args = Vec::new();
            let mut cursor = type_args_node.walk();
            for child in type_args_node.children(&mut cursor) {
                if child.kind() == kind::TYPE_IDENTIFIER {
                    let type_arg = child.text(ctx.source);
                    type_args.push(type_arg.to_string());
                }
            }

            // Substitute type parameters in the return type
            let mut substituted = return_type_str.clone();
            for (param, arg) in method_def.type_params.iter().zip(type_args.iter()) {
                substituted = substituted.replace(param, arg);
            }

            return ctx.resolve_type(&substituted);
        }
    }

    ctx.resolve_type(return_type_str)
}

/// Type inference for expressions with local variable tracking.
fn infer_expr_type(
    node: Option<Node>,
    ctx: &SemanticContext,
    local_types: &HashMap<String, TypeInfo>,
) -> TypeInfo {
    let Some(node) = node else {
        return TypeInfo::Unknown;
    };

    match node.kind() {
        kind::NUMBER => {
            let text = node.text(ctx.source);
            if text.contains('.') {
                TypeInfo::F64
            } else {
                TypeInfo::I64
            }
        }
        kind::BOOLEAN => TypeInfo::Bool,
        kind::STRING_LITERAL => TypeInfo::String,
        kind::IDENTIFIER | kind::PATH_EXPRESSION => {
            let name = node.text(ctx.source);
            // Check if it's a qualified path (e.g., Enum::Variant)
            if name.contains("::") {
                // It's a qualified name - check for enum variant
                if let Some((enum_name, _variant)) = name.split_once("::") {
                    if ctx.is_game_enum(enum_name) || ctx.is_user_enum(enum_name) {
                        return TypeInfo::enum_type(enum_name);
                    }
                }
                return TypeInfo::Unknown;
            }
            // Simple identifier - check local variables first
            if let Some(ty) = local_types.get(name) {
                return ty.clone();
            }
            // Then check global scope
            if let Some(sym) = ctx.scopes.lookup(name) {
                sym.type_info.clone()
            } else {
                TypeInfo::Unknown
            }
        }
        kind::BINARY_EXPRESSION => {
            let op = get_binary_operator(node, ctx.source);
            match op.as_str() {
                "==" | "!=" | "<" | ">" | "<=" | ">=" | "&&" | "||" => TypeInfo::Bool,
                "+" | "-" | "*" | "/" | "%" => {
                    // Return the type of the left operand (simplified)
                    // Binary expression children: [left, operator, right]
                    infer_expr_type(node.child(0), ctx, local_types)
                }
                _ => TypeInfo::Unknown,
            }
        }
        kind::UNARY_EXPRESSION => {
            let text = node.text(ctx.source);
            if text.starts_with('!') {
                TypeInfo::Bool
            } else {
                infer_expr_type(node.children(&mut node.walk()).last(), ctx, local_types)
            }
        }
        kind::CALL_EXPRESSION => {
            if let Some(callee) = node.child_by_field("function") {
                // Check if this is a method call (callee is field_access)
                if callee.kind() == kind::FIELD_ACCESS {
                    return infer_method_call_type(callee, ctx, local_types);
                }

                let func_name = callee.text(ctx.source);

                // Check user functions
                if let Some(ret_type) = ctx.user_functions.get(func_name) {
                    return ret_type.clone().unwrap_or(TypeInfo::Void);
                }

                // Check API functions
                if let Some(func) = ctx.get_global_function(func_name) {
                    if let Some(ret) = &func.return_type {
                        return ctx.resolve_type(ret);
                    }
                }
            }
            TypeInfo::Unknown
        }
        kind::PARENTHESIZED_EXPRESSION => {
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if child.kind() != "(" && child.kind() != ")" {
                    return infer_expr_type(Some(child), ctx, local_types);
                }
            }
            TypeInfo::Unknown
        }
        kind::FIELD_ACCESS => {
            // 1. Get the object expression
            let object_type = if let Some(obj) = node.child_by_field("object") {
                infer_expr_type(Some(obj), ctx, local_types)
            } else {
                return TypeInfo::Unknown;
            };

            // 2. Unwrap references/pointers to get the underlying type
            let base_type = object_type.unwrap_ref();

            // 3. Get the field name
            let field_name = node
                .child_by_field("field")
                .map_or("", |f| f.text(ctx.source));

            // 4. Look up field type
            if let TypeInfo::Struct { name, .. } = &base_type {
                if let Some(fields) = ctx.get_struct_fields(name) {
                    if let Some(field_type) = fields.get(field_name) {
                        return field_type.clone();
                    }
                }
            }

            TypeInfo::Unknown
        }
        _ => TypeInfo::Unknown,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::api::ApiDefinitions;
    use crate::diagnostics::Severity;
    use crate::semantic::context::collect_declarations;
    use nimbyscript_parser::parse;

    fn load_api() -> ApiDefinitions {
        let toml = include_str!("../../../../../api-definitions/nimbyrails.v1.toml");
        ApiDefinitions::load_from_str(toml).expect("should parse")
    }

    fn check(source: &str) -> Vec<Diagnostic> {
        let tree = parse(source);
        let api = load_api();
        let mut ctx = SemanticContext::new(source, &tree, &api);
        collect_declarations(&mut ctx);
        let mut diagnostics = Vec::new();
        TypeCheckingPass.run(&mut ctx, &mut diagnostics);
        diagnostics
    }

    fn errors(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
        diags
            .iter()
            .filter(|d| matches!(d.severity, Severity::Error))
            .collect()
    }

    // E0402 - Cannot mix i64 and f64

    #[test]
    fn test_mixed_numeric_types() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    let x: i64 = 5;
    let y: f64 = 3.0;
    let z = x + y;
}
";
        let diags = check(source);
        let errs = errors(&diags);
        // This should produce E0402
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0402")),
            "Should error on i64 + f64: {errs:?}"
        );
    }

    #[test]
    fn test_same_numeric_type_ok() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    let x: i64 = 5;
    let y: i64 = 3;
    let z = x + y;
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0402")),
            "Same type arithmetic should be ok: {errs:?}"
        );
    }

    // E0405 - Integer division

    #[test]
    fn test_integer_division() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    let x: i64 = 10;
    let y: i64 = 3;
    let z = x / y;
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0405")),
            "Integer division should error: {errs:?}"
        );
    }

    // E0406 - Integer remainder

    #[test]
    fn test_integer_remainder() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    let x: i64 = 10;
    let y: i64 = 3;
    let z = x % y;
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0406")),
            "Integer remainder should error: {errs:?}"
        );
    }

    // E0409 - Condition must be bool

    #[test]
    fn test_non_bool_condition() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    let x: i64 = 5;
    if x {
        return;
    }
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0409")),
            "Non-bool condition should error: {errs:?}"
        );
    }

    #[test]
    fn test_bool_condition_ok() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    let x: bool = true;
    if x {
        return;
    }
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0409")),
            "Bool condition should be ok: {errs:?}"
        );
    }

    // E0303 - Undefined field

    #[test]
    fn test_undefined_field_on_user_struct() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct MyHandler extend Signal {
    count: i64,
}
fn MyHandler::test(self: &MyHandler) {
    let x = self.nonexistent;
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0303")),
            "Should error on undefined field: {errs:?}"
        );
    }

    #[test]
    fn test_valid_field_access() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct MyHandler extend Signal {
    count: i64,
}
fn MyHandler::test(self: &MyHandler) {
    let x = self.count;
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0303")),
            "Valid field should not error: {errs:?}"
        );
    }

    #[test]
    fn test_undefined_field_on_game_type() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test(sig: &Signal) {
    let x = sig.nonexistent_field;
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0303")),
            "Should error on undefined game type field: {errs:?}"
        );
    }

    #[test]
    fn test_chained_field_access() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test(train: &Train) {
    let x = train.motion.nonexistent;
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0303")),
            "Should error on undefined field in chain: {errs:?}"
        );
    }

    #[test]
    fn test_undefined_field_in_argument() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct MyHandler extend Signal {
    owner: ID<Train>,
}
fn MyHandler::test(self: &MyHandler) {
    let x = max(1, self.owne);
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0303")),
            "Should error on undefined field in argument: {errs:?}"
        );
    }

    // E0403 - Wrong number of arguments

    #[test]
    fn test_too_few_arguments() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    max(1);
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0403")),
            "Should error on too few arguments: {errs:?}"
        );
    }

    #[test]
    fn test_too_many_arguments() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    max(1, 2, 3);
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0403")),
            "Should error on too many arguments: {errs:?}"
        );
    }

    #[test]
    fn test_correct_argument_count() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    let x = max(1, 2);
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0403")),
            "Correct argument count should not error: {errs:?}"
        );
    }

    #[test]
    fn test_zero_argument_function() {
        // abs() takes 1 argument
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    abs();
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0403")),
            "Should error on zero arguments when 1 expected: {errs:?}"
        );
    }

    // W0400 - Expression has no effect

    fn warnings(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
        diags
            .iter()
            .filter(|d| matches!(d.severity, Severity::Warning))
            .collect()
    }

    #[test]
    fn test_identifier_expression_no_effect() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    max;
}
";
        let diags = check(source);
        let warns = warnings(&diags);
        assert!(
            warns.iter().any(|d| d.code.as_deref() == Some("W0400")),
            "Should warn on identifier with no effect: {warns:?}"
        );
    }

    #[test]
    fn test_literal_expression_no_effect() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    42;
}
";
        let diags = check(source);
        let warns = warnings(&diags);
        assert!(
            warns.iter().any(|d| d.code.as_deref() == Some("W0400")),
            "Should warn on literal with no effect: {warns:?}"
        );
    }

    #[test]
    fn test_function_call_has_effect() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    max(1, 2);
}
";
        let diags = check(source);
        let warns = warnings(&diags);
        assert!(
            warns.iter().all(|d| d.code.as_deref() != Some("W0400")),
            "Function call should not warn about no effect: {warns:?}"
        );
    }

    // E0305 - Undefined method

    #[test]
    fn test_undefined_method() {
        // db is a field on ControlCtx, so we need ctx.db.method()
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test(ctx: &ControlCtx) {
    ctx.db.vie();
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0305")),
            "Should error on undefined method: {errs:?}"
        );
    }

    #[test]
    fn test_valid_method_call() {
        // db is a field on ControlCtx, so we need ctx.db.method()
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test(ctx: &ControlCtx) {
    let v = ctx.db.view();
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0305")),
            "Valid method should not error: {errs:?}"
        );
    }

    #[test]
    fn test_method_wrong_arg_count_on_db() {
        // db is a field on ControlCtx, so we need ctx.db.method()
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test(ctx: &ControlCtx) {
    let v = ctx.db.view(1, 2);
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0403")),
            "Wrong arg count on method should error: {errs:?}"
        );
    }

    #[test]
    fn test_method_wrong_arg_count() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test(sig: &Signal) {
    let p = sig.forward(1, 2);
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0403")),
            "Wrong arg count on method should error: {errs:?}"
        );
    }

    #[test]
    fn test_undefined_method_on_signal() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test(sig: &Signal) {
    let x = sig.undefined_method();
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0305")),
            "Should error on undefined method: {errs:?}"
        );
    }

    #[test]
    fn test_method_on_signal() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test(sig: &Signal) {
    let p = sig.forward();
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0305")),
            "Valid Signal method should not error: {errs:?}"
        );
    }

    #[test]
    fn test_field_called_as_method() {
        // Signal has `id` field which should not be callable as a method
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test(sig: &Signal) {
    let x = sig.id();
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0305")),
            "Calling field as method should error: {errs:?}"
        );
    }

    // E0404 - Argument type mismatch

    #[test]
    fn test_wrong_argument_type() {
        // abs() expects f64, but we pass a string
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    let x = abs("hello");
}
"#;
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0404")),
            "Wrong argument type should error: {errs:?}"
        );
    }

    #[test]
    fn test_correct_argument_type() {
        // abs() expects f64, we pass an integer literal (compatible)
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    let x = abs(42);
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0404")),
            "Correct argument type should not error: {errs:?}"
        );
    }

    #[test]
    fn test_wrong_argument_type_bool_to_f64() {
        // max() expects f64, but we pass bools
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    let x = max(true, false);
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0404")),
            "Bool to f64 type mismatch should error: {errs:?}"
        );
    }

    #[test]
    fn test_is_valid_accepts_generic() {
        // is_valid() accepts generic type, should not error
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test(sig: &Signal) {
    let v = is_valid(sig.id);
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0404")),
            "Generic function should accept any type: {errs:?}"
        );
    }

    #[test]
    fn test_undefined_method_with_db_param() {
        // When db is a function parameter with type &DB, calling db.vie() should error
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct MyHandler extend Signal { }
fn MyHandler::test(self: &MyHandler, db: &DB) {
    db.vie();
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0305")),
            "Should error on undefined method 'vie' on DB param: {errs:?}"
        );
    }

    #[test]
    fn test_valid_method_with_db_param() {
        // When db is a function parameter with type &DB, calling db.view() should work
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct MyHandler extend Signal { }
fn MyHandler::test(self: &MyHandler, db: &DB) {
    db.view();
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0305")),
            "Valid DB method should not error: {errs:?}"
        );
    }
}
