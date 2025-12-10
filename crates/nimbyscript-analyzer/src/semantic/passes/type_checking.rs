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
        let mut local_types: HashMap<String, TypeInfo> = HashMap::new();
        check_types(root, ctx, diagnostics, &mut local_types);
    }
}

fn check_types(
    node: Node,
    ctx: &SemanticContext,
    diagnostics: &mut Vec<Diagnostic>,
    local_types: &mut HashMap<String, TypeInfo>,
) {
    match node.kind() {
        kind::FUNCTION_DEFINITION => {
            // Enter a new local scope for this function
            let mut func_local_types: HashMap<String, TypeInfo> = HashMap::new();

            // Add parameters to local scope
            if let Some(params) = node.child_by_kind(kind::PARAMETERS) {
                let mut cursor = params.walk();
                for param in params.children(&mut cursor) {
                    if param.kind() == kind::PARAMETER {
                        if let (Some(name_node), Some(type_node)) =
                            (param.child_by_field("name"), param.child_by_field("type"))
                        {
                            let param_name = name_node.text(ctx.source).to_string();
                            let param_type = ctx.resolve_type(type_node.text(ctx.source));
                            func_local_types.insert(param_name, param_type);
                        }
                    }
                }
            }

            // Check the function body with the local scope
            if let Some(body) = node.child_by_kind(kind::BLOCK) {
                check_block(body, ctx, diagnostics, &mut func_local_types);
            }
        }
        _ => {
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                check_types(child, ctx, diagnostics, local_types);
            }
        }
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
                            .map(|t| ctx.resolve_type(t.text(ctx.source)))
                            .unwrap_or(TypeInfo::Unknown);
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
        kind::CALL_EXPRESSION => {
            // Recurse into arguments
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                check_expression(child, ctx, diagnostics, local_types);
            }
        }
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
                        format!(
                            "Undefined field '{}' on type '{}'{}",
                            field_name, name, hint
                        ),
                        Span::new(field_node.start_byte(), field_node.end_byte()),
                    )
                    .with_code("E0303"),
                );
            }
        } else {
            // Type has no known fields
            diagnostics.push(
                Diagnostic::error(
                    format!("Type '{}' has no fields", name),
                    Span::new(field_node.start_byte(), field_node.end_byte()),
                )
                .with_code("E0303"),
            );
        }
    } else {
        // Not a struct type
        diagnostics.push(
            Diagnostic::error(
                format!(
                    "Cannot access field '{}' on non-struct type '{}'",
                    field_name, base_type
                ),
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
                        "Cannot mix i64 and f64 in arithmetic operation '{}'. Use explicit conversion.",
                        op
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
            let cond_type = infer_expr_type(Some(cond), ctx, local_types);
            if !cond_type.is_bool() && !cond_type.is_unknown() {
                diagnostics.push(
                    Diagnostic::error(
                        format!("Condition must be bool, found '{}'", cond_type),
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
    let actual_type = node
        .child_by_field("value")
        .map(|v| infer_expr_type(Some(v), ctx, local_types))
        .unwrap_or(TypeInfo::Void);

    // Check compatibility
    if let Some(expected) = expected_type {
        if !actual_type.is_assignable_to(&expected) && !actual_type.is_unknown() && !expected.is_unknown() {
            diagnostics.push(
                Diagnostic::error(
                    format!(
                        "Return type mismatch: expected '{}', found '{}'",
                        expected, actual_type
                    ),
                    Span::new(node.start_byte(), node.end_byte()),
                )
                .with_code("E0408"),
            );
        }
    }
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
                .map(|f| f.text(ctx.source))
                .unwrap_or("");

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
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    let x: i64 = 5;
    let y: f64 = 3.0;
    let z = x + y;
}
"#;
        let diags = check(source);
        let errs = errors(&diags);
        // This should produce E0402
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0402")),
            "Should error on i64 + f64: {:?}",
            errs
        );
    }

    #[test]
    fn test_same_numeric_type_ok() {
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    let x: i64 = 5;
    let y: i64 = 3;
    let z = x + y;
}
"#;
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0402")),
            "Same type arithmetic should be ok: {:?}",
            errs
        );
    }

    // E0405 - Integer division

    #[test]
    fn test_integer_division() {
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    let x: i64 = 10;
    let y: i64 = 3;
    let z = x / y;
}
"#;
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0405")),
            "Integer division should error: {:?}",
            errs
        );
    }

    // E0406 - Integer remainder

    #[test]
    fn test_integer_remainder() {
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    let x: i64 = 10;
    let y: i64 = 3;
    let z = x % y;
}
"#;
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0406")),
            "Integer remainder should error: {:?}",
            errs
        );
    }

    // E0409 - Condition must be bool

    #[test]
    fn test_non_bool_condition() {
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    let x: i64 = 5;
    if x {
        return;
    }
}
"#;
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0409")),
            "Non-bool condition should error: {:?}",
            errs
        );
    }

    #[test]
    fn test_bool_condition_ok() {
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    let x: bool = true;
    if x {
        return;
    }
}
"#;
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0409")),
            "Bool condition should be ok: {:?}",
            errs
        );
    }

    // E0303 - Undefined field

    #[test]
    fn test_undefined_field_on_user_struct() {
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct MyHandler extend Signal {
    count: i64,
}
fn MyHandler::test(self: &MyHandler) {
    let x = self.nonexistent;
}
"#;
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0303")),
            "Should error on undefined field: {:?}",
            errs
        );
    }

    #[test]
    fn test_valid_field_access() {
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct MyHandler extend Signal {
    count: i64,
}
fn MyHandler::test(self: &MyHandler) {
    let x = self.count;
}
"#;
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0303")),
            "Valid field should not error: {:?}",
            errs
        );
    }

    #[test]
    fn test_undefined_field_on_game_type() {
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test(sig: &Signal) {
    let x = sig.nonexistent_field;
}
"#;
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0303")),
            "Should error on undefined game type field: {:?}",
            errs
        );
    }

    #[test]
    fn test_chained_field_access() {
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test(train: &Train) {
    let x = train.motion.nonexistent;
}
"#;
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0303")),
            "Should error on undefined field in chain: {:?}",
            errs
        );
    }
}
