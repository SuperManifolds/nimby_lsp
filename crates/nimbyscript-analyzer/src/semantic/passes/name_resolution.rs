//! Name resolution pass (E03xx).
//!
//! Validates that all referenced names are defined:
//! - E0300: Undefined variable
//! - E0301: Undefined function
//! - E0302: Undefined type
//! - E0303: Undefined field on type
//! - E0304: Undefined enum variant
//! - E0305: Undefined method on type
//! - E0306: Undefined module

use nimbyscript_parser::ast::Span;
use nimbyscript_parser::{kind, Node, NodeExt};

use crate::scope::{ScopeKind, SymbolKind};
use crate::semantic::{SemanticContext, SemanticPass};
use crate::types::TypeInfo;
use crate::Diagnostic;

/// Pass that validates all name references
pub struct NameResolutionPass;

impl SemanticPass for NameResolutionPass {
    fn name(&self) -> &'static str {
        "name_resolution"
    }

    fn run(&self, ctx: &mut SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
        let root = ctx.tree.root_node();
        resolve_names(root, ctx, diagnostics);
    }
}

fn resolve_names(node: Node, ctx: &mut SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    match node.kind() {
        kind::FUNCTION_DEFINITION => resolve_function(node, ctx, diagnostics),
        kind::TYPE => resolve_type(node, ctx, diagnostics),
        kind::PATH_EXPRESSION => resolve_path(node, ctx, diagnostics),
        kind::CALL_EXPRESSION => resolve_call(node, ctx, diagnostics),
        kind::FIELD_ACCESS => resolve_field_access(node, ctx, diagnostics),
        kind::IDENTIFIER => resolve_identifier(node, ctx, diagnostics),
        _ => {
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                resolve_names(child, ctx, diagnostics);
            }
        }
    }
}

fn resolve_function(node: Node, ctx: &mut SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    // Get function name and return type
    let return_type = node
        .child_by_field("return_type")
        .map(|rt| ctx.resolve_type(rt.text(ctx.source)));

    let func_name = node
        .child_by_field("name")
        .map(|n| n.text(ctx.source).to_string())
        .unwrap_or_default();

    // Enter function scope
    let body_span = node
        .child_by_kind(kind::BLOCK)
        .map(|b| Span::new(b.start_byte(), b.end_byte()))
        .unwrap_or_else(|| Span::new(node.start_byte(), node.end_byte()));

    ctx.scopes.enter_scope(
        ScopeKind::Function {
            name: func_name.clone(),
            return_type,
        },
        body_span,
    );

    // Add parameters to scope and check their types
    if let Some(params) = node.child_by_kind(kind::PARAMETERS) {
        let mut cursor = params.walk();
        for param in params.children(&mut cursor) {
            if param.kind() == kind::PARAMETER {
                // Get name and type - type might be a field or a child
                let name_node = param.child_by_field("name");
                let type_node = {
                    let field_type = param.child_by_field("type");
                    if field_type.is_some() {
                        field_type
                    } else {
                        // Try finding type as a named child
                        let mut tc = param.walk();
                        let type_child = param
                            .named_children(&mut tc)
                            .find(|c| c.kind() == kind::TYPE);
                        type_child
                    }
                };

                if let (Some(name_node), Some(type_node)) = (name_node, type_node) {
                    let param_name = name_node.text(ctx.source);
                    let type_text = type_node.text(ctx.source);

                    // Check if type exists (E0302)
                    check_type_reference(type_node, ctx, diagnostics);

                    let param_type = ctx.resolve_type(type_text);

                    let _ = ctx.scopes.define(
                        param_name,
                        SymbolKind::Parameter,
                        param_type,
                        Span::new(param.start_byte(), param.end_byte()),
                        Span::new(name_node.start_byte(), name_node.end_byte()),
                        false,
                    );
                }
            }
        }
    }

    // Process function body
    if let Some(body) = node.child_by_kind(kind::BLOCK) {
        resolve_block(body, ctx, diagnostics);
    }

    // Exit function scope
    ctx.scopes.exit_scope();
}

fn resolve_block(node: Node, ctx: &mut SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        resolve_statement(child, ctx, diagnostics);
    }
}

fn resolve_statement(node: Node, ctx: &mut SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    match node.kind() {
        kind::LET_STATEMENT | kind::LET_ELSE_STATEMENT => {
            resolve_let(node, ctx, diagnostics);
        }
        kind::FOR_STATEMENT => {
            resolve_for(node, ctx, diagnostics);
        }
        kind::IF_STATEMENT | kind::IF_LET_STATEMENT => {
            resolve_if(node, ctx, diagnostics);
        }
        kind::BLOCK => {
            // Enter block scope
            ctx.scopes
                .enter_scope(ScopeKind::Block, Span::new(node.start_byte(), node.end_byte()));
            resolve_block(node, ctx, diagnostics);
            ctx.scopes.exit_scope();
        }
        _ => {
            // For other statements, just resolve child expressions
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                resolve_names(child, ctx, diagnostics);
            }
        }
    }
}

fn resolve_let(node: Node, ctx: &mut SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    // Get the binding child which contains name and value fields
    let binding = node.child_by_kind("binding");

    // First resolve the value expression
    if let Some(value) = binding.and_then(|b| b.child_by_field("value")) {
        resolve_names(value, ctx, diagnostics);
    }

    // Then add the variable to scope
    if let Some(name_node) = binding.and_then(|b| b.child_by_field("name")) {
        let var_name = name_node.text(ctx.source);

        let var_type = binding
            .and_then(|b| b.child_by_field("type"))
            .map(|t| ctx.resolve_type(t.text(ctx.source)))
            .unwrap_or(TypeInfo::Unknown);

        // Check for 'mut' keyword in binding operator
        let is_mutable = node.text(ctx.source).contains("mut=");

        let _ = ctx.scopes.define(
            var_name,
            SymbolKind::Variable,
            var_type,
            Span::new(node.start_byte(), node.end_byte()),
            Span::new(name_node.start_byte(), name_node.end_byte()),
            is_mutable,
        );
    }

    // Handle let-else
    if node.kind() == kind::LET_ELSE_STATEMENT {
        if let Some(else_block) = node.child_by_kind(kind::BLOCK) {
            ctx.scopes.enter_scope(
                ScopeKind::Block,
                Span::new(else_block.start_byte(), else_block.end_byte()),
            );
            resolve_block(else_block, ctx, diagnostics);
            ctx.scopes.exit_scope();
        }
    }
}

fn resolve_for(node: Node, ctx: &mut SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    // Resolve the iterator expression first
    if let Some(iter) = node.child_by_field("iterator") {
        resolve_names(iter, ctx, diagnostics);
    }

    // Enter loop scope
    ctx.scopes
        .enter_scope(ScopeKind::Loop, Span::new(node.start_byte(), node.end_byte()));

    // Add loop variable to scope
    if let Some(var_node) = node.child_by_field("variable") {
        let var_name = var_node.text(ctx.source);
        let _ = ctx.scopes.define(
            var_name,
            SymbolKind::Variable,
            TypeInfo::Unknown, // Type inference would determine this
            Span::new(var_node.start_byte(), var_node.end_byte()),
            Span::new(var_node.start_byte(), var_node.end_byte()),
            false,
        );
    }

    // Resolve body
    if let Some(body) = node.child_by_kind(kind::BLOCK) {
        resolve_block(body, ctx, diagnostics);
    }

    ctx.scopes.exit_scope();
}

fn resolve_if(node: Node, ctx: &mut SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    // Resolve condition (for regular if statements)
    if let Some(cond) = node.child_by_field("condition") {
        resolve_names(cond, ctx, diagnostics);
    }

    // For if-let, handle the variable binding (name/value are in the binding child)
    let binding = if node.kind() == kind::IF_LET_STATEMENT {
        node.child_by_kind("binding")
    } else {
        None
    };

    if let Some(value) = binding.and_then(|b| b.child_by_field("value")) {
        resolve_names(value, ctx, diagnostics);
    }

    // Resolve then block
    if let Some(then_block) = node.child_by_field("consequence") {
        ctx.scopes.enter_scope(
            ScopeKind::Block,
            Span::new(then_block.start_byte(), then_block.end_byte()),
        );

        // For if-let, add the bound variable
        if let Some(name_node) = binding.and_then(|b| b.child_by_field("name")) {
            let var_name = name_node.text(ctx.source);
            let var_type = binding
                .and_then(|b| b.child_by_field("type"))
                .map(|t| ctx.resolve_type(t.text(ctx.source)))
                .unwrap_or(TypeInfo::Unknown);

            let _ = ctx.scopes.define(
                var_name,
                SymbolKind::Variable,
                var_type,
                Span::new(name_node.start_byte(), name_node.end_byte()),
                Span::new(name_node.start_byte(), name_node.end_byte()),
                false,
            );
        }

        resolve_block(then_block, ctx, diagnostics);
        ctx.scopes.exit_scope();
    }

    // Resolve else clause
    if let Some(else_clause) = node.child_by_kind(kind::ELSE_CLAUSE) {
        let mut cursor = else_clause.walk();
        for child in else_clause.children(&mut cursor) {
            resolve_statement(child, ctx, diagnostics);
        }
    }
}

fn resolve_type(node: Node, ctx: &SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    // Get the type identifier
    let type_text = node.text(ctx.source).trim();

    // Skip primitive types and references/pointers
    if type_text.starts_with('&') || type_text.starts_with('*') {
        // Check the inner type
        if let Some(inner) = node.child_by_kind(kind::TYPE) {
            resolve_type(inner, ctx, diagnostics);
        } else if let Some(type_id) = node.child_by_kind(kind::TYPE_IDENTIFIER) {
            check_type_exists(type_id.text(ctx.source), type_id, ctx, diagnostics);
        }
        return;
    }

    if let Some(type_id) = node.child_by_kind(kind::TYPE_IDENTIFIER) {
        let type_name = type_id.text(ctx.source);
        check_type_exists(type_name, type_id, ctx, diagnostics);
    }

    // Check generic arguments
    if let Some(generic_args) = node.child_by_kind(kind::GENERIC_ARGUMENTS) {
        let mut cursor = generic_args.walk();
        for arg in generic_args.children(&mut cursor) {
            if arg.kind() == kind::TYPE {
                resolve_type(arg, ctx, diagnostics);
            }
        }
    }
}

/// Check a type reference node and emit E0302 if the type doesn't exist
fn check_type_reference(node: Node, ctx: &SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    let type_text = node.text(ctx.source).trim();

    // Handle references and pointers
    let base_type = type_text
        .trim_start_matches('&')
        .trim_start_matches("mut ")
        .trim_start_matches('*')
        .trim();

    // For ID<T> types, check the full type name first (e.g., "ID<Signal>")
    // since they are defined as complete types in the API
    if base_type.starts_with("ID<") && base_type.ends_with('>') {
        // Check if the full ID<T> type exists
        if ctx.is_game_type(base_type) {
            return;
        }
        // If not found as a complete type, check inner type at least
        let inner = &base_type[3..base_type.len() - 1];
        check_type_exists(inner, node, ctx, diagnostics);
        return;
    }

    // Extract generic base type (e.g., "Option<Signal>" -> "Option")
    let (outer_type, inner_type) = if let Some(idx) = base_type.find('<') {
        let outer = &base_type[..idx];
        let inner = base_type[idx + 1..].trim_end_matches('>');
        (outer, Some(inner))
    } else {
        (base_type, None)
    };

    // Check outer type
    check_type_exists(outer_type, node, ctx, diagnostics);

    // Check inner type if present
    if let Some(inner) = inner_type {
        check_type_exists(inner, node, ctx, diagnostics);
    }
}

fn check_type_exists(name: &str, node: Node, ctx: &SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    // Skip primitive types
    if matches!(
        name,
        "bool" | "i64" | "i32" | "i16" | "i8" | "u64" | "u32" | "u16" | "u8" | "f64" | "f32"
            | "String" | "string" | "Self"
    ) {
        return;
    }

    // Skip std:: types
    if name.starts_with("std::") {
        return;
    }

    // Skip built-in generic wrapper types (ID<T> for entity references)
    if name == "ID" {
        return;
    }

    // Check if type exists
    if ctx.is_game_type(name) || ctx.is_game_enum(name) || ctx.is_user_struct(name) || ctx.is_user_enum(name) {
        return;
    }

    // Type not found
    diagnostics.push(
        Diagnostic::error(
            format!("Undefined type '{}'", name),
            Span::new(node.start_byte(), node.end_byte()),
        )
        .with_code("E0302"),
    );
}

fn resolve_path(node: Node, ctx: &SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    let path_text = node.text(ctx.source);

    // Handle module::function or Enum::Variant or Type::method
    if let Some((first, second)) = path_text.split_once("::") {
        // Check if first is a module
        if ctx.is_module(first) {
            // Check if function exists in module
            if ctx.get_module_function(first, second).is_none() {
                diagnostics.push(
                    Diagnostic::error(
                        format!("Undefined function '{}' in module '{}'", second, first),
                        Span::new(node.start_byte(), node.end_byte()),
                    )
                    .with_code("E0301"),
                );
            }
        } else if let Some(variants) = ctx.get_enum_variants(first) {
            // Check if it's an enum variant
            if !variants.contains(&second.to_string()) {
                diagnostics.push(
                    Diagnostic::error(
                        format!("Undefined variant '{}' for enum '{}'", second, first),
                        Span::new(node.start_byte(), node.end_byte()),
                    )
                    .with_code("E0304"),
                );
            }
        } else if ctx.is_game_type(first) || ctx.is_user_struct(first) {
            // Type::method call (e.g., ID<Train>::empty(), Task::new())
            // For now, just accept it - method validation can be done separately
        } else if first.starts_with("ID<") && first.ends_with('>') {
            // Handle generic ID types like ID<Train>::empty()
            // These are valid if the full type exists
            if !ctx.is_game_type(first) {
                diagnostics.push(
                    Diagnostic::error(
                        format!("Undefined type '{}'", first),
                        Span::new(node.start_byte(), node.end_byte()),
                    )
                    .with_code("E0302"),
                );
            }
        } else {
            // Unknown path prefix
            diagnostics.push(
                Diagnostic::error(
                    format!("Undefined module or enum '{}'", first),
                    Span::new(node.start_byte(), node.end_byte()),
                )
                .with_code("E0306"),
            );
        }
    } else {
        // Simple path expression (no ::) - treat as variable reference
        // Check if it's defined in any known scope
        let name = path_text;

        // Check local scope
        if ctx.scopes.lookup(name).is_some() {
            return;
        }

        // Check if it's a user-defined function
        if ctx.user_functions.contains_key(name) {
            return;
        }

        // Check if it's a global API function
        if ctx.get_global_function(name).is_some() {
            return;
        }

        // Check if it's a built-in function
        if is_builtin_function(name) {
            return;
        }

        // Check if it's a game type
        if ctx.api.get_type(name).is_some() {
            return;
        }

        // Check if it's a user-defined type
        if ctx.is_user_struct(name) || ctx.is_user_enum(name) {
            return;
        }

        // Check if it's a game enum
        if ctx.is_game_enum(name) {
            return;
        }

        // Unknown identifier
        diagnostics.push(
            Diagnostic::error(
                format!("Undefined variable '{}'", name),
                Span::new(node.start_byte(), node.end_byte()),
            )
            .with_code("E0302"),
        );
    }
}

fn resolve_call(node: Node, ctx: &mut SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    // Resolve the callee
    if let Some(callee) = node.child_by_field("function") {
        let callee_text = callee.text(ctx.source);

        // Check if it's a simple function call (path_expression without ::)
        if (callee.kind() == kind::IDENTIFIER || callee.kind() == kind::PATH_EXPRESSION)
            && !callee_text.contains("::")
        {
            let func_name = callee_text;

            // Check if it's a user-defined function, global API function, or built-in
            if !ctx.user_functions.contains_key(func_name)
                && ctx.get_global_function(func_name).is_none()
                && !is_builtin_function(func_name)
            {
                // Check if it might be a variable (callable)
                if ctx.scopes.lookup(func_name).is_none() {
                    diagnostics.push(
                        Diagnostic::error(
                            format!("Undefined function '{}'", func_name),
                            Span::new(callee.start_byte(), callee.end_byte()),
                        )
                        .with_code("E0301"),
                    );
                }
            }
        } else {
            // For other callee types (path with ::, field access), recurse
            resolve_names(callee, ctx, diagnostics);
        }
    }

    // Resolve arguments
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() != kind::IDENTIFIER && child.kind() != kind::PATH_EXPRESSION {
            // Skip the function name
            resolve_names(child, ctx, diagnostics);
        }
    }
}

fn resolve_field_access(node: Node, ctx: &mut SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    // Resolve the object expression for name resolution
    if let Some(object) = node.child_by_field("object") {
        resolve_names(object, ctx, diagnostics);
    }
    // Field validation is handled by type_checking pass (E0303)
}

fn resolve_identifier(node: Node, ctx: &SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    // Only check identifiers that are being used, not defined
    // Skip if parent is a definition context
    if let Some(parent) = node.parent() {
        match parent.kind() {
            // Skip field definitions, parameter names, variable bindings, etc.
            kind::STRUCT_FIELD
            | kind::ENUM_VARIANT
            | kind::PARAMETER
            | kind::FUNCTION_NAME
            | kind::FOR_STATEMENT => return,
            // Skip binding names (let x = ..., if let x = ...)
            // The binding contains name/value fields; skip the name identifier
            "binding" => {
                if parent.child_by_field("name").map(|n| n.id()) == Some(node.id()) {
                    return;
                }
            }
            // Skip if this is the "field" part of a field access (not the object)
            kind::FIELD_ACCESS => {
                if parent.child_by_field("field").map(|f| f.id()) == Some(node.id()) {
                    return;
                }
            }
            _ => {}
        }
    }

    let name = node.text(ctx.source);

    // Check if it's in local scope
    if ctx.scopes.lookup(name).is_some() {
        return;
    }

    // Check if it's a user-defined function
    if ctx.user_functions.contains_key(name) {
        return;
    }

    // Check if it's a global API function
    if ctx.get_global_function(name).is_some() {
        return;
    }

    // Check if it's a built-in function
    if is_builtin_function(name) {
        return;
    }

    // Check if it's a game type
    if ctx.api.get_type(name).is_some() {
        return;
    }

    // Check if it's a user-defined type
    if ctx.is_user_struct(name) || ctx.is_user_enum(name) {
        return;
    }

    // Check if it's a game enum
    if ctx.is_game_enum(name) {
        return;
    }

    // Unknown identifier
    diagnostics.push(
        Diagnostic::error(
            format!("Undefined variable '{}'", name),
            Span::new(node.start_byte(), node.end_byte()),
        )
        .with_code("E0302"),
    );
}

fn is_builtin_function(name: &str) -> bool {
    matches!(
        name,
        "abs" | "sqrt" | "sin" | "cos" | "tan" | "floor" | "ceil" | "round" | "min" | "max"
            | "zdiv" | "zmod" | "clamp" | "lerp"
    )
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
        NameResolutionPass.run(&mut ctx, &mut diagnostics);
        diagnostics
    }

    fn errors(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
        diags
            .iter()
            .filter(|d| matches!(d.severity, Severity::Error))
            .collect()
    }

    // E0302 - Undefined type

    #[test]
    fn test_undefined_type() {
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test(x: NonExistentType) { }
"#;
        let diags = check(source);
        let errs = errors(&diags);
        assert!(errs.iter().any(|d| d.code.as_deref() == Some("E0302")));
    }

    #[test]
    fn test_valid_game_type() {
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test(x: &Signal) { }
"#;
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0302")),
            "Game type should be valid: {:?}",
            errs
        );
    }

    #[test]
    fn test_valid_user_struct() {
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct MyStruct extend Signal { }
fn test(x: &MyStruct) { }
"#;
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0302")),
            "User struct should be valid: {:?}",
            errs
        );
    }

    // E0304 - Undefined enum variant

    #[test]
    fn test_undefined_enum_variant() {
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test(): SignalCheck {
    return SignalCheck::NotAVariant;
}
"#;
        let diags = check(source);
        let errs = errors(&diags);
        assert!(errs.iter().any(|d| d.code.as_deref() == Some("E0304")));
    }

    #[test]
    fn test_valid_enum_variant() {
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test(): SignalCheck {
    return SignalCheck::Pass;
}
"#;
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0304")),
            "Valid variant should not error: {:?}",
            errs
        );
    }

    // E0306 - Undefined module

    #[test]
    fn test_valid_module() {
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    let t = Sim::time();
}
"#;
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0306")),
            "Valid module should not error: {:?}",
            errs
        );
    }

    // E0301 - Undefined function

    #[test]
    fn test_undefined_function() {
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    is_vali();
}
"#;
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0301")),
            "Undefined function should error: {:?}",
            errs
        );
    }

    #[test]
    fn test_valid_api_function() {
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    let x = abs(-5);
}
"#;
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0301")),
            "Valid API function should not error: {:?}",
            errs
        );
    }

    #[test]
    fn test_undefined_function_in_method() {
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct MyHandler extend Signal {
    owner: ID<Train>,
}
fn MyHandler::test(self: &MyHandler) {
    if is_vali(1) {
        return;
    }
}
"#;
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0301")),
            "Undefined function in method should error: {:?}",
            errs
        );
    }
}
