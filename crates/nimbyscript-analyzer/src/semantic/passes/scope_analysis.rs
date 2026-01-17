//! Scope analysis pass (E06xx).
//!
//! Analyzes scope and usage patterns:
//! - W0600: Variable shadows existing binding
//! - W0601: Unused variable
//! - W0602: Unused function
//! - E0603: Use of variable before definition
//! - W0604: Unused parameter
//! - W0605: Unused struct
//! - W0606: Unused enum
//! - W0607: Possibly missing `pub` on callback function

use nimbyscript_parser::ast::Span;
use nimbyscript_parser::{kind, Node, NodeExt};

use crate::scope::{ScopeKind, ScopedSymbol, SymbolKind};
use crate::semantic::{SemanticContext, SemanticPass};
use crate::types::TypeInfo;
use crate::Diagnostic;

/// Pass that analyzes scope and usage patterns
pub struct ScopeAnalysisPass;

impl SemanticPass for ScopeAnalysisPass {
    fn name(&self) -> &'static str {
        "scope_analysis"
    }

    fn run(&self, ctx: &mut SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
        // First pass: collect usage information
        let root = ctx.tree.root_node();
        track_usage(root, ctx);

        // Second pass: check for shadowing in declarations
        check_shadowing(root, ctx, diagnostics);

        // Third pass: check for assignments to immutable variables
        check_immutable_assignments(root, ctx, diagnostics);

        // Report unused symbols
        report_unused(ctx, diagnostics);

        // Report possibly private callbacks
        report_private_callbacks(ctx, diagnostics);
    }
}

fn track_usage(node: Node, ctx: &mut SemanticContext) {
    match node.kind() {
        kind::IDENTIFIER => {
            let name = node.text(ctx.source);
            ctx.scopes.mark_used_by_name(name);
        }
        kind::FUNCTION_DEFINITION => {
            // Enter function scope for parameter tracking
            let return_type = node
                .child_by_field("return_type")
                .map(|rt| ctx.resolve_type(rt.text(ctx.source)));

            let func_name = node
                .child_by_field("name")
                .map(|n| n.text(ctx.source).to_string())
                .unwrap_or_default();

            let body_span = node.child_by_kind(kind::BLOCK).map_or_else(
                || Span::new(node.start_byte(), node.end_byte()),
                |b| Span::new(b.start_byte(), b.end_byte()),
            );

            ctx.scopes.enter_scope(
                ScopeKind::Function {
                    name: func_name,
                    return_type,
                },
                body_span,
            );

            // Add parameters
            if let Some(params) = node.child_by_kind(kind::PARAMETERS) {
                add_function_params(params, ctx);
            }

            // Track body
            if let Some(body) = node.child_by_kind(kind::BLOCK) {
                track_block(body, ctx);
            }

            ctx.scopes.exit_scope();
        }
        _ => {
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                track_usage(child, ctx);
            }
        }
    }
}

fn check_params_shadowing(
    params: Node,
    ctx: &mut SemanticContext,
    diagnostics: &mut Vec<Diagnostic>,
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
        let param_name = name_node.text(ctx.source);

        // Check for shadowing
        if ctx.scopes.shadows_existing(param_name).is_some() {
            diagnostics.push(
                Diagnostic::warning(
                    format!("Parameter '{param_name}' shadows an existing binding"),
                    Span::new(name_node.start_byte(), name_node.end_byte()),
                )
                .with_code("W0600"),
            );
        }

        let param_type = ctx.resolve_type(type_node.text(ctx.source));
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

fn add_function_params(params: Node, ctx: &mut SemanticContext) {
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
        let param_name = name_node.text(ctx.source);
        let param_type = ctx.resolve_type(type_node.text(ctx.source));

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

fn track_block(node: Node, ctx: &mut SemanticContext) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            kind::LET_STATEMENT | kind::LET_ELSE_STATEMENT => {
                // First track the value expression
                if let Some(value) = child.child_by_field("value") {
                    track_usage(value, ctx);
                }

                // Track the else block for let-else statements
                if child.kind() == kind::LET_ELSE_STATEMENT {
                    if let Some(else_block) = child.child_by_kind(kind::BLOCK) {
                        track_block(else_block, ctx);
                    }
                }

                // Then add the variable
                if let Some(name_node) = child.child_by_field("name") {
                    let var_name = name_node.text(ctx.source);
                    let var_type = child
                        .child_by_field("type")
                        .map_or(TypeInfo::Unknown, |t| ctx.resolve_type(t.text(ctx.source)));

                    // Check if this is a mutable binding (mut= or &mut=)
                    let is_mutable = child
                        .child_by_field("operator")
                        .is_some_and(|op| op.text(ctx.source).contains("mut"));

                    let _ = ctx.scopes.define(
                        var_name,
                        SymbolKind::Variable,
                        var_type,
                        Span::new(child.start_byte(), child.end_byte()),
                        Span::new(name_node.start_byte(), name_node.end_byte()),
                        is_mutable,
                    );
                }
            }
            kind::FOR_STATEMENT => {
                // Track iterator first
                if let Some(iter) = child.child_by_field("iterator") {
                    track_usage(iter, ctx);
                }

                // Enter loop scope
                ctx.scopes.enter_scope(
                    ScopeKind::Loop,
                    Span::new(child.start_byte(), child.end_byte()),
                );

                // Add loop variable
                if let Some(var_node) = child.child_by_field("variable") {
                    let var_name = var_node.text(ctx.source);
                    let _ = ctx.scopes.define(
                        var_name,
                        SymbolKind::Variable,
                        TypeInfo::Unknown,
                        Span::new(var_node.start_byte(), var_node.end_byte()),
                        Span::new(var_node.start_byte(), var_node.end_byte()),
                        false,
                    );
                }

                if let Some(body) = child.child_by_kind(kind::BLOCK) {
                    track_block(body, ctx);
                }

                ctx.scopes.exit_scope();
            }
            kind::IF_STATEMENT | kind::IF_LET_STATEMENT => {
                // Track condition
                if let Some(cond) = child.child_by_field("condition") {
                    track_usage(cond, ctx);
                }

                // Track then block
                if let Some(then_block) = child.child_by_field("consequence") {
                    ctx.scopes.enter_scope(
                        ScopeKind::Block,
                        Span::new(then_block.start_byte(), then_block.end_byte()),
                    );
                    track_block(then_block, ctx);
                    ctx.scopes.exit_scope();
                }

                // Track else
                if let Some(else_clause) = child.child_by_kind(kind::ELSE_CLAUSE) {
                    let mut else_cursor = else_clause.walk();
                    for else_child in else_clause.children(&mut else_cursor) {
                        track_usage(else_child, ctx);
                    }
                }
            }
            kind::BLOCK => {
                ctx.scopes.enter_scope(
                    ScopeKind::Block,
                    Span::new(child.start_byte(), child.end_byte()),
                );
                track_block(child, ctx);
                ctx.scopes.exit_scope();
            }
            _ => track_usage(child, ctx),
        }
    }
}

fn check_shadowing(node: Node, ctx: &mut SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    if node.kind() == kind::FUNCTION_DEFINITION {
        // Enter function scope
        let return_type = node
            .child_by_field("return_type")
            .map(|rt| ctx.resolve_type(rt.text(ctx.source)));

        let func_name = node
            .child_by_field("name")
            .map(|n| n.text(ctx.source).to_string())
            .unwrap_or_default();

        let body_span = node.child_by_kind(kind::BLOCK).map_or_else(
            || Span::new(node.start_byte(), node.end_byte()),
            |b| Span::new(b.start_byte(), b.end_byte()),
        );

        ctx.scopes.enter_scope(
            ScopeKind::Function {
                name: func_name,
                return_type,
            },
            body_span,
        );

        // Add parameters (they can shadow globals)
        if let Some(params) = node.child_by_kind(kind::PARAMETERS) {
            check_params_shadowing(params, ctx, diagnostics);
        }

        if let Some(body) = node.child_by_kind(kind::BLOCK) {
            check_block_shadowing(body, ctx, diagnostics);
        }

        ctx.scopes.exit_scope();
    } else {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            check_shadowing(child, ctx, diagnostics);
        }
    }
}

fn check_block_shadowing(node: Node, ctx: &mut SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            kind::LET_STATEMENT | kind::LET_ELSE_STATEMENT => {
                // The name might be directly on the node or inside a binding child
                let binding = child.child_by_kind("binding");
                let name_node = binding
                    .and_then(|b| b.child_by_field("name"))
                    .or_else(|| child.child_by_field("name"));

                if let Some(name_node) = name_node {
                    let var_name = name_node.text(ctx.source);

                    // Check for shadowing
                    if ctx.scopes.shadows_existing(var_name).is_some() {
                        diagnostics.push(
                            Diagnostic::warning(
                                format!("Variable '{var_name}' shadows an existing binding"),
                                Span::new(name_node.start_byte(), name_node.end_byte()),
                            )
                            .with_code("W0600"),
                        );
                    }

                    // Get type from binding or directly
                    let type_node = binding
                        .and_then(|b| b.child_by_kind("type_pattern"))
                        .or_else(|| child.child_by_field("type"));
                    let var_type = type_node
                        .map_or(TypeInfo::Unknown, |t| ctx.resolve_type(t.text(ctx.source)));

                    // Check if this is a mutable binding (mut= or &mut=)
                    let is_mutable = binding
                        .and_then(|b| b.child_by_field("operator"))
                        .or_else(|| child.child_by_field("operator"))
                        .is_some_and(|op| op.text(ctx.source).contains("mut"));

                    let _ = ctx.scopes.define(
                        var_name,
                        SymbolKind::Variable,
                        var_type,
                        Span::new(child.start_byte(), child.end_byte()),
                        Span::new(name_node.start_byte(), name_node.end_byte()),
                        is_mutable,
                    );
                }
            }
            kind::FOR_STATEMENT => {
                ctx.scopes.enter_scope(
                    ScopeKind::Loop,
                    Span::new(child.start_byte(), child.end_byte()),
                );

                if let Some(var_node) = child.child_by_field("variable") {
                    let var_name = var_node.text(ctx.source);

                    if ctx.scopes.shadows_existing(var_name).is_some() {
                        diagnostics.push(
                            Diagnostic::warning(
                                format!("Loop variable '{var_name}' shadows an existing binding"),
                                Span::new(var_node.start_byte(), var_node.end_byte()),
                            )
                            .with_code("W0600"),
                        );
                    }

                    let _ = ctx.scopes.define(
                        var_name,
                        SymbolKind::Variable,
                        TypeInfo::Unknown,
                        Span::new(var_node.start_byte(), var_node.end_byte()),
                        Span::new(var_node.start_byte(), var_node.end_byte()),
                        false,
                    );
                }

                if let Some(body) = child.child_by_kind(kind::BLOCK) {
                    check_block_shadowing(body, ctx, diagnostics);
                }

                ctx.scopes.exit_scope();
            }
            kind::BLOCK => {
                ctx.scopes.enter_scope(
                    ScopeKind::Block,
                    Span::new(child.start_byte(), child.end_byte()),
                );
                check_block_shadowing(child, ctx, diagnostics);
                ctx.scopes.exit_scope();
            }
            _ => {}
        }
    }
}

// E0800: Check for assignments to immutable variables
fn check_immutable_assignments(
    node: Node,
    ctx: &mut SemanticContext,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match node.kind() {
        kind::ASSIGNMENT_STATEMENT => {
            check_single_assignment(node, ctx, diagnostics);
        }
        kind::FUNCTION_DEFINITION => {
            // Get function name and return type
            let fn_name = node
                .child_by_field("name")
                .map_or(String::new(), |n| n.text(ctx.source).to_string());
            let return_type = node
                .child_by_field("return_type")
                .map(|t| ctx.resolve_type(t.text(ctx.source)));

            // Enter function scope
            ctx.scopes.enter_scope(
                ScopeKind::Function {
                    name: fn_name,
                    return_type,
                },
                Span::new(node.start_byte(), node.end_byte()),
            );

            // Add parameters to scope
            if let Some(params) = node.child_by_kind(kind::PARAMETERS) {
                add_params_to_scope(params, ctx);
            }

            // Check body
            if let Some(body) = node.child_by_kind(kind::BLOCK) {
                check_block_assignments(body, ctx, diagnostics);
            }

            ctx.scopes.exit_scope();
        }
        _ => {
            // Recurse into children
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                check_immutable_assignments(child, ctx, diagnostics);
            }
        }
    }
}

fn add_params_to_scope(params: Node, ctx: &mut SemanticContext) {
    let mut cursor = params.walk();
    for param in params.children(&mut cursor) {
        if param.kind() != kind::PARAMETER {
            continue;
        }
        if let Some(name_node) = param.child_by_field("name") {
            let name = name_node.text(ctx.source);
            let param_type = param
                .child_by_field("type")
                .map_or(TypeInfo::Unknown, |t| ctx.resolve_type(t.text(ctx.source)));
            let _ = ctx.scopes.define(
                name,
                SymbolKind::Parameter,
                param_type,
                Span::new(param.start_byte(), param.end_byte()),
                Span::new(name_node.start_byte(), name_node.end_byte()),
                false, // Parameters are not mutable
            );
        }
    }
}

fn check_block_assignments(
    node: Node,
    ctx: &mut SemanticContext,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            kind::ASSIGNMENT_STATEMENT => {
                check_single_assignment(child, ctx, diagnostics);
            }
            kind::LET_STATEMENT | kind::LET_ELSE_STATEMENT => {
                // Add variable to scope
                let binding = child.child_by_kind("binding");
                let name_node = binding
                    .and_then(|b| b.child_by_field("name"))
                    .or_else(|| child.child_by_field("name"));

                if let Some(name_node) = name_node {
                    let var_name = name_node.text(ctx.source);
                    let var_type = binding
                        .and_then(|b| b.child_by_kind("type_pattern"))
                        .or_else(|| child.child_by_field("type"))
                        .map_or(TypeInfo::Unknown, |t| ctx.resolve_type(t.text(ctx.source)));

                    // Check if mutable binding
                    let is_mutable = binding
                        .and_then(|b| b.child_by_field("operator"))
                        .or_else(|| child.child_by_field("operator"))
                        .is_some_and(|op| op.text(ctx.source).contains("mut"));

                    let _ = ctx.scopes.define(
                        var_name,
                        SymbolKind::Variable,
                        var_type,
                        Span::new(child.start_byte(), child.end_byte()),
                        Span::new(name_node.start_byte(), name_node.end_byte()),
                        is_mutable,
                    );
                }
            }
            kind::FOR_STATEMENT => {
                ctx.scopes.enter_scope(
                    ScopeKind::Loop,
                    Span::new(child.start_byte(), child.end_byte()),
                );

                // Add loop variable
                if let Some(var_node) = child.child_by_field("variable") {
                    let var_name = var_node.text(ctx.source);
                    let _ = ctx.scopes.define(
                        var_name,
                        SymbolKind::Variable,
                        TypeInfo::Unknown,
                        Span::new(var_node.start_byte(), var_node.end_byte()),
                        Span::new(var_node.start_byte(), var_node.end_byte()),
                        false,
                    );
                }

                if let Some(body) = child.child_by_kind(kind::BLOCK) {
                    check_block_assignments(body, ctx, diagnostics);
                }

                ctx.scopes.exit_scope();
            }
            kind::IF_STATEMENT | kind::IF_LET_STATEMENT => {
                check_if_assignments(child, ctx, diagnostics);
            }
            kind::BLOCK => {
                ctx.scopes.enter_scope(
                    ScopeKind::Block,
                    Span::new(child.start_byte(), child.end_byte()),
                );
                check_block_assignments(child, ctx, diagnostics);
                ctx.scopes.exit_scope();
            }
            _ => {}
        }
    }
}

fn check_if_assignments(node: Node, ctx: &mut SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    // Check then block
    if let Some(then_block) = node.child_by_field("consequence") {
        ctx.scopes.enter_scope(
            ScopeKind::Block,
            Span::new(then_block.start_byte(), then_block.end_byte()),
        );
        check_block_assignments(then_block, ctx, diagnostics);
        ctx.scopes.exit_scope();
    }

    // Check else clause
    let Some(else_clause) = node.child_by_kind(kind::ELSE_CLAUSE) else {
        return;
    };

    let mut else_cursor = else_clause.walk();
    for else_child in else_clause.children(&mut else_cursor) {
        match else_child.kind() {
            kind::BLOCK => {
                ctx.scopes.enter_scope(
                    ScopeKind::Block,
                    Span::new(else_child.start_byte(), else_child.end_byte()),
                );
                check_block_assignments(else_child, ctx, diagnostics);
                ctx.scopes.exit_scope();
            }
            kind::IF_STATEMENT => {
                check_if_assignments(else_child, ctx, diagnostics);
            }
            _ => {}
        }
    }
}

fn check_single_assignment(node: Node, ctx: &SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    let Some(target) = node.child_by_field("left") else {
        return;
    };

    // Only check simple identifiers, not field access like self.field
    if target.kind() != kind::IDENTIFIER {
        return;
    }

    let target_text = target.text(ctx.source);
    let Some(sym) = ctx.scopes.lookup(target_text) else {
        return;
    };

    if sym.is_mutable {
        return;
    }

    let hint = if sym.type_info.is_reference() {
        " - references cannot be rebound"
    } else {
        ". Use 'mut=' to create a mutable binding"
    };
    diagnostics.push(
        Diagnostic::error(
            format!("Cannot assign to immutable variable '{target_text}'{hint}"),
            Span::new(target.start_byte(), target.end_byte()),
        )
        .with_code("E0800"),
    );
}

fn report_unused(ctx: &SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    // Check global symbols for unused definitions
    for sym in ctx.scopes.global_symbols() {
        if sym.is_used || should_skip_unused(sym) {
            continue;
        }

        let (message, code) = match sym.kind {
            SymbolKind::Variable => (format!("Unused variable '{}'", sym.name), "W0601"),
            SymbolKind::Function => {
                // Skip pub functions - they might be callbacks used by the game
                if ctx.is_pub_function(&sym.name) {
                    continue;
                }
                (format!("Unused function '{}'", sym.name), "W0602")
            }
            // Methods, fields, and variants are tracked differently
            SymbolKind::Method | SymbolKind::Field | SymbolKind::EnumVariant => continue,
            SymbolKind::Parameter => (format!("Unused parameter '{}'", sym.name), "W0604"),
            SymbolKind::Struct => {
                // Skip pub structs - they're entry points used by the game
                if ctx.is_pub_struct(&sym.name) {
                    continue;
                }
                (format!("Unused struct '{}'", sym.name), "W0605")
            }
            SymbolKind::Enum => {
                // Skip pub enums - they might be used in struct fields
                if ctx.is_pub_enum(&sym.name) {
                    continue;
                }
                (format!("Unused enum '{}'", sym.name), "W0606")
            }
            SymbolKind::Constant => (format!("Unused constant '{}'", sym.name), "W0601"),
        };

        diagnostics.push(Diagnostic::warning(message, sym.name_span).with_code(code));
    }
}

fn should_skip_unused(sym: &ScopedSymbol) -> bool {
    // Skip symbols that start with underscore (intentionally unused)
    if sym.name.starts_with('_') {
        return true;
    }

    // Skip 'self' parameter
    if sym.name == "self" {
        return true;
    }

    false
}

fn report_private_callbacks(ctx: &SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    for (name, span) in &ctx.possibly_private_callbacks {
        diagnostics.push(
            Diagnostic::warning(
                format!(
                    "Function '{name}' looks like a callback but is missing `pub` keyword. \
                     It won't be called by the game engine."
                ),
                *span,
            )
            .with_code("W0607"),
        );
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
        ScopeAnalysisPass.run(&mut ctx, &mut diagnostics);
        diagnostics
    }

    fn warnings(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
        diags
            .iter()
            .filter(|d| matches!(d.severity, Severity::Warning))
            .collect()
    }

    fn errors(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
        diags
            .iter()
            .filter(|d| matches!(d.severity, Severity::Error))
            .collect()
    }

    // W0600 - Variable shadowing

    #[test]
    fn test_variable_shadowing() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
const X: i64 = 5;
fn test() {
    let X = 10;
}
";
        let diags = check(source);
        let warns = warnings(&diags);
        assert!(
            warns.iter().any(|d| d.code.as_deref() == Some("W0600")),
            "Shadowing should warn: {warns:?}"
        );
    }

    #[test]
    fn test_no_shadowing_different_scope() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test1() {
    let x = 5;
}
fn test2() {
    let x = 10;
}
";
        let diags = check(source);
        let warns = warnings(&diags);
        assert!(
            warns.iter().all(|d| d.code.as_deref() != Some("W0600")),
            "Different scope should not shadow: {warns:?}"
        );
    }

    // W0607 - Private callback warning

    #[test]
    fn test_private_callback_method_warns() {
        // Callbacks are methods written as StructName::callback_name
        // Missing `pub` should warn
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct MySignal {}
fn MySignal::event_signal_check(self: &Self, ctx: &EventCtx, train: &Train, motion: &Motion, signal: &Signal) -> SignalCheck {
    SignalCheck::Pass
}
";
        let diags = check(source);
        let warns = warnings(&diags);
        assert!(
            warns.iter().any(|d| d.code.as_deref() == Some("W0607")),
            "Private callback method should warn: {warns:?}"
        );
    }

    #[test]
    fn test_private_callback_lookahead_warns() {
        // Real-world example from user
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct ProbeCheck {}
fn ProbeCheck::event_signal_lookahead(
    self: &ProbeCheck,
    ctx: &EventCtx,
    train: &Train,
    motion: &Motion,
    signal: &Signal,
    train_distance: f64,
    check: SignalCheck,
    result: &mut SignalLookaheadResult
) {
}
";
        let diags = check(source);
        let warns = warnings(&diags);
        for w in &warns {
            eprintln!("Warning: {:?} - {}", w.code, w.message);
        }
        assert!(
            warns.iter().any(|d| d.code.as_deref() == Some("W0607")),
            "Private callback method should warn: {warns:?}"
        );
    }

    #[test]
    fn test_pub_callback_method_no_warning() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct MySignal {}
pub fn MySignal::event_signal_check(self: &Self, ctx: &EventCtx, train: &Train, motion: &Motion, signal: &Signal) -> SignalCheck {
    SignalCheck::Pass
}
";
        let diags = check(source);
        let warns = warnings(&diags);
        assert!(
            warns.iter().all(|d| d.code.as_deref() != Some("W0607")),
            "Public callback method should not warn: {warns:?}"
        );
    }

    #[test]
    fn test_regular_private_function_no_callback_warning() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn helper_function() {
    // This is just a helper, not a callback
}
";
        let diags = check(source);
        let warns = warnings(&diags);
        assert!(
            warns.iter().all(|d| d.code.as_deref() != Some("W0607")),
            "Regular private function should not get callback warning: {warns:?}"
        );
    }

    #[test]
    fn test_private_method_non_callback_no_warning() {
        // Private method that's not a callback should not warn
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct MyStruct {}
fn MyStruct::helper_method(self: &Self) {
    // Just a helper method
}
";
        let diags = check(source);
        let warns = warnings(&diags);
        assert!(
            warns.iter().all(|d| d.code.as_deref() != Some("W0607")),
            "Private non-callback method should not warn: {warns:?}"
        );
    }

    // W0602 - Unused function (false positive tests)

    #[test]
    fn test_function_used_in_let_else_block() {
        // Function called in the else block of a let-else should be marked as used
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn helper() { }
fn main() {
    let x &= get_ptr() else { helper(); return; };
}
";
        let diags = check(source);
        let warns = warnings(&diags);
        assert!(
            warns
                .iter()
                .all(|d| { !(d.code.as_deref() == Some("W0602") && d.message.contains("helper")) }),
            "Function used in let-else else block should not get unused warning: {warns:?}"
        );
    }

    #[test]
    fn test_function_used_before_declaration() {
        // Function called before its declaration should still be marked as used
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn a() { b(); }
fn b() { }
";
        let diags = check(source);
        let warns = warnings(&diags);
        assert!(
            warns
                .iter()
                .all(|d| { !(d.code.as_deref() == Some("W0602") && d.message.contains("'b'")) }),
            "Function used before declaration should not get unused warning: {warns:?}"
        );
    }

    // E0800 - Assigning to immutable variables

    #[test]
    fn test_assign_to_immutable_variable_errors() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    let found_any = false;
    found_any = true;
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0800")),
            "Assigning to immutable variable should error: {errs:?}"
        );
        assert!(
            errs.iter().any(|d| d.message.contains("mut=")),
            "Should hint about mut=: {errs:?}"
        );
    }

    #[test]
    fn test_assign_to_mutable_variable_ok() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    let found_any mut= false;
    found_any = true;
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0800")),
            "Assigning to mutable variable should not error: {errs:?}"
        );
    }
}
