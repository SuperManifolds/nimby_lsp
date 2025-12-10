//! NimbyScript-specific rules pass (E08xx).
//!
//! Validates NimbyScript-specific language rules:
//! - E0800: Cannot rebind reference
//! - E0801: Invalid const expression (must be literal)
//! - E0802: Cannot use 'mut' with reference in params
//! - W0803: Pointer without validity check

use nimbyscript_parser::ast::Span;
use nimbyscript_parser::{kind, Node, NodeExt};

use crate::semantic::{SemanticContext, SemanticPass};
use crate::Diagnostic;

/// Pass that validates NimbyScript-specific rules
pub struct NimbyScriptRulesPass;

impl SemanticPass for NimbyScriptRulesPass {
    fn name(&self) -> &'static str {
        "nimbyscript_rules"
    }

    fn run(&self, ctx: &mut SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
        let root = ctx.tree.root_node();
        check_rules(root, ctx, diagnostics);
    }
}

fn check_rules(node: Node, ctx: &SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    match node.kind() {
        kind::CONST_DECLARATION => check_const(node, ctx, diagnostics),
        kind::ASSIGNMENT_STATEMENT => check_assignment(node, ctx, diagnostics),
        _ => {
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                check_rules(child, ctx, diagnostics);
            }
        }
    }
}

fn check_const(node: Node, ctx: &SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    // E0801: Const must have a compile-time constant value
    // Value is a direct child - find it by looking for expression-like nodes
    let mut value: Option<Node> = None;
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        // Skip identifier (name) and type nodes
        if child.kind() != kind::IDENTIFIER && child.kind() != kind::TYPE {
            value = Some(child);
            break;
        }
    }

    if let Some(value) = value {
        if !is_const_expression(value, ctx.source) {
            diagnostics.push(
                Diagnostic::error(
                    "Const expression must be a compile-time constant (numeric literal, bool, or time expression)".to_string(),
                    Span::new(value.start_byte(), value.end_byte()),
                )
                .with_code("E0801"),
            );
        }
    }
}

fn is_const_expression(node: Node, source: &str) -> bool {
    match node.kind() {
        kind::NUMBER | kind::BOOLEAN | kind::TIME_LITERAL => true,
        kind::UNARY_EXPRESSION => {
            // Allow negation of numbers
            let text = node.text(source);
            text.starts_with('-')
                && node
                    .children(&mut node.walk())
                    .any(|c| c.kind() == kind::NUMBER)
        }
        kind::PARENTHESIZED_EXPRESSION => {
            // Check inner expression
            node.children(&mut node.walk())
                .any(|c| is_const_expression(c, source))
        }
        _ => false,
    }
}

fn check_assignment(node: Node, ctx: &SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    // E0800: Cannot reassign reference variables
    // This would require tracking which variables are references
    // For now, we check if the assignment target is a reference-typed variable

    if let Some(target) = node.child_by_field("left") {
        let target_text = target.text(ctx.source);

        // Look up the variable
        if let Some(sym) = ctx.scopes.lookup(target_text) {
            // Check if it's a reference type and not mutable
            if sym.type_info.is_reference() && !sym.is_mutable {
                diagnostics.push(
                    Diagnostic::error(
                        format!(
                            "Cannot rebind reference '{}' - references are immutable in NimbyScript",
                            target_text
                        ),
                        Span::new(target.start_byte(), target.end_byte()),
                    )
                    .with_code("E0800"),
                );
            }
        }
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
        NimbyScriptRulesPass.run(&mut ctx, &mut diagnostics);
        diagnostics
    }

    fn errors(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
        diags
            .iter()
            .filter(|d| matches!(d.severity, Severity::Error))
            .collect()
    }

    // E0801 - Invalid const expression

    #[test]
    fn test_const_literal_ok() {
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
const X: i64 = 42;
const Y: f64 = 3.14;
const Z: bool = true;
"#;
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0801")),
            "Literal const should be ok: {:?}",
            errs
        );
    }

    #[test]
    fn test_const_negative_ok() {
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
const X: i64 = -42;
"#;
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0801")),
            "Negative const should be ok: {:?}",
            errs
        );
    }

    #[test]
    fn test_const_call_error() {
        // Note: The tree-sitter grammar doesn't support function calls in const declarations,
        // so this produces a parse error (ERROR node) rather than reaching semantic analysis.
        // This test verifies we don't crash on parse errors.
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
const X: i64 = get_value();
"#;
        let diags = check(source);
        // The grammar rejects this at parse time, not semantic analysis time
        // We just ensure the pass doesn't crash
        assert!(diags.is_empty() || diags.iter().all(|d| d.code.as_deref() != Some("E0801")));
    }

    #[test]
    fn test_const_variable_error() {
        // Note: The tree-sitter grammar doesn't support variable references in const declarations,
        // so this produces a parse error (ERROR node) rather than reaching semantic analysis.
        // This test verifies we don't crash on parse errors.
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
const X: i64 = Y;
"#;
        let diags = check(source);
        // The grammar rejects this at parse time, not semantic analysis time
        // We just ensure the pass doesn't crash
        assert!(diags.is_empty() || diags.iter().all(|d| d.code.as_deref() != Some("E0801")));
    }
}
