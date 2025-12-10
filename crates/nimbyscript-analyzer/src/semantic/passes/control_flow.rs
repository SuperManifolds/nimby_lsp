//! Control flow validation pass (E07xx).
//!
//! Validates control flow:
//! - E0700: 'break' outside of loop
//! - E0701: 'continue' outside of loop
//! - E0702: Missing return in function with return type
//! - W0703: Unreachable code
//! - E0704: Not all code paths return a value

use nimbyscript_parser::ast::Span;
use nimbyscript_parser::{kind, Node, NodeExt};

use crate::semantic::{SemanticContext, SemanticPass};
use crate::Diagnostic;

/// Pass that validates control flow
pub struct ControlFlowPass;

impl SemanticPass for ControlFlowPass {
    fn name(&self) -> &'static str {
        "control_flow"
    }

    fn run(&self, ctx: &mut SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
        let root = ctx.tree.root_node();
        check_control_flow(root, ctx, diagnostics, false);
    }
}

fn check_control_flow(
    node: Node,
    ctx: &mut SemanticContext,
    diagnostics: &mut Vec<Diagnostic>,
    in_loop: bool,
) {
    match node.kind() {
        kind::FUNCTION_DEFINITION => {
            check_function(node, ctx, diagnostics);
            // Also recurse into the function body for break/continue checks
            if let Some(body) = node.child_by_kind(kind::BLOCK) {
                let mut cursor = body.walk();
                for child in body.children(&mut cursor) {
                    check_control_flow(child, ctx, diagnostics, in_loop);
                }
            }
        }
        kind::FOR_STATEMENT => {
            // Process the loop body with in_loop = true
            if let Some(body) = node.child_by_kind(kind::BLOCK) {
                let mut cursor = body.walk();
                for child in body.children(&mut cursor) {
                    check_control_flow(child, ctx, diagnostics, true);
                }
            }
        }
        kind::BREAK_STATEMENT => {
            if !in_loop {
                diagnostics.push(
                    Diagnostic::error(
                        "'break' outside of loop".to_string(),
                        Span::new(node.start_byte(), node.end_byte()),
                    )
                    .with_code("E0700"),
                );
            }
        }
        kind::CONTINUE_STATEMENT => {
            if !in_loop {
                diagnostics.push(
                    Diagnostic::error(
                        "'continue' outside of loop".to_string(),
                        Span::new(node.start_byte(), node.end_byte()),
                    )
                    .with_code("E0701"),
                );
            }
        }
        _ => {
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                check_control_flow(child, ctx, diagnostics, in_loop);
            }
        }
    }
}

fn check_function(node: Node, ctx: &SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    // Get return type
    let return_type = node.child_by_field("return_type");

    // If function has a return type, check that all paths return
    if return_type.is_some() {
        if let Some(body) = node.child_by_kind(kind::BLOCK) {
            if !all_paths_return(body, ctx.source) {
                let func_name = node
                    .child_by_field("name")
                    .map(|n| n.text(ctx.source))
                    .unwrap_or("?");

                let return_type_str = return_type
                    .map(|rt| rt.text(ctx.source))
                    .unwrap_or("?");

                diagnostics.push(
                    Diagnostic::error(
                        format!(
                            "Function '{}' with return type '{}' may not return on all code paths",
                            func_name, return_type_str
                        ),
                        Span::new(node.start_byte(), node.end_byte()),
                    )
                    .with_code("E0704"),
                );
            }
        }
    }

    // Check for unreachable code within the function body
    if let Some(body) = node.child_by_kind(kind::BLOCK) {
        check_unreachable(body, ctx.source, diagnostics);
    }
}

/// Check if all code paths in a block return a value
fn all_paths_return(block: Node, source: &str) -> bool {
    let mut cursor = block.walk();
    let children: Vec<_> = block.children(&mut cursor).collect();

    // Empty block doesn't return
    if children.is_empty() {
        return false;
    }

    // Check the last statement
    for child in children.iter().rev() {
        match child.kind() {
            kind::RETURN_STATEMENT => return true,
            kind::IF_STATEMENT | kind::IF_LET_STATEMENT => {
                // Both branches must return
                let then_returns = child
                    .child_by_field("consequence")
                    .is_some_and(|b| all_paths_return(b, source));

                let else_returns = child
                    .child_by_kind(kind::ELSE_CLAUSE)
                    .is_some_and(|else_clause| {
                        let mut else_cursor = else_clause.walk();
                        for else_child in else_clause.children(&mut else_cursor) {
                            if else_child.kind() == kind::BLOCK {
                                return all_paths_return(else_child, source);
                            }
                            if else_child.kind() == kind::IF_STATEMENT
                                || else_child.kind() == kind::IF_LET_STATEMENT
                            {
                                // else if - recurse
                                return all_paths_return_if(else_child, source);
                            }
                        }
                        false
                    });

                if then_returns && else_returns {
                    return true;
                }
            }
            kind::FOR_STATEMENT => {
                // Loop might not execute, so doesn't guarantee return
            }
            kind::BLOCK => {
                if all_paths_return(*child, source) {
                    return true;
                }
            }
            kind::BREAK_STATEMENT | kind::CONTINUE_STATEMENT => {
                // These exit the current context but don't return
            }
            _ => {
                // Other statements don't return
            }
        }
    }

    false
}

fn all_paths_return_if(node: Node, source: &str) -> bool {
    let then_returns = node
        .child_by_field("consequence")
        .is_some_and(|b| all_paths_return(b, source));

    let else_returns = node
        .child_by_kind(kind::ELSE_CLAUSE)
        .is_some_and(|else_clause| {
            let mut cursor = else_clause.walk();
            for child in else_clause.children(&mut cursor) {
                if child.kind() == kind::BLOCK {
                    return all_paths_return(child, source);
                }
                if child.kind() == kind::IF_STATEMENT || child.kind() == kind::IF_LET_STATEMENT {
                    return all_paths_return_if(child, source);
                }
            }
            false
        });

    then_returns && else_returns
}

/// Check for unreachable code in a block
fn check_unreachable(block: Node, source: &str, diagnostics: &mut Vec<Diagnostic>) {
    let mut cursor = block.walk();
    // Use named_children to skip punctuation like { and }
    let children: Vec<_> = block.named_children(&mut cursor).collect();

    let mut saw_terminator = false;

    for child in children {
        // Skip non-statement nodes (punctuation, etc.)
        if !child.is_named() {
            continue;
        }

        if saw_terminator {
            // This code is unreachable
            diagnostics.push(
                Diagnostic::warning(
                    "Unreachable code".to_string(),
                    Span::new(child.start_byte(), child.end_byte()),
                )
                .with_code("W0703"),
            );
            break; // Only report once
        }

        match child.kind() {
            kind::RETURN_STATEMENT | kind::BREAK_STATEMENT | kind::CONTINUE_STATEMENT => {
                saw_terminator = true;
            }
            kind::IF_STATEMENT | kind::IF_LET_STATEMENT => {
                // Check nested blocks
                if let Some(then_block) = child.child_by_field("consequence") {
                    check_unreachable(then_block, source, diagnostics);
                }
                if let Some(else_clause) = child.child_by_kind(kind::ELSE_CLAUSE) {
                    let mut else_cursor = else_clause.walk();
                    for else_child in else_clause.named_children(&mut else_cursor) {
                        if else_child.kind() == kind::BLOCK {
                            check_unreachable(else_child, source, diagnostics);
                        }
                    }
                }
            }
            kind::FOR_STATEMENT => {
                if let Some(body) = child.child_by_kind(kind::BLOCK) {
                    check_unreachable(body, source, diagnostics);
                }
            }
            kind::BLOCK => {
                check_unreachable(child, source, diagnostics);
            }
            _ => {}
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
        ControlFlowPass.run(&mut ctx, &mut diagnostics);
        diagnostics
    }

    fn errors(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
        diags
            .iter()
            .filter(|d| matches!(d.severity, Severity::Error))
            .collect()
    }

    fn warnings(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
        diags
            .iter()
            .filter(|d| matches!(d.severity, Severity::Warning))
            .collect()
    }

    // E0700 - break outside loop

    #[test]
    fn test_break_outside_loop() {
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    break;
}
"#;
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0700")),
            "break outside loop should error: {:?}",
            errs
        );
    }

    #[test]
    fn test_break_inside_loop_ok() {
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    for x in items {
        break;
    }
}
"#;
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0700")),
            "break inside loop should be ok: {:?}",
            errs
        );
    }

    // E0701 - continue outside loop

    #[test]
    fn test_continue_outside_loop() {
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    continue;
}
"#;
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0701")),
            "continue outside loop should error: {:?}",
            errs
        );
    }

    // E0704 - not all paths return

    #[test]
    fn test_missing_return() {
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test(): i64 {
    let x = 5;
}
"#;
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0704")),
            "Missing return should error: {:?}",
            errs
        );
    }

    #[test]
    fn test_has_return_ok() {
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test(): i64 {
    return 5;
}
"#;
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0704")),
            "Has return should be ok: {:?}",
            errs
        );
    }

    #[test]
    fn test_void_function_no_return_ok() {
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test() {
    let x = 5;
}
"#;
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0704")),
            "Void function without return should be ok: {:?}",
            errs
        );
    }

    // W0703 - unreachable code

    #[test]
    fn test_unreachable_after_return() {
        let source = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test(): i64 {
    return 5;
    let x = 10;
}
"#;
        let diags = check(source);
        let warns = warnings(&diags);
        assert!(
            warns.iter().any(|d| d.code.as_deref() == Some("W0703")),
            "Unreachable code should warn: {:?}",
            warns
        );
    }
}
