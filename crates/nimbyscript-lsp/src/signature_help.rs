use tower_lsp::lsp_types::*;

use nimbyscript_analyzer::{ApiDefinitions, FunctionDef};

use crate::document::Document;

/// Result of analyzing cursor position for signature help
#[derive(Debug)]
pub struct CallContext {
    /// The function/method name being called
    pub function_name: String,
    /// Module or type prefix (e.g., "Math" in Math::abs or struct name for methods)
    pub prefix: Option<String>,
    /// Which parameter index we're currently on (0-based)
    pub active_parameter: usize,
}

/// Find the function call context at the cursor position
pub fn get_call_context(content: &str, offset: usize) -> Option<CallContext> {
    let before = &content[..offset];

    // Find the opening paren we're inside, tracking nesting
    let mut paren_depth = 0;
    let mut paren_pos = None;

    for (i, c) in before.char_indices().rev() {
        match c {
            ')' => paren_depth += 1,
            '(' => {
                if paren_depth == 0 {
                    paren_pos = Some(i);
                    break;
                }
                paren_depth -= 1;
            }
            _ => {}
        }
    }

    let paren_pos = paren_pos?;

    // Count commas between the paren and cursor to get active parameter
    let inside_parens = &before[paren_pos + 1..];
    let active_parameter = count_parameters(inside_parens);

    // Extract function name before the paren
    let before_paren = &before[..paren_pos].trim_end();
    let (function_name, prefix) = extract_function_name(before_paren)?;

    Some(CallContext {
        function_name,
        prefix,
        active_parameter,
    })
}

/// Count which parameter we're on by counting commas at depth 0
fn count_parameters(text: &str) -> usize {
    let mut count = 0;
    let mut paren_depth: i32 = 0;

    for c in text.chars() {
        match c {
            '(' => paren_depth += 1,
            ')' => paren_depth = paren_depth.saturating_sub(1),
            ',' if paren_depth == 0 => count += 1,
            _ => {}
        }
    }

    count
}

/// Extract function name and optional prefix from before the paren
fn extract_function_name(before_paren: &str) -> Option<(String, Option<String>)> {
    // Handle method calls: expr.method_name
    if let Some(dot_pos) = before_paren.rfind('.') {
        let method_name = before_paren[dot_pos + 1..].trim();
        if is_valid_identifier(method_name) {
            // For method calls, we'd need type info to know the receiver type
            // For now, just return the method name without prefix
            return Some((method_name.to_string(), None));
        }
    }

    // Handle path expressions: Module::function or Type::method
    if let Some(colon_pos) = before_paren.rfind("::") {
        let func_name = before_paren[colon_pos + 2..].trim();
        if is_valid_identifier(func_name) {
            // Find the module/type name
            let before_colon = &before_paren[..colon_pos];
            let start = before_colon
                .rfind(|c: char| !c.is_alphanumeric() && c != '_')
                .map(|i| i + 1)
                .unwrap_or(0);
            let prefix = before_colon[start..].trim();
            if is_valid_identifier(prefix) {
                return Some((func_name.to_string(), Some(prefix.to_string())));
            }
        }
    }

    // Handle simple function calls: function_name
    let start = before_paren
        .rfind(|c: char| !c.is_alphanumeric() && c != '_')
        .map(|i| i + 1)
        .unwrap_or(0);
    let func_name = before_paren[start..].trim();

    if is_valid_identifier(func_name) {
        Some((func_name.to_string(), None))
    } else {
        None
    }
}

fn is_valid_identifier(s: &str) -> bool {
    !s.is_empty() && s.chars().next().map(|c| c.is_alphabetic() || c == '_').unwrap_or(false)
        && s.chars().all(|c| c.is_alphanumeric() || c == '_')
}

/// Get signature help for the current position
pub fn get_signature_help(
    doc: &Document,
    position: Position,
    api: &ApiDefinitions,
) -> Option<SignatureHelp> {
    let offset = doc.position_to_offset(position);
    let context = get_call_context(&doc.content, offset)?;

    // Try to find the function definition
    let func_def = find_function_def(&context, doc, api)?;

    let signature = build_signature_info(func_def, context.active_parameter);

    Some(SignatureHelp {
        signatures: vec![signature],
        active_signature: Some(0),
        active_parameter: Some(context.active_parameter as u32),
    })
}

/// Find the function definition based on the call context
fn find_function_def<'a>(
    context: &CallContext,
    doc: &Document,
    api: &'a ApiDefinitions,
) -> Option<&'a FunctionDef> {
    match &context.prefix {
        Some(prefix) => {
            // Check if it's a module function: Module::function
            if let Some(module) = api.get_module(prefix) {
                if let Some(func) = module.functions.iter().find(|f| f.name == context.function_name) {
                    return Some(func);
                }
            }

            // Check if it's a type method: Type::method
            if let Some(type_def) = api.get_type(prefix) {
                if let Some(method) = type_def.methods.iter().find(|m| m.name == context.function_name) {
                    return Some(method);
                }
            }

            // Check if it's a callback on a user-defined struct
            if let Some(extends_type) = doc.struct_extends(prefix) {
                if let Some(callback) = api.callbacks_for_type(extends_type)
                    .into_iter()
                    .find(|c| c.name == context.function_name)
                {
                    return Some(callback);
                }
            }

            None
        }
        None => {
            // Global function
            api.get_function(&context.function_name)
        }
    }
}

/// Build the SignatureInformation with parameter highlighting
fn build_signature_info(func: &FunctionDef, active_param: usize) -> SignatureInformation {
    // Build the signature label
    let params_str: Vec<String> = func.params.iter()
        .map(|p| format!("{}: {}", p.name, p.ty))
        .collect();

    let params_joined = params_str.join(", ");
    let label = match &func.return_type {
        Some(ret) => format!("{}({}) -> {}", func.name, params_joined, ret),
        None => format!("{}({})", func.name, params_joined),
    };

    // Calculate parameter label offsets for highlighting
    let mut parameters = Vec::new();
    let mut current_offset = func.name.len() + 1; // +1 for '('

    for (i, param_str) in params_str.iter().enumerate() {
        let start = current_offset;
        let end = start + param_str.len();

        parameters.push(ParameterInformation {
            label: ParameterLabel::LabelOffsets([start as u32, end as u32]),
            documentation: func.params.get(i)
                .and_then(|p| p.doc.as_ref())
                .map(|d| Documentation::String(d.clone())),
        });

        current_offset = end + 2; // +2 for ", "
    }

    SignatureInformation {
        label,
        documentation: func.doc.as_ref().map(|d| {
            Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: d.clone(),
            })
        }),
        parameters: Some(parameters),
        active_parameter: Some(active_param as u32),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_call_context_simple() {
        let content = "foo(bar, ";
        let ctx = get_call_context(content, content.len()).unwrap();
        assert_eq!(ctx.function_name, "foo");
        assert_eq!(ctx.prefix, None);
        assert_eq!(ctx.active_parameter, 1);
    }

    #[test]
    fn test_call_context_module() {
        let content = "Math::abs(x, ";
        let ctx = get_call_context(content, content.len()).unwrap();
        assert_eq!(ctx.function_name, "abs");
        assert_eq!(ctx.prefix, Some("Math".to_string()));
        assert_eq!(ctx.active_parameter, 1);
    }

    #[test]
    fn test_call_context_first_param() {
        let content = "foo(";
        let ctx = get_call_context(content, content.len()).unwrap();
        assert_eq!(ctx.active_parameter, 0);
    }

    #[test]
    fn test_call_context_nested() {
        let content = "foo(bar(x), ";
        let ctx = get_call_context(content, content.len()).unwrap();
        assert_eq!(ctx.function_name, "foo");
        assert_eq!(ctx.active_parameter, 1); // second param of foo
    }
}
