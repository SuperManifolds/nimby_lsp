//! Completion provider for NimbyScript LSP.
//!
//! Provides intelligent, context-aware completions using semantic analysis.

use std::collections::HashMap;
use std::fmt::Write;

use serde::{Deserialize, Serialize};
use serde_json::Value;
use tower_lsp::lsp_types::*;

use nimbyscript_analyzer::{
    collect_declarations, ApiDefinitions, FunctionDef, SemanticContext, TypeInfo,
};
use nimbyscript_parser::{kind, parse, Node, NodeExt, Tree};

use crate::document::Document;

/// NimbyScript keywords
const KEYWORDS: &[&str] = &[
    "script", "meta", "const", "struct", "enum", "fn", "pub", "extend", "let", "mut", "if", "else",
    "for", "in", "return", "break", "continue", "true", "false", "log",
];

/// Primitive types
const PRIMITIVE_TYPES: &[&str] = &[
    "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f32", "f64", "bool",
];

// ============================================================================
// Completion Engine
// ============================================================================

/// Engine for computing completions with semantic awareness.
pub struct CompletionEngine<'a> {
    content: &'a str,
    tree: Tree,
    api: &'a ApiDefinitions,
    offset: usize,
    /// Local variable types extracted from function parameters and let bindings
    local_types: HashMap<String, TypeInfo>,
    /// User-defined struct fields (struct_name -> (field_name -> field_type))
    struct_fields: HashMap<String, HashMap<String, TypeInfo>>,
    /// User-defined structs (name -> extends type)
    user_structs: HashMap<String, Option<String>>,
    /// User-defined enums (name -> variants)
    user_enums: HashMap<String, Vec<String>>,
    /// User-defined symbol documentation (name -> doc comment)
    user_docs: HashMap<String, String>,
    /// Current function's return type (for smart filtering)
    current_return_type: Option<TypeInfo>,
}

impl<'a> CompletionEngine<'a> {
    /// Create a new completion engine for the given document and position.
    pub fn new(content: &'a str, api: &'a ApiDefinitions, position: Position) -> Self {
        let tree = parse(content);
        let offset = position_to_offset(content, position);

        // Build semantic context to collect declarations
        let mut ctx = SemanticContext::new(content, &tree, api);
        collect_declarations(&mut ctx);

        // Extract what we need from the semantic context
        let struct_fields = ctx.struct_fields.clone();
        let user_structs = ctx.user_structs.clone();
        let user_enums = ctx.user_enums.clone();

        let mut engine = Self {
            content,
            tree,
            api,
            offset,
            local_types: HashMap::new(),
            struct_fields,
            user_structs,
            user_enums,
            user_docs: HashMap::new(),
            current_return_type: None,
        };

        // Collect user documentation from comments
        engine.collect_user_docs();

        // Populate local types from enclosing function parameters
        // Also sets current_return_type
        engine.collect_local_types();

        engine
    }

    /// Compute completions for the current position.
    pub fn completions(&self) -> Vec<CompletionItem> {
        let prefix = self.get_prefix();
        let context = self.determine_context();

        match context {
            CompletionContext::Type => {
                let mut items = Vec::new();
                self.add_primitive_types(&mut items, &prefix);
                self.add_api_types(&mut items, &prefix);
                self.add_user_types(&mut items, &prefix);
                items
            }
            CompletionContext::PathAfterColon(name) => {
                let mut items = Vec::new();
                self.add_module_members(&mut items, &name, &prefix);
                self.add_struct_callbacks(&mut items, &name, &prefix);
                items
            }
            CompletionContext::FieldAccess(base_type) => {
                self.complete_field_access(&base_type, &prefix)
            }
            CompletionContext::Return => {
                // Smart return: show items that match the return type first
                let mut items = Vec::new();
                self.add_return_completions(&mut items, &prefix);
                items
            }
            CompletionContext::General => {
                let mut items = Vec::new();
                self.add_keywords(&mut items, &prefix);
                self.add_snippets(&mut items, &prefix);
                self.add_local_variables(&mut items, &prefix);
                self.add_primitive_types(&mut items, &prefix);
                self.add_api_completions(&mut items, &prefix);
                self.add_document_symbols(&mut items, &prefix);
                items
            }
        }
    }

    // ========================================================================
    // Context Detection
    // ========================================================================

    /// Determine the completion context from the cursor position.
    fn determine_context(&self) -> CompletionContext {
        let before = &self.content[..self.offset];

        // First, try AST-based detection for field access
        if let Some(ctx) = self.detect_field_access_context() {
            return ctx;
        }

        // Check if we're after :: (path context)
        if let Some(colon_pos) = before.rfind("::") {
            let after_colon = &before[colon_pos + 2..];
            if after_colon.is_empty() || after_colon.chars().all(|c| c.is_alphanumeric() || c == '_')
            {
                let before_colon = &before[..colon_pos];
                let start = before_colon
                    .rfind(|c: char| !c.is_alphanumeric() && c != '_')
                    .map_or(0, |i| i + 1);
                let module = before_colon[start..].to_string();
                if !module.is_empty() {
                    return CompletionContext::PathAfterColon(module);
                }
            }
        }

        // Check if we're in a type annotation context
        if before.ends_with(": ") || before.ends_with(":<") || before.ends_with(", ") {
            let trimmed = before.trim();
            if trimmed.ends_with(':') || trimmed.contains("->") {
                return CompletionContext::Type;
            }
        }

        // Check if we're after "return " keyword
        let trimmed = before.trim_end();
        if trimmed.ends_with("return") || trimmed.ends_with("return ") {
            if self.current_return_type.is_some() {
                return CompletionContext::Return;
            }
        }

        CompletionContext::General
    }

    /// Detect field access context using AST analysis.
    fn detect_field_access_context(&self) -> Option<CompletionContext> {
        let before = &self.content[..self.offset];

        // Quick check: must have a dot somewhere before cursor
        let dot_pos = before.rfind('.')?;
        let after_dot = &before[dot_pos + 1..];

        // After the dot, we can only have identifier chars (or nothing if cursor is right after dot)
        if !after_dot.is_empty() && !after_dot.chars().all(|c| c.is_alphanumeric() || c == '_') {
            return None;
        }

        // Extract the expression before the dot
        // Work backwards from the dot to find the expression
        let expr_str = self.extract_expression_before(&before[..dot_pos])?;

        // Parse the expression to infer its type
        let base_type = self.infer_expression_type_from_text(&expr_str)?;

        Some(CompletionContext::FieldAccess(base_type))
    }

    /// Extract the expression text immediately before a position.
    /// Handles chained field access like "ctx.db" by working backward.
    fn extract_expression_before(&self, text: &str) -> Option<String> {
        let text = text.trim_end();
        if text.is_empty() {
            return None;
        }

        // Work backwards to find the start of the expression
        let end = text.len();
        let mut depth = 0; // Track parentheses depth
        let chars: Vec<char> = text.chars().collect();
        let mut i = chars.len();

        while i > 0 {
            i -= 1;
            let c = chars[i];

            match c {
                ')' => depth += 1,
                '(' => {
                    if depth > 0 {
                        depth -= 1;
                    } else {
                        // Hit unmatched open paren - expression starts after this
                        return Some(text[i + 1..end].to_string());
                    }
                }
                '.' if depth == 0 => {
                    // Part of chained access, continue
                }
                c if c.is_alphanumeric() || c == '_' => {
                    // Valid identifier char, continue
                }
                _ if depth == 0 => {
                    // Hit a delimiter (space, operator, etc.) - expression starts after this
                    return Some(text[i + 1..end].to_string());
                }
                _ => {
                    // Inside parentheses, continue
                }
            }
        }

        // Expression starts at the beginning
        Some(text[..end].to_string())
    }

    // ========================================================================
    // Type Inference
    // ========================================================================

    /// Infer the type of an expression from its text representation.
    fn infer_expression_type_from_text(&self, expr: &str) -> Option<TypeInfo> {
        let expr = expr.trim();

        // Handle chained field access: "ctx.db" -> infer type of ctx, get field db
        if let Some(dot_pos) = expr.rfind('.') {
            let object_expr = &expr[..dot_pos];
            let field_name = &expr[dot_pos + 1..];

            let object_type = self.infer_expression_type_from_text(object_expr)?;
            return self.get_field_type(&object_type, field_name);
        }

        // Handle method call: "ctx.db.view(self.owner)" -> infer return type
        if expr.ends_with(')') {
            return self.infer_call_return_type_from_text(expr);
        }

        // Simple identifier: look up in local types
        if expr.chars().all(|c| c.is_alphanumeric() || c == '_') {
            return self.lookup_variable_type(expr);
        }

        None
    }

    /// Infer the return type of a call expression from its text.
    fn infer_call_return_type_from_text(&self, expr: &str) -> Option<TypeInfo> {
        // Find the function/method being called
        let paren_pos = expr.rfind('(')?;
        let callee = &expr[..paren_pos];

        // Method call: "something.method(...)"
        if let Some(dot_pos) = callee.rfind('.') {
            let object_expr = &callee[..dot_pos];
            let method_name = &callee[dot_pos + 1..];

            let object_type = self.infer_expression_type_from_text(object_expr)?;
            return self.get_method_return_type(&object_type, method_name);
        }

        // Path call: "Module::func(...)"
        if let Some(colon_pos) = callee.rfind("::") {
            let module = &callee[..colon_pos];
            let func_name = &callee[colon_pos + 2..];

            // Check module functions
            if let Some(module_def) = self.api.get_module(module) {
                if let Some(func) = module_def.functions.iter().find(|f| f.name == func_name) {
                    return func
                        .return_type
                        .as_ref()
                        .map(|t| parse_type_string_simple(t));
                }
            }

            // Check type static methods
            if let Some(type_def) = self.api.get_type(module) {
                if let Some(method) = type_def.methods.iter().find(|m| m.name == func_name) {
                    return method
                        .return_type
                        .as_ref()
                        .map(|t| parse_type_string_simple(t));
                }
            }
        }

        // Global function call
        if let Some(func) = self.api.get_function(callee) {
            return func
                .return_type
                .as_ref()
                .map(|t| parse_type_string_simple(t));
        }

        None
    }

    /// Look up the type of a variable by name.
    fn lookup_variable_type(&self, name: &str) -> Option<TypeInfo> {
        // Check local variables first (parameters, let bindings)
        if let Some(ty) = self.local_types.get(name) {
            return Some(ty.clone());
        }

        None
    }

    /// Get the type of a field on a given type.
    fn get_field_type(&self, base_type: &TypeInfo, field_name: &str) -> Option<TypeInfo> {
        let type_name = self.unwrap_to_type_name(base_type)?;

        // Check user-defined struct fields
        if let Some(fields) = self.struct_fields.get(&type_name) {
            if let Some(field_type) = fields.get(field_name) {
                return Some(field_type.clone());
            }
        }

        // Check game type fields
        if let Some(type_def) = self.api.get_type(&type_name) {
            if let Some(field) = type_def.fields.get(field_name) {
                return Some(parse_type_string_simple(&field.ty));
            }
        }

        None
    }

    /// Get the return type of a method on a given type.
    fn get_method_return_type(&self, base_type: &TypeInfo, method_name: &str) -> Option<TypeInfo> {
        let type_name = self.unwrap_to_type_name(base_type)?;

        // Check game type methods
        if let Some(type_def) = self.api.get_type(&type_name) {
            if let Some(method) = type_def.methods.iter().find(|m| m.name == method_name) {
                return method
                    .return_type
                    .as_ref()
                    .map(|t| parse_type_string_simple(t));
            }
        }

        None
    }

    /// Unwrap references/pointers and get the underlying type name.
    fn unwrap_to_type_name(&self, ty: &TypeInfo) -> Option<String> {
        match ty {
            TypeInfo::Struct { name, .. } => Some(name.clone()),
            TypeInfo::Enum { name } => Some(name.clone()),
            TypeInfo::Reference { inner, .. } | TypeInfo::Pointer { inner, .. } => {
                self.unwrap_to_type_name(inner)
            }
            TypeInfo::Generic { name, args } => {
                // For ID<T>, return the generic form "ID<T>"
                if !args.is_empty() {
                    let arg_names: Vec<_> = args
                        .iter()
                        .filter_map(|a| self.unwrap_to_type_name(a))
                        .collect();
                    Some(format!("{}<{}>", name, arg_names.join(", ")))
                } else {
                    Some(name.clone())
                }
            }
            _ => None,
        }
    }

    // ========================================================================
    // Local Variable Collection
    // ========================================================================

    /// Collect local variable types from the enclosing function.
    fn collect_local_types(&mut self) {
        // Find the function containing the cursor position
        // We need to collect the info we need before mutating self
        let root = self.tree.root_node();
        let Some((params_info, body_info, return_type)) = self.find_function_info(root) else {
            return;
        };

        // Set current return type for smart return completions
        self.current_return_type = return_type;

        // Process parameters
        for (name, type_info) in params_info {
            self.local_types.insert(name, type_info);
        }

        // Process let bindings
        for (name, type_info) in body_info {
            self.local_types.insert(name, type_info);
        }
    }

    /// Find function info at cursor position, extracting parameters, let bindings, and return type.
    fn find_function_info(&self, node: Node) -> Option<(Vec<(String, TypeInfo)>, Vec<(String, TypeInfo)>, Option<TypeInfo>)> {
        // Check if this is a function containing our position
        if node.kind() == kind::FUNCTION_DEFINITION {
            let start = node.start_byte();
            let end = node.end_byte();
            if start <= self.offset && self.offset <= end {
                let mut params = Vec::new();
                let mut bindings = Vec::new();

                // Extract return type
                let return_type = node.child_by_field("return_type")
                    .map(|rt| parse_type_string_simple(rt.text(self.content)));

                // Extract parameters
                if let Some(params_node) = node.child_by_kind(kind::PARAMETERS) {
                    let mut cursor = params_node.walk();
                    for child in params_node.children(&mut cursor) {
                        if child.kind() == kind::PARAMETER {
                            if let (Some(name_node), Some(type_node)) =
                                (child.child_by_field("name"), child.child_by_field("type"))
                            {
                                let name = name_node.text(self.content).to_string();
                                let type_str = type_node.text(self.content);
                                let type_info = parse_type_string_simple(type_str);
                                params.push((name, type_info));
                            }
                        }
                    }
                }

                // Extract let bindings from body
                if let Some(body) = node.child_by_field("body") {
                    self.collect_bindings_from_block(body, &mut bindings);
                }

                return Some((params, bindings, return_type));
            }
        }

        // Search children
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(info) = self.find_function_info(child) {
                return Some(info);
            }
        }

        None
    }

    /// Collect let binding types from a block (only those before cursor).
    fn collect_bindings_from_block(&self, block_node: Node, bindings: &mut Vec<(String, TypeInfo)>) {
        let mut cursor = block_node.walk();
        for child in block_node.children(&mut cursor) {
            // Only process bindings before the cursor
            if child.start_byte() >= self.offset {
                break;
            }

            match child.kind() {
                kind::LET_STATEMENT | kind::LET_ELSE_STATEMENT => {
                    if let Some((name, type_info)) = self.extract_binding_info(child) {
                        bindings.push((name, type_info));
                    }
                }
                kind::IF_STATEMENT | kind::IF_LET_STATEMENT | kind::FOR_STATEMENT => {
                    // Recurse into nested blocks
                    let mut inner = child.walk();
                    for inner_child in child.children(&mut inner) {
                        if inner_child.kind() == kind::BLOCK {
                            self.collect_bindings_from_block(inner_child, bindings);
                        }
                    }
                }
                _ => {}
            }
        }
    }

    /// Extract variable name and type from a let statement.
    fn extract_binding_info(&self, let_node: Node) -> Option<(String, TypeInfo)> {
        // Find the binding node
        let binding = let_node.child_by_kind("binding")?;

        let name_node = binding.child_by_field("name")?;
        let name = name_node.text(self.content).to_string();

        // Try to get explicit type annotation
        let mut cursor = binding.walk();
        for child in binding.children(&mut cursor) {
            if child.kind() == "type_pattern" || child.kind() == kind::TYPE {
                let type_str = child.text(self.content);
                let type_info = parse_type_string_simple(type_str);
                return Some((name, type_info));
            }
        }

        // If no explicit type, try to infer from the value expression
        if let Some(value_node) = binding.child_by_field("value") {
            let text = value_node.text(self.content);
            if let Some(type_info) = self.infer_expression_type_from_text(text) {
                return Some((name, type_info));
            }
        }

        None
    }

    // ========================================================================
    // Field/Method Completion
    // ========================================================================

    /// Generate completions for field access on a type.
    fn complete_field_access(&self, base_type: &TypeInfo, prefix: &str) -> Vec<CompletionItem> {
        let mut items = Vec::new();

        let Some(type_name) = self.unwrap_to_type_name(base_type) else {
            return items;
        };

        // Handle generic types like ID<Train>
        let base_type_name = if type_name.contains('<') {
            type_name.split('<').next().unwrap_or(&type_name)
        } else {
            &type_name
        };

        // Add fields from user-defined structs
        if let Some(fields) = self.struct_fields.get(base_type_name) {
            for (field_name, field_type) in fields {
                if field_name.starts_with(prefix) {
                    items.push(CompletionItem {
                        label: field_name.clone(),
                        kind: Some(CompletionItemKind::FIELD),
                        detail: Some(field_type.to_string()),
                        ..Default::default()
                    });
                }
            }

            // Add inherited fields from extended game type
            if let Some(Some(extends_type)) = self.user_structs.get(base_type_name) {
                self.add_inherited_fields(&mut items, extends_type, prefix);
            }
        }

        // Add fields from game types
        if let Some(type_def) = self.api.get_type(base_type_name) {
            for (field_name, field) in &type_def.fields {
                if field_name.starts_with(prefix) {
                    items.push(CompletionItem {
                        label: field_name.clone(),
                        kind: Some(CompletionItemKind::FIELD),
                        detail: Some(field.ty.clone()),
                        documentation: field.doc.as_ref().map(|d| {
                            Documentation::MarkupContent(MarkupContent {
                                kind: MarkupKind::Markdown,
                                value: d.clone(),
                            })
                        }),
                        data: make_resolve_data(CompletionResolveData::Field {
                            type_name: base_type_name.to_string(),
                            field_name: field_name.clone(),
                        }),
                        ..Default::default()
                    });
                }
            }

            // Add methods from game types
            for method in &type_def.methods {
                if method.name.starts_with(prefix) {
                    items.push(CompletionItem {
                        label: method.name.clone(),
                        kind: Some(CompletionItemKind::METHOD),
                        detail: Some(format_signature(method)),
                        documentation: format_documentation(method),
                        insert_text: Some(format_snippet(&method.name, method)),
                        insert_text_format: Some(InsertTextFormat::SNIPPET),
                        data: make_resolve_data(CompletionResolveData::Method {
                            type_name: base_type_name.to_string(),
                            method_name: method.name.clone(),
                        }),
                        ..Default::default()
                    });
                }
            }
        }

        items
    }

    /// Add fields inherited from an extended game type.
    fn add_inherited_fields(&self, items: &mut Vec<CompletionItem>, extends_type: &str, prefix: &str) {
        if let Some(type_def) = self.api.get_type(extends_type) {
            for (field_name, field) in &type_def.fields {
                if field_name.starts_with(prefix) {
                    items.push(CompletionItem {
                        label: field_name.clone(),
                        kind: Some(CompletionItemKind::FIELD),
                        detail: Some(format!("{} (from {})", field.ty, extends_type)),
                        documentation: field.doc.as_ref().map(|d| {
                            Documentation::MarkupContent(MarkupContent {
                                kind: MarkupKind::Markdown,
                                value: d.clone(),
                            })
                        }),
                        // Sort inherited fields after user fields
                        sort_text: Some(format!("1_{field_name}")),
                        data: make_resolve_data(CompletionResolveData::Field {
                            type_name: extends_type.to_string(),
                            field_name: field_name.clone(),
                        }),
                        ..Default::default()
                    });
                }
            }

            // Add methods from the extended type
            for method in &type_def.methods {
                if method.name.starts_with(prefix) {
                    items.push(CompletionItem {
                        label: method.name.clone(),
                        kind: Some(CompletionItemKind::METHOD),
                        detail: Some(format!("{} (from {})", format_signature(method), extends_type)),
                        documentation: format_documentation(method),
                        insert_text: Some(format_snippet(&method.name, method)),
                        insert_text_format: Some(InsertTextFormat::SNIPPET),
                        sort_text: Some(format!("1_{}", method.name)),
                        data: make_resolve_data(CompletionResolveData::Method {
                            type_name: extends_type.to_string(),
                            method_name: method.name.clone(),
                        }),
                        ..Default::default()
                    });
                }
            }
        }
    }

    // ========================================================================
    // Helper Methods
    // ========================================================================

    /// Get the identifier prefix at the cursor position.
    fn get_prefix(&self) -> String {
        let before = &self.content[..self.offset];

        // Check if we're after a dot - prefix is what comes after the last dot
        if let Some(dot_pos) = before.rfind('.') {
            let after_dot = &before[dot_pos + 1..];
            if after_dot.chars().all(|c| c.is_alphanumeric() || c == '_') {
                return after_dot.to_string();
            }
        }

        // Check if we're after :: - prefix is what comes after
        if let Some(colon_pos) = before.rfind("::") {
            let after_colon = &before[colon_pos + 2..];
            if after_colon.chars().all(|c| c.is_alphanumeric() || c == '_') {
                return after_colon.to_string();
            }
        }

        // Regular prefix extraction
        let start = before
            .rfind(|c: char| !c.is_alphanumeric() && c != '_')
            .map_or(0, |i| i + 1);
        before[start..].to_string()
    }

    // ========================================================================
    // Completion Providers
    // ========================================================================

    fn add_keywords(&self, items: &mut Vec<CompletionItem>, prefix: &str) {
        for &keyword in KEYWORDS {
            if keyword.starts_with(prefix) {
                items.push(CompletionItem {
                    label: (*keyword).to_string(),
                    kind: Some(CompletionItemKind::KEYWORD),
                    detail: Some("keyword".to_string()),
                    ..Default::default()
                });
            }
        }
    }

    fn add_local_variables(&self, items: &mut Vec<CompletionItem>, prefix: &str) {
        for (name, type_info) in &self.local_types {
            if name.starts_with(prefix) {
                items.push(CompletionItem {
                    label: name.clone(),
                    kind: Some(CompletionItemKind::VARIABLE),
                    detail: Some(type_info.to_string()),
                    // Sort local variables first
                    sort_text: Some(format!("0_{name}")),
                    ..Default::default()
                });
            }
        }
    }

    fn add_primitive_types(&self, items: &mut Vec<CompletionItem>, prefix: &str) {
        for &ty in PRIMITIVE_TYPES {
            if ty.starts_with(prefix) {
                items.push(CompletionItem {
                    label: (*ty).to_string(),
                    kind: Some(CompletionItemKind::TYPE_PARAMETER),
                    detail: Some("primitive type".to_string()),
                    ..Default::default()
                });
            }
        }

        // Add ID<T> generic
        if "ID".starts_with(prefix) {
            items.push(CompletionItem {
                label: "ID".to_string(),
                kind: Some(CompletionItemKind::TYPE_PARAMETER),
                detail: Some("generic type".to_string()),
                insert_text: Some("ID<$1>".to_string()),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                ..Default::default()
            });
        }
    }

    fn add_api_types(&self, items: &mut Vec<CompletionItem>, prefix: &str) {
        for name in self.api.type_names() {
            if name.starts_with(prefix) {
                let type_def = self.api.get_type(name);
                items.push(CompletionItem {
                    label: name.to_string(),
                    kind: Some(CompletionItemKind::STRUCT),
                    detail: Some("game type".to_string()),
                    documentation: type_def.and_then(|t| t.doc.as_ref()).map(|d| {
                        Documentation::MarkupContent(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: d.clone(),
                        })
                    }),
                    data: make_resolve_data(CompletionResolveData::Type {
                        name: name.to_string(),
                    }),
                    ..Default::default()
                });
            }
        }

        for name in self.api.enum_names() {
            if name.starts_with(prefix) {
                let enum_def = self.api.get_enum(name);
                items.push(CompletionItem {
                    label: name.to_string(),
                    kind: Some(CompletionItemKind::ENUM),
                    detail: Some("game enum".to_string()),
                    documentation: enum_def.and_then(|e| e.doc.as_ref()).map(|d| {
                        Documentation::MarkupContent(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: d.clone(),
                        })
                    }),
                    data: make_resolve_data(CompletionResolveData::Enum {
                        name: name.to_string(),
                    }),
                    ..Default::default()
                });
            }
        }
    }

    fn add_user_types(&self, items: &mut Vec<CompletionItem>, prefix: &str) {
        for name in self.user_structs.keys() {
            if name.starts_with(prefix) {
                items.push(CompletionItem {
                    label: name.clone(),
                    kind: Some(CompletionItemKind::STRUCT),
                    detail: Some("user struct".to_string()),
                    ..Default::default()
                });
            }
        }

        for name in self.user_enums.keys() {
            if name.starts_with(prefix) {
                items.push(CompletionItem {
                    label: name.clone(),
                    kind: Some(CompletionItemKind::ENUM),
                    detail: Some("user enum".to_string()),
                    ..Default::default()
                });
            }
        }
    }

    fn add_api_completions(&self, items: &mut Vec<CompletionItem>, prefix: &str) {
        self.add_api_types(items, prefix);

        // Add modules
        for name in self.api.module_names() {
            if name.starts_with(prefix) {
                let module_def = self.api.get_module(name);
                items.push(CompletionItem {
                    label: name.to_string(),
                    kind: Some(CompletionItemKind::MODULE),
                    detail: Some("module".to_string()),
                    documentation: module_def.and_then(|m| m.doc.as_ref()).map(|d| {
                        Documentation::MarkupContent(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: d.clone(),
                        })
                    }),
                    data: make_resolve_data(CompletionResolveData::Module {
                        name: name.to_string(),
                    }),
                    ..Default::default()
                });
            }
        }

        // Add global functions from API
        for name in self.api.function_names() {
            if name.starts_with(prefix) {
                if let Some(func) = self.api.get_function(name) {
                    items.push(CompletionItem {
                        label: name.to_string(),
                        kind: Some(CompletionItemKind::FUNCTION),
                        detail: Some(format!("fn {}", format_signature(func))),
                        documentation: format_documentation(func),
                        insert_text: Some(format_snippet(name, func)),
                        insert_text_format: Some(InsertTextFormat::SNIPPET),
                        data: make_resolve_data(CompletionResolveData::Function {
                            name: name.to_string(),
                        }),
                        ..Default::default()
                    });
                }
            }
        }
    }

    fn add_module_members(&self, items: &mut Vec<CompletionItem>, module: &str, prefix: &str) {
        // Check if it's a module
        if let Some(module_def) = self.api.get_module(module) {
            for func in &module_def.functions {
                if func.name.starts_with(prefix) {
                    items.push(CompletionItem {
                        label: func.name.clone(),
                        kind: Some(CompletionItemKind::FUNCTION),
                        detail: Some(format_signature(func)),
                        documentation: format_documentation(func),
                        insert_text: Some(format_snippet(&func.name, func)),
                        insert_text_format: Some(InsertTextFormat::SNIPPET),
                        data: make_resolve_data(CompletionResolveData::ModuleFunction {
                            module_name: module.to_string(),
                            function_name: func.name.clone(),
                        }),
                        ..Default::default()
                    });
                }
            }
        }

        // Check if it's a type with methods
        if let Some(type_def) = self.api.get_type(module) {
            for method in &type_def.methods {
                if method.name.starts_with(prefix) {
                    items.push(CompletionItem {
                        label: method.name.clone(),
                        kind: Some(CompletionItemKind::METHOD),
                        detail: Some(format_signature(method)),
                        documentation: format_documentation(method),
                        insert_text: Some(format_snippet(&method.name, method)),
                        insert_text_format: Some(InsertTextFormat::SNIPPET),
                        data: make_resolve_data(CompletionResolveData::Method {
                            type_name: module.to_string(),
                            method_name: method.name.clone(),
                        }),
                        ..Default::default()
                    });
                }
            }
        }

        // Check if it's an enum
        if let Some(enum_def) = self.api.get_enum(module) {
            for variant in &enum_def.variants {
                if variant.name.starts_with(prefix) {
                    items.push(CompletionItem {
                        label: variant.name.clone(),
                        kind: Some(CompletionItemKind::ENUM_MEMBER),
                        detail: Some(format!("{module}::{}", variant.name)),
                        documentation: variant.doc.as_ref().map(|d| Documentation::String(d.clone())),
                        data: make_resolve_data(CompletionResolveData::EnumVariant {
                            enum_name: module.to_string(),
                            variant_name: variant.name.clone(),
                        }),
                        ..Default::default()
                    });
                }
            }
        }

        // Check if it's a user-defined enum (no resolve data - no API docs)
        if let Some(variants) = self.user_enums.get(module) {
            for variant in variants {
                if variant.starts_with(prefix) {
                    items.push(CompletionItem {
                        label: variant.clone(),
                        kind: Some(CompletionItemKind::ENUM_MEMBER),
                        detail: Some(format!("{module}::{variant}")),
                        ..Default::default()
                    });
                }
            }
        }
    }

    fn add_struct_callbacks(&self, items: &mut Vec<CompletionItem>, struct_name: &str, prefix: &str) {
        // Check if this is a user-defined struct that extends a game type
        let Some(extends_type) = self.user_structs.get(struct_name).and_then(|e| e.as_ref()) else {
            return;
        };

        // Get callbacks that apply to this game type
        for callback in self.api.callbacks_for_type(extends_type) {
            if callback.name.starts_with(prefix) {
                let signature = format_callback_signature(callback, struct_name);
                items.push(CompletionItem {
                    label: callback.name.clone(),
                    kind: Some(CompletionItemKind::METHOD),
                    detail: Some(format!("callback {signature}")),
                    documentation: format_callback_documentation(callback, struct_name),
                    insert_text: Some(format_callback_snippet(callback, struct_name)),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    data: make_resolve_data(CompletionResolveData::Callback {
                        callback_name: callback.name.clone(),
                        struct_name: struct_name.to_string(),
                    }),
                    ..Default::default()
                });
            }
        }
    }

    fn add_document_symbols(&self, items: &mut Vec<CompletionItem>, prefix: &str) {
        // Add user-defined structs
        for name in self.user_structs.keys() {
            if name.starts_with(prefix) {
                items.push(CompletionItem {
                    label: name.clone(),
                    kind: Some(CompletionItemKind::STRUCT),
                    detail: Some("struct".to_string()),
                    documentation: self.user_docs.get(name).cloned().map(Documentation::String),
                    ..Default::default()
                });
            }
        }

        // Add user-defined enums
        for name in self.user_enums.keys() {
            if name.starts_with(prefix) {
                items.push(CompletionItem {
                    label: name.clone(),
                    kind: Some(CompletionItemKind::ENUM),
                    detail: Some("enum".to_string()),
                    documentation: self.user_docs.get(name).cloned().map(Documentation::String),
                    ..Default::default()
                });
            }
        }
    }

    /// Add snippet completions for control flow statements.
    fn add_snippets(&self, items: &mut Vec<CompletionItem>, prefix: &str) {
        // Control flow snippets
        let snippets = [
            ("if", "if ${1:condition} {\n\t$0\n}", "if statement"),
            ("if else", "if ${1:condition} {\n\t$2\n} else {\n\t$0\n}", "if-else statement"),
            ("if let", "if let ${1:pattern} = ${2:value} {\n\t$0\n}", "if-let statement"),
            ("for", "for ${1:item} in ${2:iterator} {\n\t$0\n}", "for loop"),
            ("let", "let ${1:name} = ${2:value};", "let binding"),
            ("let mut", "let ${1:name} mut= ${2:value};", "mutable let binding"),
            ("fn", "fn ${1:name}(${2:params})${3:: ${4:ReturnType}} {\n\t$0\n}", "function definition"),
            ("pub fn", "pub fn ${1:StructName}::${2:method}(${3:self: &${1:StructName}})${4:: ${5:ReturnType}} {\n\t$0\n}", "public method"),
            ("struct", "pub struct ${1:Name} extend ${2:Signal} {\n\t$0\n}", "struct definition"),
            ("log", "log(\"${1:message}\", ${0});", "log statement"),
        ];

        for (label, snippet, detail) in snippets {
            if label.starts_with(prefix) {
                items.push(CompletionItem {
                    label: label.to_string(),
                    kind: Some(CompletionItemKind::SNIPPET),
                    detail: Some(detail.to_string()),
                    insert_text: Some(snippet.to_string()),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    // Sort snippets after keywords
                    sort_text: Some(format!("2_{label}")),
                    ..Default::default()
                });
            }
        }
    }

    /// Add completions for return statements, prioritizing items matching return type.
    fn add_return_completions(&self, items: &mut Vec<CompletionItem>, prefix: &str) {
        let return_type = self.current_return_type.as_ref();

        // Add local variables, prioritizing ones that match return type
        for (name, type_info) in &self.local_types {
            if name.starts_with(prefix) {
                let matches_return = return_type
                    .map(|rt| self.types_compatible(type_info, rt))
                    .unwrap_or(false);

                items.push(CompletionItem {
                    label: name.clone(),
                    kind: Some(CompletionItemKind::VARIABLE),
                    detail: Some(type_info.to_string()),
                    // Sort matching types first
                    sort_text: Some(format!("{}_{name}", if matches_return { "0" } else { "2" })),
                    ..Default::default()
                });
            }
        }

        // Add enum variants if return type is an enum
        if let Some(TypeInfo::Enum { name: enum_name }) = return_type {
            // Check game enums
            if let Some(enum_def) = self.api.get_enum(enum_name) {
                for variant in &enum_def.variants {
                    if variant.name.starts_with(prefix) || enum_name.starts_with(prefix) {
                        items.push(CompletionItem {
                            label: format!("{enum_name}::{}", variant.name),
                            kind: Some(CompletionItemKind::ENUM_MEMBER),
                            detail: Some(format!("matches return type {enum_name}")),
                            sort_text: Some(format!("0_{}", variant.name)),
                            data: make_resolve_data(CompletionResolveData::EnumVariant {
                                enum_name: enum_name.clone(),
                                variant_name: variant.name.clone(),
                            }),
                            ..Default::default()
                        });
                    }
                }
            }

            // Check user enums
            if let Some(variants) = self.user_enums.get(enum_name) {
                for variant in variants {
                    if variant.starts_with(prefix) || enum_name.starts_with(prefix) {
                        items.push(CompletionItem {
                            label: format!("{enum_name}::{variant}"),
                            kind: Some(CompletionItemKind::ENUM_MEMBER),
                            detail: Some(format!("matches return type {enum_name}")),
                            sort_text: Some(format!("0_{variant}")),
                            ..Default::default()
                        });
                    }
                }
            }
        }

        // Add true/false for bool return types
        if matches!(return_type, Some(TypeInfo::Bool)) {
            if "true".starts_with(prefix) {
                items.push(CompletionItem {
                    label: "true".to_string(),
                    kind: Some(CompletionItemKind::KEYWORD),
                    detail: Some("matches return type bool".to_string()),
                    sort_text: Some("0_true".to_string()),
                    ..Default::default()
                });
            }
            if "false".starts_with(prefix) {
                items.push(CompletionItem {
                    label: "false".to_string(),
                    kind: Some(CompletionItemKind::KEYWORD),
                    detail: Some("matches return type bool".to_string()),
                    sort_text: Some("0_false".to_string()),
                    ..Default::default()
                });
            }
        }

        // Also add general completions with lower priority
        self.add_api_completions(items, prefix);
    }

    /// Check if two types are compatible (basic check).
    fn types_compatible(&self, actual: &TypeInfo, expected: &TypeInfo) -> bool {
        // Unwrap references for comparison
        let actual_unwrapped = match actual {
            TypeInfo::Reference { inner, .. } | TypeInfo::Pointer { inner, .. } => inner.as_ref(),
            other => other,
        };
        let expected_unwrapped = match expected {
            TypeInfo::Reference { inner, .. } | TypeInfo::Pointer { inner, .. } => inner.as_ref(),
            other => other,
        };

        match (actual_unwrapped, expected_unwrapped) {
            (TypeInfo::Bool, TypeInfo::Bool) => true,
            (TypeInfo::I64, TypeInfo::I64) => true,
            (TypeInfo::F64, TypeInfo::F64) => true,
            (TypeInfo::String, TypeInfo::String) => true,
            (TypeInfo::Struct { name: a, .. }, TypeInfo::Struct { name: b, .. }) => a == b,
            (TypeInfo::Enum { name: a }, TypeInfo::Enum { name: b }) => a == b,
            (TypeInfo::Generic { name: a, .. }, TypeInfo::Generic { name: b, .. }) => a == b,
            _ => false,
        }
    }

    /// Collect documentation comments from the source code.
    fn collect_user_docs(&mut self) {
        let root = self.tree.root_node();
        let mut cursor = root.walk();

        // Track the last comment block for each position
        let mut last_comment_end: Option<usize> = None;
        let mut accumulated_doc = String::new();

        for child in root.children(&mut cursor) {
            if child.kind() == kind::COMMENT {
                let comment_text = child.text(self.content);
                // Strip the leading "//" and whitespace
                let doc_line = comment_text.strip_prefix("//").unwrap_or(comment_text).trim();

                if last_comment_end.is_some() && child.start_byte() == last_comment_end.unwrap() + 1 {
                    // Continuation of previous comment block
                    if !accumulated_doc.is_empty() {
                        accumulated_doc.push('\n');
                    }
                    accumulated_doc.push_str(doc_line);
                } else {
                    // New comment block
                    accumulated_doc.clear();
                    accumulated_doc.push_str(doc_line);
                }
                last_comment_end = Some(child.end_byte());
            } else {
                // Check if this is a documentable item right after a comment
                if !accumulated_doc.is_empty() {
                    if let Some(name) = self.get_item_name(child) {
                        self.user_docs.insert(name, accumulated_doc.clone());
                    }
                }
                accumulated_doc.clear();
                last_comment_end = None;
            }
        }
    }

    /// Get the name of a documentable item (struct, enum, function).
    fn get_item_name(&self, node: Node) -> Option<String> {
        match node.kind() {
            kind::STRUCT_DEFINITION | kind::ENUM_DEFINITION => {
                node.child_by_field("name")
                    .map(|n| n.text(self.content).to_string())
            }
            kind::FUNCTION_DEFINITION => {
                node.child_by_field("name")
                    .map(|n| n.text(self.content).to_string())
            }
            _ => None,
        }
    }
}

// ============================================================================
// Completion Context
// ============================================================================

#[derive(Debug)]
enum CompletionContext {
    Type,
    PathAfterColon(String),
    FieldAccess(TypeInfo),
    Return,
    General,
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Convert Position to byte offset.
fn position_to_offset(content: &str, position: Position) -> usize {
    let mut offset = 0;
    for (i, line) in content.lines().enumerate() {
        if i == position.line as usize {
            return offset + (position.character as usize).min(line.len());
        }
        offset += line.len() + 1; // +1 for newline
    }
    content.len()
}

/// Simple type string parser (reuses the concept from TypeInfo).
fn parse_type_string_simple(s: &str) -> TypeInfo {
    let s = s.trim();

    // Handle reference types
    if let Some(inner) = s.strip_prefix('&') {
        let inner = inner.trim();
        if let Some(inner) = inner.strip_prefix("mut ") {
            return TypeInfo::reference(parse_type_string_simple(inner.trim()), true);
        }
        return TypeInfo::reference(parse_type_string_simple(inner), false);
    }

    // Handle pointer types
    if let Some(inner) = s.strip_prefix('*') {
        let inner = inner.trim();
        if let Some(inner) = inner.strip_prefix("mut ") {
            return TypeInfo::pointer(parse_type_string_simple(inner.trim()), true);
        }
        return TypeInfo::pointer(parse_type_string_simple(inner), false);
    }

    // Handle generic types
    if let Some(angle_pos) = s.find('<') {
        if s.ends_with('>') {
            let name = s[..angle_pos].trim();
            let args_str = &s[angle_pos + 1..s.len() - 1];
            let args = parse_generic_args_simple(args_str);
            return TypeInfo::generic(name, args);
        }
    }

    // Primitive types
    match s {
        "bool" => TypeInfo::Bool,
        "i64" | "i32" | "i16" | "i8" | "u64" | "u32" | "u16" | "u8" => TypeInfo::I64,
        "f64" | "f32" => TypeInfo::F64,
        "String" | "string" => TypeInfo::String,
        "void" | "()" => TypeInfo::Void,
        "" => TypeInfo::Unknown,
        _ => TypeInfo::struct_type(s),
    }
}

fn parse_generic_args_simple(s: &str) -> Vec<TypeInfo> {
    let mut args = Vec::new();
    let mut current = String::new();
    let mut depth = 0;

    for c in s.chars() {
        match c {
            '<' => {
                depth += 1;
                current.push(c);
            }
            '>' => {
                depth -= 1;
                current.push(c);
            }
            ',' if depth == 0 => {
                let trimmed = current.trim();
                if !trimmed.is_empty() {
                    args.push(parse_type_string_simple(trimmed));
                }
                current.clear();
            }
            _ => current.push(c),
        }
    }

    let trimmed = current.trim();
    if !trimmed.is_empty() {
        args.push(parse_type_string_simple(trimmed));
    }

    args
}

// ============================================================================
// Formatting Functions
// ============================================================================

fn format_signature(func: &FunctionDef) -> String {
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

fn format_snippet(name: &str, func: &FunctionDef) -> String {
    if func.params.is_empty() {
        return format!("{name}()");
    }

    let placeholders = func
        .params
        .iter()
        .enumerate()
        .map(|(i, p)| format!("${{{}:{}}}", i + 1, p.name))
        .collect::<Vec<_>>()
        .join(", ");

    format!("{name}({placeholders})")
}

fn format_documentation(func: &FunctionDef) -> Option<Documentation> {
    let mut doc = func.doc.clone().unwrap_or_default();

    if !func.params.is_empty() {
        if !doc.is_empty() {
            doc.push_str("\n\n");
        }
        doc.push_str("**Parameters:**\n");
        for p in &func.params {
            let _ = write!(doc, "- `{}`: `{}`", p.name, p.ty);
            if let Some(param_doc) = &p.doc {
                let _ = write!(doc, " — {param_doc}");
            }
            doc.push('\n');
        }
    }

    if let Some(ret) = &func.return_type {
        let _ = write!(doc, "\n**Returns:** `{ret}`");
    }

    if doc.is_empty() {
        None
    } else {
        Some(Documentation::MarkupContent(MarkupContent {
            kind: MarkupKind::Markdown,
            value: doc,
        }))
    }
}

fn format_callback_signature(func: &FunctionDef, struct_name: &str) -> String {
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

fn format_callback_snippet(func: &FunctionDef, struct_name: &str) -> String {
    let params = func
        .params
        .iter()
        .enumerate()
        .map(|(i, p)| {
            let ty = if p.ty == "&Self" {
                format!("&{struct_name}")
            } else {
                p.ty.clone()
            };
            format!("${{{}:{}: {ty}}}", i + 1, p.name)
        })
        .collect::<Vec<_>>()
        .join(", ");

    match &func.return_type {
        Some(ret) => format!("{}({params})$0: {ret} {{\n\t\n}}", func.name),
        None => format!("{}({params})$0 {{\n\t\n}}", func.name),
    }
}

fn format_callback_documentation(func: &FunctionDef, struct_name: &str) -> Option<Documentation> {
    let mut doc = func.doc.clone().unwrap_or_default();

    if !func.params.is_empty() {
        if !doc.is_empty() {
            doc.push_str("\n\n");
        }
        doc.push_str("**Parameters:**\n");
        for p in &func.params {
            let ty = if p.ty == "&Self" {
                format!("&{struct_name}")
            } else {
                p.ty.clone()
            };
            let _ = write!(doc, "- `{}`: `{ty}`", p.name);
            if let Some(param_doc) = &p.doc {
                let _ = write!(doc, " — {param_doc}");
            }
            doc.push('\n');
        }
    }

    if let Some(ret) = &func.return_type {
        let _ = write!(doc, "\n**Returns:** `{ret}`");
    }

    if doc.is_empty() {
        None
    } else {
        Some(Documentation::MarkupContent(MarkupContent {
            kind: MarkupKind::Markdown,
            value: doc,
        }))
    }
}

// ============================================================================
// Public API
// ============================================================================

/// Get completions for a document at a position.
pub fn get_completions(
    doc: &Document,
    position: Position,
    api: &ApiDefinitions,
) -> Vec<CompletionItem> {
    let engine = CompletionEngine::new(&doc.content, api, position);
    engine.completions()
}

// ============================================================================
// Completion Resolve
// ============================================================================

/// Data stored in CompletionItem.data for deferred resolution.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "kind")]
pub enum CompletionResolveData {
    /// API function - resolve documentation
    #[serde(rename = "function")]
    Function { name: String },
    /// API type - resolve documentation
    #[serde(rename = "type")]
    Type { name: String },
    /// API enum - resolve documentation
    #[serde(rename = "enum")]
    Enum { name: String },
    /// API module - resolve documentation
    #[serde(rename = "module")]
    Module { name: String },
    /// Type method - resolve documentation
    #[serde(rename = "method")]
    Method { type_name: String, method_name: String },
    /// Module function - resolve documentation
    #[serde(rename = "module_function")]
    ModuleFunction { module_name: String, function_name: String },
    /// Type field - resolve documentation
    #[serde(rename = "field")]
    Field { type_name: String, field_name: String },
    /// Enum variant - resolve documentation
    #[serde(rename = "enum_variant")]
    EnumVariant { enum_name: String, variant_name: String },
    /// Callback - resolve documentation with struct name substitution
    #[serde(rename = "callback")]
    Callback { callback_name: String, struct_name: String },
}

/// Helper to create resolve data JSON value.
fn make_resolve_data(data: CompletionResolveData) -> Option<Value> {
    serde_json::to_value(data).ok()
}

/// Resolve documentation for a completion item.
pub fn resolve_completion(data: &Value, api: &ApiDefinitions) -> Option<Documentation> {
    let resolve_data: CompletionResolveData = serde_json::from_value(data.clone()).ok()?;

    match resolve_data {
        CompletionResolveData::Function { name } => {
            let func = api.get_function(&name)?;
            format_documentation(func)
        }
        CompletionResolveData::Type { name } => {
            let type_def = api.get_type(&name)?;
            type_def.doc.clone().map(|d| Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: d,
            }))
        }
        CompletionResolveData::Enum { name } => {
            let enum_def = api.get_enum(&name)?;
            enum_def.doc.clone().map(|d| Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: d,
            }))
        }
        CompletionResolveData::Module { name } => {
            let module_def = api.get_module(&name)?;
            module_def.doc.clone().map(|d| Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: d,
            }))
        }
        CompletionResolveData::Method { type_name, method_name } => {
            let type_def = api.get_type(&type_name)?;
            let method = type_def.methods.iter().find(|m| m.name == method_name)?;
            format_documentation(method)
        }
        CompletionResolveData::ModuleFunction { module_name, function_name } => {
            let module_def = api.get_module(&module_name)?;
            let func = module_def.functions.iter().find(|f| f.name == function_name)?;
            format_documentation(func)
        }
        CompletionResolveData::Field { type_name, field_name } => {
            let type_def = api.get_type(&type_name)?;
            let field = type_def.fields.get(&field_name)?;
            field.doc.clone().map(|d| Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: d,
            }))
        }
        CompletionResolveData::EnumVariant { enum_name, variant_name } => {
            let enum_def = api.get_enum(&enum_name)?;
            let variant = enum_def.variants.iter().find(|v| v.name == variant_name)?;
            variant.doc.clone().map(Documentation::String)
        }
        CompletionResolveData::Callback { callback_name, struct_name } => {
            let callback = api.get_callback(&callback_name)?;
            format_callback_documentation(callback, &struct_name)
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    fn load_api() -> ApiDefinitions {
        let toml = include_str!("../../../api-definitions/nimbyrails.v1.toml");
        ApiDefinitions::load_from_str(toml).expect("should parse")
    }

    // Context detection tests

    #[test]
    fn test_context_type_after_colon() {
        let api = load_api();
        let content = "let x: ";
        let engine = CompletionEngine::new(content, &api, Position::new(0, 7));
        let context = engine.determine_context();
        assert!(matches!(context, CompletionContext::Type));
    }

    #[test]
    fn test_context_path_after_double_colon() {
        let api = load_api();
        let content = "SignalCheck::";
        let engine = CompletionEngine::new(content, &api, Position::new(0, 13));
        let context = engine.determine_context();
        assert!(
            matches!(context, CompletionContext::PathAfterColon(name) if name == "SignalCheck")
        );
    }

    #[test]
    fn test_context_path_with_partial() {
        let api = load_api();
        let content = "Math::ab";
        let engine = CompletionEngine::new(content, &api, Position::new(0, 8));
        let context = engine.determine_context();
        assert!(matches!(context, CompletionContext::PathAfterColon(name) if name == "Math"));
    }

    #[test]
    fn test_context_field_after_dot() {
        let api = load_api();
        let content = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test(ctx: &ControlCtx) {
    ctx.
}
"#;
        // Position at the dot
        let engine = CompletionEngine::new(content, &api, Position::new(3, 8));
        let context = engine.determine_context();
        assert!(matches!(context, CompletionContext::FieldAccess(_)));
    }

    #[test]
    fn test_context_general() {
        let api = load_api();
        let content = "let x = ";
        let engine = CompletionEngine::new(content, &api, Position::new(0, 8));
        let context = engine.determine_context();
        assert!(matches!(context, CompletionContext::General));
    }

    // Prefix extraction tests

    #[test]
    fn test_prefix_word_start() {
        let api = load_api();
        let content = "let sig";
        let engine = CompletionEngine::new(content, &api, Position::new(0, 7));
        assert_eq!(engine.get_prefix(), "sig");
    }

    #[test]
    fn test_prefix_mid_word() {
        let api = load_api();
        let content = "SignalChe";
        let engine = CompletionEngine::new(content, &api, Position::new(0, 9));
        assert_eq!(engine.get_prefix(), "SignalChe");
    }

    #[test]
    fn test_prefix_empty() {
        let api = load_api();
        let content = "let ";
        let engine = CompletionEngine::new(content, &api, Position::new(0, 4));
        assert_eq!(engine.get_prefix(), "");
    }

    #[test]
    fn test_prefix_after_operator() {
        let api = load_api();
        let content = "x + y";
        let engine = CompletionEngine::new(content, &api, Position::new(0, 5));
        assert_eq!(engine.get_prefix(), "y");
    }

    // Keyword completion tests

    #[test]
    fn test_keyword_completions() {
        let api = load_api();
        let content = "if";
        let engine = CompletionEngine::new(content, &api, Position::new(0, 2));
        let items = engine.completions();
        assert!(items.iter().any(|i| i.label == "if"));
    }

    #[test]
    fn test_keyword_filter_by_prefix() {
        let api = load_api();
        let content = "ret";
        let engine = CompletionEngine::new(content, &api, Position::new(0, 3));
        let items = engine.completions();
        let keywords: Vec<_> = items
            .iter()
            .filter(|i| i.kind == Some(CompletionItemKind::KEYWORD))
            .collect();
        assert_eq!(keywords.len(), 1);
        assert_eq!(keywords[0].label, "return");
    }

    // Primitive type completion tests

    #[test]
    fn test_primitive_type_completions() {
        let api = load_api();
        let content = "let x: i";
        let engine = CompletionEngine::new(content, &api, Position::new(0, 8));
        let items = engine.completions();
        assert!(items.iter().any(|i| i.label == "i64"));
        assert!(items.iter().any(|i| i.label == "i32"));
    }

    #[test]
    fn test_id_generic_completion() {
        let api = load_api();
        let content = "let x: ID";
        let engine = CompletionEngine::new(content, &api, Position::new(0, 9));
        let items = engine.completions();
        let id_item = items.iter().find(|i| i.label == "ID");
        assert!(id_item.is_some());
        assert!(id_item.unwrap().insert_text.as_ref().is_some_and(|t| t.contains("<")));
    }

    // API type completion tests

    #[test]
    fn test_api_type_completions() {
        let api = load_api();
        let content = "let x: Sig";
        let engine = CompletionEngine::new(content, &api, Position::new(0, 10));
        let items = engine.completions();
        assert!(items.iter().any(|i| i.label == "Signal"));
    }

    // Module member completion tests

    #[test]
    fn test_type_method_completions() {
        let api = load_api();
        let content = "DB::";
        let engine = CompletionEngine::new(content, &api, Position::new(0, 4));
        let items = engine.completions();
        assert!(!items.is_empty(), "DB should have methods");
        assert!(items.iter().any(|i| i.label == "view"), "DB should have view method");
    }

    #[test]
    fn test_enum_variant_completions() {
        let api = load_api();
        let content = "SignalCheck::";
        let engine = CompletionEngine::new(content, &api, Position::new(0, 13));
        let items = engine.completions();
        assert!(items.iter().any(|i| i.label == "Pass"));
        assert!(items.iter().any(|i| i.label == "Stop"));
    }

    // Field access completion tests

    #[test]
    fn test_field_access_completions() {
        let api = load_api();
        let content = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test(ctx: &ControlCtx) {
    ctx.
}
"#;
        let engine = CompletionEngine::new(content, &api, Position::new(3, 8));
        let items = engine.completions();
        // ControlCtx should have db and extrapolator fields
        assert!(items.iter().any(|i| i.label == "db"), "should have db field");
        assert!(
            items.iter().any(|i| i.label == "extrapolator"),
            "should have extrapolator field"
        );
    }

    #[test]
    fn test_chained_field_access() {
        let api = load_api();
        let content = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test(ctx: &ControlCtx) {
    ctx.db.
}
"#;
        let engine = CompletionEngine::new(content, &api, Position::new(3, 11));
        let items = engine.completions();
        // DB should have view method
        assert!(items.iter().any(|i| i.label == "view"), "DB should have view method");
    }

    // User struct field completion tests

    #[test]
    fn test_user_struct_field_completions() {
        let api = load_api();
        let content = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct MyHandler extend Signal {
    owner: ID<Train>,
    count: i64,
}
fn MyHandler::test(self: &MyHandler) {
    self.
}
"#;
        let engine = CompletionEngine::new(content, &api, Position::new(7, 9));
        let items = engine.completions();
        assert!(items.iter().any(|i| i.label == "owner"), "should have owner field");
        assert!(items.iter().any(|i| i.label == "count"), "should have count field");
    }

    // Callback completion tests

    #[test]
    fn test_struct_callback_completions() {
        let api = load_api();
        let content = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct MySignal extend Signal { }
MySignal::
"#;
        let engine = CompletionEngine::new(content, &api, Position::new(3, 10));
        let items = engine.completions();
        assert!(items.iter().any(|i| i.label == "event_signal_check"));
    }

    // Local variable completion tests

    #[test]
    fn test_local_variable_completions() {
        let api = load_api();
        let content = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn test(ctx: &ControlCtx, train: &Train) {

}
"#;
        let engine = CompletionEngine::new(content, &api, Position::new(3, 4));
        let items = engine.completions();
        assert!(items.iter().any(|i| i.label == "ctx"), "should have ctx parameter");
        assert!(items.iter().any(|i| i.label == "train"), "should have train parameter");
    }

    // Snippet formatting tests

    #[test]
    fn test_snippet_with_params() {
        let func = FunctionDef {
            name: "test".to_string(),
            params: vec![
                nimbyscript_analyzer::ParamDef {
                    name: "a".to_string(),
                    ty: "i64".to_string(),
                    doc: None,
                    is_mut: false,
                    is_ref: false,
                },
                nimbyscript_analyzer::ParamDef {
                    name: "b".to_string(),
                    ty: "f64".to_string(),
                    doc: None,
                    is_mut: false,
                    is_ref: false,
                },
            ],
            return_type: None,
            doc: None,
            type_params: vec![],
            for_type: None,
        };
        let snippet = format_snippet("test", &func);
        assert_eq!(snippet, "test(${1:a}, ${2:b})");
    }

    #[test]
    fn test_snippet_no_params() {
        let func = FunctionDef {
            name: "empty".to_string(),
            params: vec![],
            return_type: None,
            doc: None,
            type_params: vec![],
            for_type: None,
        };
        let snippet = format_snippet("empty", &func);
        assert_eq!(snippet, "empty()");
    }

    #[test]
    fn test_format_signature() {
        let func = FunctionDef {
            name: "add".to_string(),
            params: vec![nimbyscript_analyzer::ParamDef {
                name: "a".to_string(),
                ty: "i64".to_string(),
                doc: None,
                is_mut: false,
                is_ref: false,
            }],
            return_type: Some("i64".to_string()),
            doc: None,
            type_params: vec![],
            for_type: None,
        };
        let sig = format_signature(&func);
        assert_eq!(sig, "(a: i64) -> i64");
    }

    // Integration test

    #[test]
    fn test_get_completions_general_context() {
        let api = load_api();
        let content = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct MyStruct { }
"#;
        let doc = Document::new(content.to_string(), Some(&api));
        let items = get_completions(&doc, Position::new(2, 0), &api);
        assert!(items.iter().any(|i| i.kind == Some(CompletionItemKind::KEYWORD)));
        assert!(items.iter().any(|i| i.kind == Some(CompletionItemKind::STRUCT)));
    }

    // Completion resolve tests

    #[test]
    fn test_callback_completion_has_resolve_data() {
        let api = load_api();
        let content = r#"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct MySignal extend Signal { }
MySignal::
"#;
        let engine = CompletionEngine::new(content, &api, Position::new(3, 10));
        let items = engine.completions();

        let pass_by = items.iter().find(|i| i.label == "event_signal_pass_by");
        assert!(pass_by.is_some(), "should have event_signal_pass_by callback");
        let pass_by = pass_by.unwrap();

        // Verify data field is set
        assert!(pass_by.data.is_some(), "callback should have resolve data");

        // Verify we can deserialize it
        let data: CompletionResolveData = serde_json::from_value(pass_by.data.clone().unwrap())
            .expect("should deserialize resolve data");
        assert!(matches!(data, CompletionResolveData::Callback { .. }));
    }

    #[test]
    fn test_resolve_callback_documentation() {
        let api = load_api();

        // Create resolve data for event_signal_pass_by
        let data = CompletionResolveData::Callback {
            callback_name: "event_signal_pass_by".to_string(),
            struct_name: "MySignal".to_string(),
        };
        let json_data = serde_json::to_value(data).expect("should serialize");

        // Resolve documentation
        let doc = resolve_completion(&json_data, &api);
        assert!(doc.is_some(), "should resolve documentation for callback");

        // Check the documentation contains expected content
        if let Some(Documentation::MarkupContent(content)) = doc {
            assert!(content.value.contains("train passes"), "doc should mention 'train passes'");
        } else {
            panic!("expected MarkupContent documentation");
        }
    }

    #[test]
    fn test_resolve_function_documentation() {
        let api = load_api();

        let data = CompletionResolveData::Function { name: "abs".to_string() };
        let json_data = serde_json::to_value(data).expect("should serialize");

        let doc = resolve_completion(&json_data, &api);
        assert!(doc.is_some(), "should resolve documentation for function");

        if let Some(Documentation::MarkupContent(content)) = doc {
            assert!(content.value.contains("absolute value"), "doc should mention 'absolute value'");
        } else {
            panic!("expected MarkupContent documentation");
        }
    }

    #[test]
    fn test_resolve_method_documentation() {
        let api = load_api();

        let data = CompletionResolveData::Method {
            type_name: "DB".to_string(),
            method_name: "view".to_string(),
        };
        let json_data = serde_json::to_value(data).expect("should serialize");

        let doc = resolve_completion(&json_data, &api);
        assert!(doc.is_some(), "should resolve documentation for method");

        if let Some(Documentation::MarkupContent(content)) = doc {
            assert!(content.value.contains("script data"), "doc should mention 'script data'");
        } else {
            panic!("expected MarkupContent documentation");
        }
    }

    #[test]
    fn test_function_completion_has_documentation_and_resolve_data() {
        let api = load_api();
        let content = "ab";
        let engine = CompletionEngine::new(content, &api, Position::new(0, 2));
        let items = engine.completions();

        let abs_item = items.iter().find(|i| i.label == "abs");
        assert!(abs_item.is_some(), "should have abs function");
        let abs_item = abs_item.unwrap();

        // Verify inline documentation is included for immediate display
        assert!(abs_item.documentation.is_some(), "should have inline documentation");

        // Verify data field is also set for resolve fallback
        assert!(abs_item.data.is_some(), "function should have resolve data");
    }
}
