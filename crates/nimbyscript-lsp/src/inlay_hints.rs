//! Inlay hints provider for NimbyScript LSP.
//!
//! Provides two types of hints:
//! - Type hints: Show inferred types for let bindings without explicit annotations
//! - Parameter hints: Show parameter names at function call arguments

use std::collections::HashMap;

use tower_lsp::lsp_types::*;

use nimbyscript_analyzer::{collect_declarations, ApiDefinitions, SemanticContext, TypeInfo};
use nimbyscript_parser::{kind, Node, NodeExt};

use crate::document::Document;
use crate::type_inference::{
    base_type_name, find_ancestor_of_kind, get_enclosing_struct_name, infer_node_type,
    parse_type_string, unwrap_to_type_name, TypeContext,
};

// ============================================================================
// Public API
// ============================================================================

/// Get inlay hints for a document within a range.
pub fn get_inlay_hints(doc: &Document, range: Range, api: &ApiDefinitions) -> Vec<InlayHint> {
    let engine = InlayHintEngine::new(doc, api, range);
    engine.collect_hints()
}

// ============================================================================
// Inlay Hint Engine
// ============================================================================

/// Engine for computing inlay hints.
struct InlayHintEngine<'a> {
    doc: &'a Document,
    content: &'a str,
    api: &'a ApiDefinitions,
    range: Range,
    /// User-defined struct fields (struct_name -> (field_name -> field_type))
    struct_fields: HashMap<String, HashMap<String, TypeInfo>>,
    /// User-defined structs (name -> extends type)
    user_structs: HashMap<String, Option<String>>,
}

impl<'a> InlayHintEngine<'a> {
    fn new(doc: &'a Document, api: &'a ApiDefinitions, range: Range) -> Self {
        // Build semantic context to collect declarations
        let mut ctx = SemanticContext::new(&doc.content, doc.tree(), api);
        collect_declarations(&mut ctx);

        Self {
            doc,
            content: &doc.content,
            api,
            range,
            struct_fields: ctx.struct_fields,
            user_structs: ctx.user_structs,
        }
    }

    /// Create a TypeContext from this engine's state.
    fn type_context(&self) -> TypeContext<'_> {
        TypeContext {
            content: self.content,
            api: self.api,
            struct_fields: &self.struct_fields,
            user_structs: &self.user_structs,
        }
    }

    /// Collect all inlay hints within the range.
    fn collect_hints(&self) -> Vec<InlayHint> {
        let mut hints = Vec::new();
        let root = self.doc.tree().root_node();

        self.collect_hints_recursive(root, &mut hints);

        hints
    }

    /// Recursively walk the AST and collect hints.
    fn collect_hints_recursive(&self, node: Node, hints: &mut Vec<InlayHint>) {
        // Check if node is within range
        if !self.node_intersects_range(node) {
            return;
        }

        match node.kind() {
            kind::LET_STATEMENT | kind::LET_ELSE_STATEMENT => {
                if let Some(hint) = self.type_hint_for_let(node) {
                    hints.push(hint);
                }
            }
            kind::CALL_EXPRESSION => {
                self.param_hints_for_call(node, hints);
            }
            _ => {}
        }

        // Recurse into children
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.collect_hints_recursive(child, hints);
        }
    }

    /// Check if a node intersects with the requested range.
    fn node_intersects_range(&self, node: Node) -> bool {
        let start_pos = self.doc.offset_to_position(node.start_byte());
        let end_pos = self.doc.offset_to_position(node.end_byte());

        // Node ends before range starts
        if end_pos.line < self.range.start.line {
            return false;
        }
        if end_pos.line == self.range.start.line && end_pos.character < self.range.start.character {
            return false;
        }

        // Node starts after range ends
        if start_pos.line > self.range.end.line {
            return false;
        }
        if start_pos.line == self.range.end.line && start_pos.character > self.range.end.character {
            return false;
        }

        true
    }

    // ========================================================================
    // Type Hints
    // ========================================================================

    /// Generate a type hint for a let statement without explicit type annotation.
    fn type_hint_for_let(&self, node: Node) -> Option<InlayHint> {
        // The let statement has a binding child which contains name, type, and value
        let binding = node.child_by_kind(kind::BINDING)?;

        // Skip if there's an explicit type annotation
        let mut cursor = binding.walk();
        let has_explicit_type = binding
            .children(&mut cursor)
            .any(|c| c.kind() == kind::TYPE || c.kind() == "type_pattern");
        if has_explicit_type {
            return None;
        }

        let name_node = binding.child_by_field("name")?;
        let value_node = binding.child_by_field("value")?;

        // Infer type from the initializer
        let type_info = self.infer_type_in_function(node, value_node)?;

        // Skip if type is unknown
        if matches!(type_info, TypeInfo::Unknown) {
            return None;
        }

        let type_name = type_info.type_name();

        // Position after the variable name
        let position = self.doc.offset_to_position(name_node.end_byte());

        Some(InlayHint {
            position,
            label: InlayHintLabel::String(format!(": {type_name}")),
            kind: Some(InlayHintKind::TYPE),
            text_edits: None,
            tooltip: None,
            padding_left: Some(false),
            padding_right: Some(false),
            data: None,
        })
    }

    /// Infer the type of an expression within a function context.
    fn infer_type_in_function(&self, let_node: Node, expr: Node) -> Option<TypeInfo> {
        // Find the enclosing function to get local variable types
        let func_node = find_ancestor_of_kind(let_node, kind::FUNCTION_DEFINITION)?;
        let local_types = self.collect_locals_before(func_node, let_node.start_byte());
        let enclosing_struct = get_enclosing_struct_name(func_node, self.content);

        // Use shared type inference
        let ctx = self.type_context();
        infer_node_type(&ctx, expr, &local_types, enclosing_struct.as_deref())
    }

    /// Collect local variable types declared before a given offset.
    fn collect_locals_before(
        &self,
        func_node: Node,
        before_offset: usize,
    ) -> HashMap<String, TypeInfo> {
        let mut locals = HashMap::new();

        // Add function parameters
        if let Some(params_node) = func_node.child_by_kind(kind::PARAMETERS) {
            let mut cursor = params_node.walk();
            for child in params_node.children(&mut cursor) {
                if child.kind() != kind::PARAMETER {
                    continue;
                }
                let Some(name) = child.child_by_field("name") else {
                    continue;
                };
                let Some(ty) = child.child_by_field("type") else {
                    continue;
                };
                locals.insert(
                    name.text(self.content).to_string(),
                    parse_type_string(ty.text(self.content)),
                );
            }
        }

        // Add let bindings before the current position
        if let Some(body) = func_node.child_by_field("body") {
            self.collect_bindings_before(body, before_offset, &mut locals);
        }

        locals
    }

    /// Recursively collect let bindings before an offset.
    fn collect_bindings_before(
        &self,
        node: Node,
        before_offset: usize,
        locals: &mut HashMap<String, TypeInfo>,
    ) {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.start_byte() >= before_offset {
                break;
            }

            // Recurse into blocks
            if child.kind() == kind::BLOCK {
                self.collect_bindings_before(child, before_offset, locals);
                continue;
            }

            let is_let =
                child.kind() == kind::LET_STATEMENT || child.kind() == kind::LET_ELSE_STATEMENT;
            if !is_let {
                continue;
            }

            // Get the binding node which contains name, type, and value
            let Some(binding) = child.child_by_kind(kind::BINDING) else {
                continue;
            };

            let Some(name_node) = binding.child_by_field("name") else {
                continue;
            };

            let name = name_node.text(self.content).to_string();

            // First check for explicit type annotation
            let type_info = if let Some(t) = binding.child_by_field("type") {
                parse_type_string(t.text(self.content))
            } else if let Some(value_node) = binding.child_by_field("value") {
                // Try to infer type from the value expression
                self.infer_node_type(value_node, locals)
                    .unwrap_or(TypeInfo::Unknown)
            } else {
                TypeInfo::Unknown
            };

            locals.insert(name, type_info);
        }
    }

    // ========================================================================
    // Parameter Hints
    // ========================================================================

    /// Generate parameter hints for a function call.
    fn param_hints_for_call(&self, node: Node, hints: &mut Vec<InlayHint>) {
        let Some(callee) = node.child_by_field("function") else {
            return;
        };

        let Some(args_node) = node.child_by_kind("arguments") else {
            return;
        };

        // Collect argument nodes
        let mut cursor = args_node.walk();
        let args: Vec<Node> = args_node
            .children(&mut cursor)
            .filter(|c| c.kind() != "," && c.kind() != "(" && c.kind() != ")")
            .collect();

        if args.is_empty() {
            return;
        }

        // Get parameter names based on callee type
        let params = self.get_param_names_for_call(callee, node);
        let Some(params) = params else { return };

        // Generate hints for each argument
        for (i, arg) in args.iter().enumerate() {
            if i >= params.len() {
                break;
            }

            let param_name = &params[i];

            // Skip if argument already matches parameter name
            if self.arg_matches_param(arg, param_name) {
                continue;
            }

            let position = self.doc.offset_to_position(arg.start_byte());

            hints.push(InlayHint {
                position,
                label: InlayHintLabel::String(format!("{param_name}:")),
                kind: Some(InlayHintKind::PARAMETER),
                text_edits: None,
                tooltip: None,
                padding_left: Some(false),
                padding_right: Some(true),
                data: None,
            });
        }
    }

    /// Get parameter names for a function call.
    fn get_param_names_for_call(&self, callee: Node, call_node: Node) -> Option<Vec<String>> {
        match callee.kind() {
            kind::IDENTIFIER => {
                // Global function call: foo(...)
                let name = callee.text(self.content);
                if let Some(func) = self.api.get_function(name) {
                    return Some(func.params.iter().map(|p| p.name.clone()).collect());
                }
                None
            }
            kind::PATH_EXPRESSION => {
                // Could be: simple function (pow), Module::func, or Type::method
                let text = callee.text(self.content);

                // Simple path without :: is a global function
                if !text.contains("::") {
                    if let Some(func) = self.api.get_function(text) {
                        return Some(func.params.iter().map(|p| p.name.clone()).collect());
                    }
                    return None;
                }

                // Module or type function: Math::abs(...) or Type::method(...)
                let parts: Vec<&str> = text.split("::").collect();
                if parts.len() != 2 {
                    return None;
                }
                let prefix = parts[0];
                let name = parts[1];

                // Try module function
                if let Some(module) = self.api.get_module(prefix) {
                    if let Some(func) = module.functions.iter().find(|f| f.name == name) {
                        return Some(func.params.iter().map(|p| p.name.clone()).collect());
                    }
                }

                // Try type method
                if let Some(type_def) = self.api.get_type(prefix) {
                    if let Some(method) = type_def.methods.iter().find(|m| m.name == name) {
                        return Some(method.params.iter().map(|p| p.name.clone()).collect());
                    }
                }

                // Try user struct callback
                if let Some(extends_type) = self.user_structs.get(prefix).and_then(|e| e.as_ref()) {
                    if let Some(callback) = self
                        .api
                        .callbacks_for_type(extends_type)
                        .into_iter()
                        .find(|c| c.name == name)
                    {
                        return Some(callback.params.iter().map(|p| p.name.clone()).collect());
                    }
                }

                None
            }
            kind::FIELD_ACCESS => {
                // Method call: obj.method(...)
                let method_name_node = callee.child_by_field("field")?;
                let object_node = callee.child_by_field("object")?;
                let method_name = method_name_node.text(self.content);

                // Find the enclosing function to get local types
                let func_node = find_ancestor_of_kind(call_node, kind::FUNCTION_DEFINITION)?;
                let local_types = self.collect_locals_before(func_node, call_node.start_byte());

                let object_type = self.infer_node_type(object_node, &local_types)?;
                let type_name = unwrap_to_type_name(&object_type)?;
                let base_type_name = base_type_name(&type_name);

                if let Some(type_def) = self.api.get_type(&base_type_name) {
                    if let Some(method) = type_def.methods.iter().find(|m| m.name == method_name) {
                        return Some(method.params.iter().map(|p| p.name.clone()).collect());
                    }
                }

                None
            }
            _ => None,
        }
    }

    /// Check if an argument already matches the parameter name.
    fn arg_matches_param(&self, arg: &Node, param_name: &str) -> bool {
        match arg.kind() {
            kind::IDENTIFIER | kind::PATH_EXPRESSION => {
                let text = arg.text(self.content);
                // Handle path expressions like "self.field"
                if text.contains('.') {
                    let parts: Vec<&str> = text.split('.').collect();
                    parts.last() == Some(&param_name)
                } else if text.contains("::") {
                    let parts: Vec<&str> = text.split("::").collect();
                    parts.last() == Some(&param_name)
                } else {
                    text == param_name
                }
            }
            kind::FIELD_ACCESS => {
                if let Some(field) = arg.child_by_field("field") {
                    field.text(self.content) == param_name
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    // ========================================================================
    // Type Inference
    // ========================================================================

    /// Infer the type of an AST node.
    fn infer_node_type(
        &self,
        node: Node,
        local_types: &HashMap<String, TypeInfo>,
    ) -> Option<TypeInfo> {
        self.infer_node_type_with_context(node, local_types, None)
    }

    /// Infer the type of an AST node with optional enclosing struct context.
    /// The enclosing_struct is used to resolve `Self` to the actual struct type.
    fn infer_node_type_with_context(
        &self,
        node: Node,
        local_types: &HashMap<String, TypeInfo>,
        enclosing_struct: Option<&str>,
    ) -> Option<TypeInfo> {
        match node.kind() {
            kind::IDENTIFIER => {
                let name = node.text(self.content);
                local_types.get(name).cloned()
            }
            kind::PATH_EXPRESSION => {
                let text = node.text(self.content);
                if text.contains("::") {
                    // Path like Foo::bar - enum variant or static
                    None
                } else {
                    local_types.get(text).cloned()
                }
            }
            kind::FIELD_ACCESS => {
                let object = node.child_by_field("object")?;
                let field = node.child_by_field("field")?;
                let object_type =
                    self.infer_node_type_with_context(object, local_types, enclosing_struct)?;
                let type_name = Self::resolve_type_name(&object_type, enclosing_struct)?;
                let field_name = field.text(self.content);

                // Check user struct fields
                if let Some(fields) = self.struct_fields.get(&type_name) {
                    if let Some(field_type) = fields.get(field_name) {
                        return Some(field_type.clone());
                    }
                }

                // Check API type fields
                if let Some(type_def) = self.api.get_type(&type_name) {
                    if let Some(field_def) = type_def.fields.get(field_name) {
                        return Some(parse_type_string(&field_def.ty));
                    }
                }

                None
            }
            kind::CALL_EXPRESSION => {
                self.infer_call_expression_type_with_context(node, local_types, enclosing_struct)
            }
            kind::NUMBER => Some(self.infer_number_type(node)),
            kind::BOOLEAN => Some(TypeInfo::Bool),
            kind::STRING_LITERAL => Some(TypeInfo::String),
            kind::UNARY_EXPRESSION => {
                // For negation, check operand
                if let Some(operand) = node.child_by_field("operand") {
                    self.infer_node_type_with_context(operand, local_types, enclosing_struct)
                } else {
                    None
                }
            }
            kind::BINARY_EXPRESSION => {
                // Binary expressions don't have named fields, get first named child (left operand)
                let mut cursor = node.walk();
                let left = node.children(&mut cursor).find(Node::is_named);
                left.and_then(|l| {
                    self.infer_node_type_with_context(l, local_types, enclosing_struct)
                })
            }
            _ => None,
        }
    }

    /// Resolve a type name, handling `Self` resolution via enclosing struct.
    fn resolve_type_name(ty: &TypeInfo, enclosing_struct: Option<&str>) -> Option<String> {
        let name = unwrap_to_type_name(ty)?;
        if name == "Self" {
            enclosing_struct.map(ToString::to_string)
        } else {
            Some(name)
        }
    }

    /// Infer return type for a default struct method (new, clone).
    fn infer_default_method_return_type(
        &self,
        method: &nimbyscript_analyzer::FunctionDef,
        type_name: &str,
        base_type_name: &str,
    ) -> Option<TypeInfo> {
        let ret = method.return_type.as_ref()?;
        if ret == "Self" {
            Some(TypeInfo::Struct {
                name: type_name.to_string(),
                extends: self.user_structs.get(base_type_name).cloned().flatten(),
            })
        } else {
            Some(parse_type_string(ret))
        }
    }

    /// Infer the type of a number literal.
    fn infer_number_type(&self, node: Node) -> TypeInfo {
        let text = node.text(self.content);
        if text.contains('.') {
            TypeInfo::F64
        } else {
            TypeInfo::I64
        }
    }

    /// Infer the return type of a call expression with enclosing struct context.
    fn infer_call_expression_type_with_context(
        &self,
        node: Node,
        local_types: &HashMap<String, TypeInfo>,
        enclosing_struct: Option<&str>,
    ) -> Option<TypeInfo> {
        let callee = node.child_by_field("function")?;

        match callee.kind() {
            kind::IDENTIFIER => {
                // Global function
                let name = callee.text(self.content);
                let func = self.api.get_function(name)?;
                func.return_type.as_ref().map(|t| parse_type_string(t))
            }
            kind::PATH_EXPRESSION => {
                // Could be: simple function (pow), Module::func, or Type::method
                let text = callee.text(self.content);

                // Simple path without :: is a global function
                if !text.contains("::") {
                    let func = self.api.get_function(text)?;
                    return func.return_type.as_ref().map(|t| parse_type_string(t));
                }

                // Module function or type method
                let parts: Vec<&str> = text.split("::").collect();
                if parts.len() != 2 {
                    return None;
                }
                let prefix = parts[0];
                let name = parts[1];

                // Try module function
                if let Some(module) = self.api.get_module(prefix) {
                    if let Some(func) = module.functions.iter().find(|f| f.name == name) {
                        return func.return_type.as_ref().map(|t| parse_type_string(t));
                    }
                }

                // Try type static method
                if let Some(type_def) = self.api.get_type(prefix) {
                    if let Some(method) = type_def.methods.iter().find(|m| m.name == name) {
                        return method.return_type.as_ref().map(|t| parse_type_string(t));
                    }
                }

                None
            }
            kind::FIELD_ACCESS => {
                // Method call
                let object = callee.child_by_field("object")?;
                let method = callee.child_by_field("field")?;
                let object_type =
                    self.infer_node_type_with_context(object, local_types, enclosing_struct)?;
                let type_name = Self::resolve_type_name(&object_type, enclosing_struct)?;
                let method_name = method.text(self.content);
                let base_type_name = base_type_name(&type_name);

                // Check for default struct methods (e.g., clone on private structs)
                if let Some(default_method) = self.api.get_default_struct_method(method_name) {
                    return self.infer_default_method_return_type(
                        default_method,
                        &type_name,
                        &base_type_name,
                    );
                }

                let type_def = self.api.get_type(&base_type_name)?;
                let method_def = type_def.methods.iter().find(|m| m.name == method_name)?;
                let return_type = method_def
                    .return_type
                    .as_ref()
                    .map(|t| parse_type_string(t))?;

                // First, check for explicit type arguments on the method call (e.g., view<Hitcher>)
                let mut generic_args =
                    Self::extract_explicit_type_args(callee, self.content, self.api);

                // If no explicit type args, try to get generic args from object type (e.g., std::optional<T>.get())
                if generic_args.is_empty() {
                    generic_args = Self::extract_generic_args(&object_type);
                }

                // If still no args and method has type_params, try to infer from arguments
                if generic_args.is_empty() && !method_def.type_params.is_empty() {
                    generic_args = self.infer_type_params_from_args(node, method_def, local_types);
                }

                Some(Self::substitute_type_params(return_type, &generic_args))
            }
            _ => None,
        }
    }

    // ========================================================================
    // Helpers
    // ========================================================================

    /// Extract generic type arguments from a type, unwrapping references/pointers.
    /// For `std::optional<Motion::Drive>`, returns `[Motion::Drive]`.
    fn extract_generic_args(ty: &TypeInfo) -> Vec<TypeInfo> {
        match ty {
            TypeInfo::Generic { args, .. } => args.clone(),
            TypeInfo::Reference { inner, .. } | TypeInfo::Pointer { inner, .. } => {
                Self::extract_generic_args(inner)
            }
            _ => Vec::new(),
        }
    }

    /// Extract explicit type arguments from a field_access node's type_arguments field.
    /// For `view<Hitcher>`, returns `[Hitcher]`.
    fn extract_explicit_type_args(
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
    /// For `DB.view(id: ID<T>) -> *T`, if called with `ID<Signal>`, returns `[Signal]`.
    fn infer_type_params_from_args(
        &self,
        call_node: Node,
        method_def: &nimbyscript_analyzer::FunctionDef,
        local_types: &HashMap<String, TypeInfo>,
    ) -> Vec<TypeInfo> {
        // Get the arguments node
        let Some(args_node) = call_node.child_by_kind("arguments") else {
            return Vec::new();
        };

        // Collect argument nodes
        let mut cursor = args_node.walk();
        let args: Vec<Node> = args_node
            .children(&mut cursor)
            .filter(|c| c.kind() != "," && c.kind() != "(" && c.kind() != ")")
            .collect();

        // For each type parameter, try to find it in parameter types and extract from args
        method_def
            .type_params
            .iter()
            .map(|type_param| {
                self.find_type_param_in_args(type_param, &args, method_def, local_types)
                    .unwrap_or(TypeInfo::Unknown)
            })
            .collect()
    }

    /// Find a type parameter value by matching against argument types.
    fn find_type_param_in_args(
        &self,
        type_param: &str,
        args: &[Node],
        method_def: &nimbyscript_analyzer::FunctionDef,
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
            let Some(arg_type) = self.infer_node_type(args[i], local_types) else {
                continue;
            };

            // Extract the generic argument that corresponds to T
            if let Some(t) = Self::extract_type_param_from_arg(&arg_type, &param_def.ty, type_param)
            {
                return Some(t);
            }
        }
        None
    }

    /// Extract a type parameter value from an argument type given the parameter pattern.
    /// For arg_type `ID<Signal>`, param_pattern `ID<T>`, type_param `T`, returns `Signal`.
    fn extract_type_param_from_arg(
        arg_type: &TypeInfo,
        param_pattern: &str,
        type_param: &str,
    ) -> Option<TypeInfo> {
        // Parse the parameter pattern to understand its structure
        let pattern_type = parse_type_string(param_pattern);

        // If pattern is Generic { name, args } where one arg matches the type_param
        let TypeInfo::Generic {
            name: pattern_name,
            args: pattern_args,
        } = &pattern_type
        else {
            return None;
        };

        // Find which position has the type parameter
        let position = pattern_args.iter().position(|pattern_arg| {
            matches!(pattern_arg, TypeInfo::Struct { name, .. } if name == type_param)
        })?;

        // Now extract the position-th arg from the actual arg_type
        let actual_args = Self::extract_generic_args(arg_type);
        let actual_arg = actual_args.get(position)?;

        // Also verify the base types match
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
    /// For example, `*T` with args `[Motion::Drive]` becomes `*Motion::Drive`.
    fn substitute_type_params(ty: TypeInfo, generic_args: &[TypeInfo]) -> TypeInfo {
        match ty {
            // T is typically the first generic argument
            TypeInfo::Struct { ref name, .. } if name == "T" => {
                generic_args.first().cloned().unwrap_or(ty)
            }
            TypeInfo::Reference { inner, is_mut } => TypeInfo::Reference {
                inner: Box::new(Self::substitute_type_params(*inner, generic_args)),
                is_mut,
            },
            TypeInfo::Pointer { inner, is_mut } => TypeInfo::Pointer {
                inner: Box::new(Self::substitute_type_params(*inner, generic_args)),
                is_mut,
            },
            TypeInfo::Generic { name, args } => TypeInfo::Generic {
                name,
                args: args
                    .into_iter()
                    .map(|a| Self::substitute_type_params(a, generic_args))
                    .collect(),
            },
            TypeInfo::Array { element, size } => TypeInfo::Array {
                element: Box::new(Self::substitute_type_params(*element, generic_args)),
                size,
            },
            other => other,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_api() -> ApiDefinitions {
        let api_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("should have parent dir")
            .parent()
            .expect("should have grandparent dir")
            .join("api-definitions/nimbyrails.v1.toml");
        ApiDefinitions::load_from_file(&api_path).expect("Failed to load API")
    }

    fn full_range() -> Range {
        Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 1000,
                character: 0,
            },
        }
    }

    #[test]
    fn test_type_hint_for_literal() {
        let api = make_api();
        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub fn test() {
    let x = 42;
    let y = true;
    let z = 3.14;
}
";
        let doc = Document::new(code.to_string(), Some(&api));
        let hints = get_inlay_hints(&doc, full_range(), &api);

        // Should have type hints for x, y, z
        let type_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == Some(InlayHintKind::TYPE))
            .collect();

        assert_eq!(type_hints.len(), 3, "Should have 3 type hints");

        let labels: Vec<_> = type_hints
            .iter()
            .map(|h| match &h.label {
                InlayHintLabel::String(s) => s.as_str(),
                InlayHintLabel::LabelParts(_) => "",
            })
            .collect();

        assert!(labels.contains(&": i64"), "Should have i64 hint");
        assert!(labels.contains(&": bool"), "Should have bool hint");
        assert!(labels.contains(&": f64"), "Should have f64 hint");
    }

    #[test]
    fn test_no_type_hint_when_explicit() {
        let api = make_api();
        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub fn test() {
    let x: i64 = 42;
}
";
        let doc = Document::new(code.to_string(), Some(&api));
        let hints = get_inlay_hints(&doc, full_range(), &api);

        let type_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == Some(InlayHintKind::TYPE))
            .collect();

        assert!(
            type_hints.is_empty(),
            "Should have no type hints when explicit"
        );
    }

    #[test]
    fn test_param_hints_for_global_function() {
        let api = make_api();
        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub fn test() {
    let x = pow(2.0, 3.0);
}
";
        let doc = Document::new(code.to_string(), Some(&api));
        let hints = get_inlay_hints(&doc, full_range(), &api);

        let param_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == Some(InlayHintKind::PARAMETER))
            .collect();

        assert_eq!(
            param_hints.len(),
            2,
            "Should have 2 parameter hints for pow"
        );

        let labels: Vec<_> = param_hints
            .iter()
            .map(|h| match &h.label {
                InlayHintLabel::String(s) => s.as_str(),
                InlayHintLabel::LabelParts(_) => "",
            })
            .collect();

        assert!(labels.contains(&"base:"), "Should have 'base:' hint");
        assert!(
            labels.contains(&"exponent:"),
            "Should have 'exponent:' hint"
        );
    }

    #[test]
    fn test_skip_hint_when_arg_matches_param() {
        let api = make_api();
        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub fn test() {
    let base = 2.0;
    let x = pow(base, 3.0);
}
";
        let doc = Document::new(code.to_string(), Some(&api));
        let hints = get_inlay_hints(&doc, full_range(), &api);

        let param_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == Some(InlayHintKind::PARAMETER))
            .collect();

        // Should only have hint for exponent, not base
        assert_eq!(param_hints.len(), 1, "Should have 1 parameter hint");

        let label = match &param_hints[0].label {
            InlayHintLabel::String(s) => s.as_str(),
            InlayHintLabel::LabelParts(_) => "",
        };
        assert_eq!(label, "exponent:", "Should only have 'exponent:' hint");
    }

    #[test]
    fn test_type_hint_from_method_call() {
        let api = make_api();
        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal {}
pub fn Test::event_signal_check(
    self: &Test,
    ctx: &EventCtx,
    train: &Train,
    arrive_time: f64,
    position: &Signal,
    dir: TrackDir
) -> SignalCheck {
    let forward = position.forward();
    SignalCheck::Pass
}
";
        let doc = Document::new(code.to_string(), Some(&api));
        let hints = get_inlay_hints(&doc, full_range(), &api);

        let type_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == Some(InlayHintKind::TYPE))
            .collect();

        assert!(
            !type_hints.is_empty(),
            "Should have type hint for 'forward'"
        );

        // Check that the hint is for Pos type (Signal.forward() returns Pos)
        let forward_hint = type_hints.iter().find(|h| {
            if let InlayHintLabel::String(s) = &h.label {
                s.contains("Pos")
            } else {
                false
            }
        });
        assert!(forward_hint.is_some(), "Should have Pos type hint");
    }

    #[test]
    fn test_range_filtering() {
        let api = make_api();
        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub fn test() {
    let x = 42;
    let y = true;
}
";
        let doc = Document::new(code.to_string(), Some(&api));

        // Request hints only for line 2 (let x = 42)
        let range = Range {
            start: Position {
                line: 2,
                character: 0,
            },
            end: Position {
                line: 2,
                character: 100,
            },
        };
        let hints = get_inlay_hints(&doc, range, &api);

        let type_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == Some(InlayHintKind::TYPE))
            .collect();

        assert_eq!(type_hints.len(), 1, "Should have 1 type hint in range");
    }

    #[test]
    fn test_type_hint_resolves_generic_return_type() {
        let api = make_api();
        // motion.drive is std::optional<Motion::Drive>
        // .get() returns *T, which should resolve to *Motion::Drive
        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal {}
pub fn Test::control_train(self: &Test, ctx: &ControlCtx, train: &Train, motion: &Motion) {
    let drive = motion.drive.get();
}
";
        let doc = Document::new(code.to_string(), Some(&api));
        let hints = get_inlay_hints(&doc, full_range(), &api);

        let type_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == Some(InlayHintKind::TYPE))
            .collect();

        assert!(!type_hints.is_empty(), "Should have type hint for 'drive'");

        // Check that the hint shows *Motion::Drive, not *T
        let drive_hint = type_hints.iter().find(|h| {
            if let InlayHintLabel::String(s) = &h.label {
                s.contains("Motion::Drive")
            } else {
                false
            }
        });
        assert!(
            drive_hint.is_some(),
            "Should resolve T to Motion::Drive, got: {:?}",
            type_hints
                .iter()
                .map(|h| match &h.label {
                    InlayHintLabel::String(s) => s.as_str(),
                    InlayHintLabel::LabelParts(_) => "",
                })
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_type_hint_resolves_generic_with_let_else() {
        let api = make_api();
        // Test with let-else syntax (&=)
        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal {}
pub fn Test::control_train(self: &Test, ctx: &ControlCtx, train: &Train, motion: &Motion) {
    let drive &= motion.drive.get() else { return; };
}
";
        let doc = Document::new(code.to_string(), Some(&api));
        let hints = get_inlay_hints(&doc, full_range(), &api);

        eprintln!("let-else hints: {hints:?}");

        let type_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == Some(InlayHintKind::TYPE))
            .collect();

        assert!(!type_hints.is_empty(), "Should have type hint for 'drive'");

        // With &=, the type should be &Motion::Drive (dereferenced pointer)
        let drive_hint = type_hints.iter().find(|h| {
            if let InlayHintLabel::String(s) = &h.label {
                s.contains("Motion::Drive")
            } else {
                false
            }
        });
        assert!(
            drive_hint.is_some(),
            "Should resolve T to Motion::Drive, got: {:?}",
            type_hints
                .iter()
                .map(|h| match &h.label {
                    InlayHintLabel::String(s) => s.as_str(),
                    InlayHintLabel::LabelParts(_) => "",
                })
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_example_file_inlay_hints() {
        let api = make_api();
        let example_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("should have parent dir")
            .parent()
            .expect("should have grandparent dir")
            .join("tests/fixtures/valid/example.nimbyscript");
        let code = std::fs::read_to_string(&example_path).expect("Failed to read example file");
        let doc = Document::new(code, Some(&api));
        let hints = get_inlay_hints(&doc, full_range(), &api);

        // Print all type hints for debugging
        let type_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == Some(InlayHintKind::TYPE))
            .collect();

        eprintln!("=== Type hints from example.nimbyscript ===");
        for hint in &type_hints {
            let label = match &hint.label {
                InlayHintLabel::String(s) => s.as_str(),
                InlayHintLabel::LabelParts(_) => "<parts>",
            };
            eprintln!("  Line {}: {}", hint.position.line, label);
        }

        // Check that none of the hints contain unresolved "T"
        let unresolved_hints: Vec<_> = type_hints
            .iter()
            .filter(|h| {
                if let InlayHintLabel::String(s) = &h.label {
                    // Check for *T or &T but not actual type names containing T
                    s == ": *T" || s == ": &T" || s == ": T"
                } else {
                    false
                }
            })
            .collect();

        assert!(
            unresolved_hints.is_empty(),
            "Found unresolved generic type parameters: {:?}",
            unresolved_hints
                .iter()
                .map(|h| format!(
                    "line {}: {}",
                    h.position.line,
                    match &h.label {
                        InlayHintLabel::String(s) => s.as_str(),
                        InlayHintLabel::LabelParts(_) => "<parts>",
                    }
                ))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_type_hint_for_user_struct_field() {
        let api = make_api();
        // Access a field of a user struct to ensure field type inference works
        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct MySignal extend Signal {
    count: i64,
}
pub fn MySignal::event_signal_check(
    self: &MySignal,
    ctx: &EventCtx,
    train: &Train,
    motion: &Motion,
    signal: &Signal
): SignalCheck {
    let val = self.count;
    return SignalCheck::Pass;
}
";
        let doc = Document::new(code.to_string(), Some(&api));
        let hints = get_inlay_hints(&doc, full_range(), &api);

        let type_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == Some(InlayHintKind::TYPE))
            .collect();

        assert!(!type_hints.is_empty(), "Should have type hint for 'val'");

        let val_hint = type_hints.iter().find(|h| {
            if let InlayHintLabel::String(s) = &h.label {
                s.contains("i64")
            } else {
                false
            }
        });
        assert!(val_hint.is_some(), "Should have i64 type hint for val");
    }

    #[test]
    fn test_type_hint_for_api_field() {
        let api = make_api();
        // Access an API type field
        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal {}
pub fn Test::event_signal_check(
    self: &Test,
    ctx: &EventCtx,
    train: &Train,
    motion: &Motion,
    signal: &Signal
): SignalCheck {
    let id = train.id;
    return SignalCheck::Pass;
}
";
        let doc = Document::new(code.to_string(), Some(&api));
        let hints = get_inlay_hints(&doc, full_range(), &api);

        let type_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == Some(InlayHintKind::TYPE))
            .collect();

        assert!(!type_hints.is_empty(), "Should have type hint for 'id'");

        let id_hint = type_hints.iter().find(|h| {
            if let InlayHintLabel::String(s) = &h.label {
                s.contains("ID<Train>")
            } else {
                false
            }
        });
        assert!(id_hint.is_some(), "Should have ID<Train> type hint for id");
    }

    #[test]
    fn test_param_hints_for_method_call() {
        let api = make_api();
        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal {
    target: ID<Signal>,
}
pub fn Test::event_signal_check(
    self: &Test,
    ctx: &EventCtx,
    train: &Train,
    motion: &Motion,
    signal: &Signal
): SignalCheck {
    let sig &= ctx.db.view(self.target) else { return SignalCheck::Pass; }
    return SignalCheck::Pass;
}
";
        let doc = Document::new(code.to_string(), Some(&api));
        let hints = get_inlay_hints(&doc, full_range(), &api);

        let param_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == Some(InlayHintKind::PARAMETER))
            .collect();

        // DB.view takes an id parameter
        assert!(
            !param_hints.is_empty(),
            "Should have parameter hint for view()"
        );
    }

    #[test]
    fn test_no_hints_for_empty_function() {
        let api = make_api();
        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub fn test() {
}
";
        let doc = Document::new(code.to_string(), Some(&api));
        let hints = get_inlay_hints(&doc, full_range(), &api);

        assert!(hints.is_empty(), "Should have no hints for empty function");
    }

    #[test]
    fn test_type_hint_for_binary_expression() {
        let api = make_api();
        // Test binary expression type inference in a callback context
        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal {}
pub fn Test::event_signal_check(
    self: &Test,
    ctx: &EventCtx,
    train: &Train,
    motion: &Motion,
    signal: &Signal
): SignalCheck {
    let sum = 1 + 2;
    let product = 3.0 * 4.0;
    return SignalCheck::Pass;
}
";
        let doc = Document::new(code.to_string(), Some(&api));
        let hints = get_inlay_hints(&doc, full_range(), &api);

        let type_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == Some(InlayHintKind::TYPE))
            .collect();

        assert_eq!(type_hints.len(), 2, "Should have 2 type hints");

        let labels: Vec<_> = type_hints
            .iter()
            .map(|h| match &h.label {
                InlayHintLabel::String(s) => s.as_str(),
                InlayHintLabel::LabelParts(_) => "",
            })
            .collect();

        assert!(labels.contains(&": i64"), "Should have i64 for sum");
        assert!(labels.contains(&": f64"), "Should have f64 for product");
    }

    #[test]
    fn test_type_hint_for_unary_expression() {
        let api = make_api();
        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub fn test() {
    let neg = -42;
}
";
        let doc = Document::new(code.to_string(), Some(&api));
        let hints = get_inlay_hints(&doc, full_range(), &api);

        let type_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == Some(InlayHintKind::TYPE))
            .collect();

        assert_eq!(type_hints.len(), 1, "Should have 1 type hint for neg");

        let label = match &type_hints[0].label {
            InlayHintLabel::String(s) => s.as_str(),
            InlayHintLabel::LabelParts(_) => "",
        };
        assert_eq!(label, ": i64", "Should have i64 for negated integer");
    }

    #[test]
    fn test_type_hint_for_string_literal() {
        let api = make_api();
        // Need function context for type inference
        let code = r#"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal {}
pub fn Test::event_signal_check(
    self: &Test,
    ctx: &EventCtx,
    train: &Train,
    motion: &Motion,
    signal: &Signal
): SignalCheck {
    let msg = "hello";
    return SignalCheck::Pass;
}
"#;
        let doc = Document::new(code.to_string(), Some(&api));
        let hints = get_inlay_hints(&doc, full_range(), &api);

        let type_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == Some(InlayHintKind::TYPE))
            .collect();

        assert_eq!(type_hints.len(), 1, "Should have 1 type hint for msg");

        let label = match &type_hints[0].label {
            InlayHintLabel::String(s) => s.as_str(),
            InlayHintLabel::LabelParts(_) => "",
        };
        // Type name is "String" (capitalized)
        assert_eq!(label, ": String", "Should have String type");
    }

    #[test]
    fn test_param_hints_for_abs_function() {
        let api = make_api();
        // abs function takes a "value" parameter
        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal {}
pub fn Test::event_signal_check(
    self: &Test,
    ctx: &EventCtx,
    train: &Train,
    motion: &Motion,
    signal: &Signal
): SignalCheck {
    let result = abs(-5);
    return SignalCheck::Pass;
}
";
        let doc = Document::new(code.to_string(), Some(&api));
        let hints = get_inlay_hints(&doc, full_range(), &api);

        let param_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == Some(InlayHintKind::PARAMETER))
            .collect();

        // abs takes "value" as param
        let value_hint = param_hints.iter().find(|h| {
            if let InlayHintLabel::String(s) = &h.label {
                s.contains("value")
            } else {
                false
            }
        });
        assert!(
            value_hint.is_some(),
            "Should have 'value:' hint for abs, got: {:?}",
            param_hints
                .iter()
                .map(|h| match &h.label {
                    InlayHintLabel::String(s) => s.as_str(),
                    InlayHintLabel::LabelParts(_) => "",
                })
                .collect::<Vec<_>>()
        );
    }
}
