//! Hover provider for NimbyScript LSP.
//!
//! Provides rich hover information for symbols, API types, and callbacks.

use std::collections::HashMap;
use std::fmt::Write;

use tower_lsp::lsp_types::*;

use nimbyscript_analyzer::{
    collect_declarations, types::parse_type_string, ApiDefinitions, FunctionDef, SemanticContext,
    TypeInfo,
};
use nimbyscript_parser::{kind, Node, NodeExt};

use crate::document::Document;

/// Type alias for (name, type) pairs used for params and bindings
type NameTypePairs = Vec<(String, TypeInfo)>;

/// Extract parameter (name, type) as strings from a PARAMETER node.
fn extract_param_strings(param_node: Node, content: &str) -> Option<(String, String)> {
    let name = param_node.child_by_field("name")?;
    let ty = param_node.child_by_field("type")?;
    Some((name.text(content).to_string(), ty.text(content).to_string()))
}

/// Extract all parameters from a PARAMETERS node as (name, type) string pairs.
fn extract_params_from_node(params_node: Node, content: &str) -> Vec<(String, String)> {
    let mut cursor = params_node.walk();
    params_node
        .children(&mut cursor)
        .filter(|c| c.kind() == kind::PARAMETER)
        .filter_map(|c| extract_param_strings(c, content))
        .collect()
}

/// Extract parameter (name, TypeInfo) from a PARAMETER node.
fn extract_param_type_info(param_node: Node, content: &str) -> Option<(String, TypeInfo)> {
    let name = param_node.child_by_field("name")?;
    let ty = param_node.child_by_field("type")?;
    Some((
        name.text(content).to_string(),
        parse_type_string(ty.text(content)),
    ))
}

/// Extract all parameters from a PARAMETERS node as (name, TypeInfo) pairs.
fn extract_params_with_types(params_node: Node, content: &str) -> Vec<(String, TypeInfo)> {
    let mut cursor = params_node.walk();
    params_node
        .children(&mut cursor)
        .filter(|c| c.kind() == kind::PARAMETER)
        .filter_map(|c| extract_param_type_info(c, content))
        .collect()
}

/// Extract binding (name, type) from a let statement node.
fn extract_binding_from_let(node: Node, content: &str) -> Option<(String, TypeInfo)> {
    let name_node = node.child_by_field("name")?;
    let name = name_node.text(content).to_string();
    let type_info = node
        .child_by_field("type")
        .map_or(TypeInfo::Unknown, |t| parse_type_string(t.text(content)));
    Some((name, type_info))
}

// ============================================================================
// Public API
// ============================================================================

/// Get hover information for a document at a position.
pub fn get_hover(doc: &Document, position: Position, api: &ApiDefinitions) -> Option<Hover> {
    let engine = HoverEngine::new(doc, api, position);
    engine.hover()
}

// ============================================================================
// Hover Context
// ============================================================================

/// The context of what is being hovered over.
#[derive(Debug)]
enum HoverContext {
    /// User-defined struct definition
    UserStruct {
        name: String,
        extends: Option<String>,
        fields: Vec<(String, String)>,
    },
    /// User-defined function or method
    UserFunction {
        name: String,
        params: Vec<(String, String)>,
        return_type: Option<String>,
        doc: Option<String>,
    },
    /// API type (Signal, Train, etc.)
    ApiType(String),
    /// API function (abs, sqrt, etc.)
    ApiFunction(String),
    /// API method on a type (e.g., signal.forward())
    ApiMethod {
        type_name: String,
        method_name: String,
    },
    /// API enum variant (e.g., SignalCheck::Pass)
    ApiEnumVariant {
        enum_name: String,
        variant_name: String,
    },
    /// API field on a type (e.g., ctx.db)
    ApiField {
        type_name: String,
        field_name: String,
    },
    /// User-defined struct field (e.g., motion.presence)
    UserField {
        struct_name: String,
        field_name: String,
        field_type: String,
    },
    /// Callback function in a user struct (e.g., event_signal_check)
    Callback {
        struct_name: String,
        callback_name: String,
        extends_type: String,
    },
    /// Local variable
    LocalVariable { name: String, type_name: String },
    /// User-defined enum
    UserEnum { name: String, variants: Vec<String> },
}

// ============================================================================
// Hover Engine
// ============================================================================

/// Engine for computing hover information.
struct HoverEngine<'a> {
    doc: &'a Document,
    content: &'a str,
    api: &'a ApiDefinitions,
    offset: usize,
    /// Local variable types
    local_types: HashMap<String, TypeInfo>,
    /// User-defined struct fields (struct_name -> (field_name -> field_type))
    struct_fields: HashMap<String, HashMap<String, TypeInfo>>,
    /// User-defined structs (name -> extends type)
    user_structs: HashMap<String, Option<String>>,
    /// User-defined enums (name -> variants)
    user_enums: HashMap<String, Vec<String>>,
}

impl<'a> HoverEngine<'a> {
    fn new(doc: &'a Document, api: &'a ApiDefinitions, position: Position) -> Self {
        let offset = doc.position_to_offset(position);

        // Build semantic context to collect declarations
        let mut ctx = SemanticContext::new(&doc.content, doc.tree(), api);
        collect_declarations(&mut ctx);

        let struct_fields = ctx.struct_fields.clone();
        let user_structs = ctx.user_structs.clone();
        let user_enums = ctx.user_enums.clone();

        let mut engine = Self {
            doc,
            content: &doc.content,
            api,
            offset,
            local_types: HashMap::new(),
            struct_fields,
            user_structs,
            user_enums,
        };

        engine.collect_local_types();
        engine
    }

    /// Compute hover information for the current position.
    fn hover(&self) -> Option<Hover> {
        let context = self.determine_context()?;
        let contents = self.format_hover(&context);

        Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: contents,
            }),
            range: None,
        })
    }

    // ========================================================================
    // Context Detection
    // ========================================================================

    /// Determine what is being hovered over.
    fn determine_context(&self) -> Option<HoverContext> {
        let root = self.doc.tree().root_node();

        // Find the smallest node containing the cursor
        let node = self.find_deepest_node_at(root)?;

        // Try various context detection strategies
        self.detect_struct_definition(node)
            .or_else(|| self.detect_function_definition(node))
            .or_else(|| self.detect_enum_definition(node))
            .or_else(|| self.detect_field_access(node))
            .or_else(|| self.detect_method_call(node))
            .or_else(|| self.detect_type_reference(node))
            .or_else(|| self.detect_path_expression(node))
            .or_else(|| self.detect_identifier(node))
    }

    /// Find the deepest AST node containing the cursor position.
    fn find_deepest_node_at<'b>(&self, node: Node<'b>) -> Option<Node<'b>> {
        let start = node.start_byte();
        let end = node.end_byte();

        // tree-sitter end_byte is exclusive, so use >= for end check
        if self.offset < start || self.offset >= end {
            return None;
        }

        // Try to find a more specific child node
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(deeper) = self.find_deepest_node_at(child) {
                return Some(deeper);
            }
        }

        Some(node)
    }

    /// Detect if hovering over a struct definition.
    fn detect_struct_definition(&self, node: Node) -> Option<HoverContext> {
        // Walk up to find struct_definition
        let struct_node = Self::find_ancestor_of_kind(node, kind::STRUCT_DEFINITION)?;
        let name_node = struct_node.child_by_field("name")?;

        // Only trigger if cursor is on the struct name
        if !self.cursor_in_node(name_node) {
            return None;
        }

        let name = name_node.text(self.content).to_string();
        let extends = self.doc.struct_extends(&name).map(String::from);

        // Collect fields by iterating through children
        let mut fields = Vec::new();
        let mut cursor = struct_node.walk();
        for child in struct_node.children(&mut cursor) {
            if child.kind() == kind::STRUCT_FIELD {
                if let (Some(field_name), Some(field_type)) =
                    (child.child_by_field("name"), child.child_by_field("type"))
                {
                    fields.push((
                        field_name.text(self.content).to_string(),
                        field_type.text(self.content).to_string(),
                    ));
                }
            }
        }

        Some(HoverContext::UserStruct {
            name,
            extends,
            fields,
        })
    }

    /// Detect if hovering over a function definition.
    fn detect_function_definition(&self, node: Node) -> Option<HoverContext> {
        let func_node = Self::find_ancestor_of_kind(node, kind::FUNCTION_DEFINITION)?;
        let name_node = func_node.child_by_field("name")?;

        // Only trigger if cursor is on the function name
        if !self.cursor_in_node(name_node) {
            return None;
        }

        let name = name_node.text(self.content).to_string();

        // Check if this is a callback (method on a struct that extends a game type)
        if let Some(hover_ctx) = self.try_detect_callback(&name) {
            return Some(hover_ctx);
        }

        // Collect parameters
        let params = func_node
            .child_by_kind(kind::PARAMETERS)
            .map(|pn| extract_params_from_node(pn, self.content))
            .unwrap_or_default();

        let return_type = func_node
            .child_by_field("return_type")
            .map(|rt| rt.text(self.content).to_string());

        Some(HoverContext::UserFunction {
            name,
            params,
            return_type,
            doc: None,
        })
    }

    /// Try to detect if a function name is a callback (e.g., MyStruct::on_tick).
    fn try_detect_callback(&self, name: &str) -> Option<HoverContext> {
        if !name.contains("::") {
            return None;
        }
        let parts: Vec<&str> = name.split("::").collect();
        if parts.len() != 2 {
            return None;
        }
        let struct_name = parts[0];
        let method_name = parts[1];
        let extends_type = self.doc.struct_extends(struct_name)?;
        if !self.api.is_valid_callback(method_name) {
            return None;
        }
        Some(HoverContext::Callback {
            struct_name: struct_name.to_string(),
            callback_name: method_name.to_string(),
            extends_type: extends_type.to_string(),
        })
    }

    /// Detect if hovering over an enum definition.
    fn detect_enum_definition(&self, node: Node) -> Option<HoverContext> {
        let enum_node = Self::find_ancestor_of_kind(node, kind::ENUM_DEFINITION)?;
        let name_node = enum_node.child_by_field("name")?;

        // Only trigger if cursor is on the enum name
        if !self.cursor_in_node(name_node) {
            return None;
        }

        let name = name_node.text(self.content).to_string();

        // Collect variants by iterating through children
        let mut variants = Vec::new();
        let mut cursor = enum_node.walk();
        for child in enum_node.children(&mut cursor) {
            if child.kind() == kind::ENUM_VARIANT {
                if let Some(variant_name) = child.child_by_field("name") {
                    variants.push(variant_name.text(self.content).to_string());
                }
            }
        }

        Some(HoverContext::UserEnum { name, variants })
    }

    /// Detect if hovering over a field access (e.g., ctx.db).
    fn detect_field_access(&self, node: Node) -> Option<HoverContext> {
        // Look for field_access node
        let field_node = Self::find_ancestor_of_kind(node, kind::FIELD_ACCESS)?;

        // Get the field name being accessed
        let field_name_node = field_node.child_by_field("field")?;

        // Only trigger if cursor is on the field name
        if !self.cursor_in_node(field_name_node) {
            return None;
        }

        let field_name = field_name_node.text(self.content).to_string();

        // Get the object being accessed
        let object_node = field_node.child_by_field("object")?;
        let object_type = self.infer_node_type(object_node)?;
        let type_name = Self::unwrap_to_type_name(&object_type)?;

        // Check if this is an API field
        if let Some(type_def) = self.api.get_type(&type_name) {
            if type_def.fields.contains_key(&field_name) {
                return Some(HoverContext::ApiField {
                    type_name,
                    field_name,
                });
            }
        }

        // Check if this is a user struct field
        if let Some(fields) = self.struct_fields.get(&type_name) {
            if let Some(field_type) = fields.get(&field_name) {
                return Some(HoverContext::UserField {
                    struct_name: type_name,
                    field_name,
                    field_type: field_type.type_name(),
                });
            }
        }

        None
    }

    /// Detect if hovering over a method call (e.g., db.view()).
    fn detect_method_call(&self, node: Node) -> Option<HoverContext> {
        // Look for call_expression with a field_access as callee
        let call_node = Self::find_ancestor_of_kind(node, kind::CALL_EXPRESSION)?;
        let callee = call_node.child_by_field("function")?;

        if callee.kind() != kind::FIELD_ACCESS {
            return None;
        }

        let method_name_node = callee.child_by_field("field")?;

        // Only trigger if cursor is on the method name
        if !self.cursor_in_node(method_name_node) {
            return None;
        }

        let method_name = method_name_node.text(self.content).to_string();
        let object_node = callee.child_by_field("object")?;
        let object_type = self.infer_node_type(object_node)?;
        let type_name = Self::unwrap_to_type_name(&object_type)?;

        // Get the base type name (strip generic parameters)
        let base_type_name = Self::base_type_name(&type_name);

        // Verify this is an API method
        if let Some(type_def) = self.api.get_type(&base_type_name) {
            if type_def.methods.iter().any(|m| m.name == method_name) {
                return Some(HoverContext::ApiMethod {
                    type_name: base_type_name,
                    method_name,
                });
            }
        }

        None
    }

    /// Extract the base type name from a potentially generic type.
    /// e.g., "Option<Presence>" -> "Option"
    fn base_type_name(type_name: &str) -> String {
        if let Some(idx) = type_name.find('<') {
            type_name[..idx].to_string()
        } else {
            type_name.to_string()
        }
    }

    /// Detect if hovering over a type reference in a type annotation (e.g., ID<Train>).
    fn detect_type_reference(&self, node: Node) -> Option<HoverContext> {
        // Check if we're in a type_identifier context
        let type_id_node = Self::find_ancestor_of_kind(node, kind::TYPE_IDENTIFIER)?;

        // Get the full type text (e.g., "ID<Train>") for API lookup
        let full_type = type_id_node.text(self.content);

        // Get the base type name (the identifier part before any generic arguments)
        let base_type_name = if node.kind() == kind::IDENTIFIER {
            // We're on the type name identifier itself
            node.text(self.content)
        } else {
            // Find the identifier child
            let mut cursor = type_id_node.walk();
            let mut found_name = None;
            for child in type_id_node.children(&mut cursor) {
                if child.kind() == kind::IDENTIFIER {
                    found_name = Some(child.text(self.content));
                    break;
                }
            }
            found_name?
        };

        // Try full type first (e.g., "ID<Train>"), then base type (e.g., "ID")
        // This handles both specific generic instantiations and regular types
        let type_to_check = if self.api.get_type(full_type).is_some() {
            full_type
        } else if self.api.get_type(base_type_name).is_some() {
            base_type_name
        } else {
            // Neither found in API, try other options below
            ""
        };

        // Check if this is an API type
        if !type_to_check.is_empty() && self.api.get_type(type_to_check).is_some() {
            return Some(HoverContext::ApiType(type_to_check.to_string()));
        }

        // Check if this is an API enum (use base type name)
        if self.api.get_enum(base_type_name).is_some() {
            return Some(HoverContext::ApiType(base_type_name.to_string()));
        }

        // Check if it's a user-defined struct (use base type name)
        if self.user_structs.contains_key(base_type_name) {
            let extends = self.doc.struct_extends(base_type_name).map(String::from);
            let fields = self
                .struct_fields
                .get(base_type_name)
                .map(|f| f.iter().map(|(k, v)| (k.clone(), v.type_name())).collect())
                .unwrap_or_default();
            return Some(HoverContext::UserStruct {
                name: base_type_name.to_string(),
                extends,
                fields,
            });
        }

        // Check if it's a user-defined enum
        if let Some(variants) = self.user_enums.get(base_type_name) {
            return Some(HoverContext::UserEnum {
                name: base_type_name.to_string(),
                variants: variants.clone(),
            });
        }

        None
    }

    /// Detect if hovering over a path expression (e.g., SignalCheck::Pass).
    fn detect_path_expression(&self, node: Node) -> Option<HoverContext> {
        let path_node = Self::find_ancestor_of_kind(node, kind::PATH_EXPRESSION)?;
        let text = path_node.text(self.content);

        if !text.contains("::") {
            return None;
        }

        let parts: Vec<&str> = text.split("::").collect();
        if parts.len() != 2 {
            return None;
        }

        let type_name = parts[0];
        let member_name = parts[1];

        // Check if it's an enum variant
        if let Some(enum_def) = self.api.get_enum(type_name) {
            if enum_def.variants.iter().any(|v| v.name == member_name) {
                return Some(HoverContext::ApiEnumVariant {
                    enum_name: type_name.to_string(),
                    variant_name: member_name.to_string(),
                });
            }
        }

        // Check user-defined enums
        if let Some(variants) = self.user_enums.get(type_name) {
            if variants.contains(&member_name.to_string()) {
                return Some(HoverContext::ApiEnumVariant {
                    enum_name: type_name.to_string(),
                    variant_name: member_name.to_string(),
                });
            }
        }

        None
    }

    /// Detect if hovering over a simple identifier.
    fn detect_identifier(&self, node: Node) -> Option<HoverContext> {
        if node.kind() != kind::IDENTIFIER {
            return None;
        }

        let name = node.text(self.content);

        // Check if it's a local variable
        if let Some(type_info) = self.local_types.get(name) {
            let type_name = type_info.type_name();
            return Some(HoverContext::LocalVariable {
                name: name.to_string(),
                type_name,
            });
        }

        // Check if it's an API type
        if self.api.get_type(name).is_some() {
            return Some(HoverContext::ApiType(name.to_string()));
        }

        // Check if it's an API function
        if self.api.get_function(name).is_some() {
            return Some(HoverContext::ApiFunction(name.to_string()));
        }

        // Check if it's an API enum
        if self.api.get_enum(name).is_some() {
            return Some(HoverContext::ApiType(name.to_string()));
        }

        // Check if it's a user-defined struct
        if self.user_structs.contains_key(name) {
            let extends = self.doc.struct_extends(name).map(String::from);
            let fields = self
                .struct_fields
                .get(name)
                .map(|f| f.iter().map(|(k, v)| (k.clone(), v.type_name())).collect())
                .unwrap_or_default();
            return Some(HoverContext::UserStruct {
                name: name.to_string(),
                extends,
                fields,
            });
        }

        // Check if it's a user-defined enum
        if let Some(variants) = self.user_enums.get(name) {
            return Some(HoverContext::UserEnum {
                name: name.to_string(),
                variants: variants.clone(),
            });
        }

        None
    }

    // ========================================================================
    // Helpers
    // ========================================================================

    fn cursor_in_node(&self, node: Node) -> bool {
        self.offset >= node.start_byte() && self.offset <= node.end_byte()
    }

    fn find_ancestor_of_kind<'b>(node: Node<'b>, target_kind: &str) -> Option<Node<'b>> {
        let mut current = Some(node);
        while let Some(n) = current {
            if n.kind() == target_kind {
                return Some(n);
            }
            current = n.parent();
        }
        None
    }

    /// Infer the type of an AST node.
    fn infer_node_type(&self, node: Node) -> Option<TypeInfo> {
        match node.kind() {
            kind::IDENTIFIER => {
                let name = node.text(self.content);
                self.local_types.get(name).cloned()
            }
            kind::PATH_EXPRESSION => {
                // Path expressions can be simple identifiers like "motion"
                // or paths like "Foo::bar"
                let text = node.text(self.content);
                if text.contains("::") {
                    // Path like Foo::bar - not supported for type inference yet
                    None
                } else {
                    // Simple identifier - look up in local types
                    self.local_types.get(text).cloned()
                }
            }
            kind::FIELD_ACCESS => {
                let object = node.child_by_field("object")?;
                let field = node.child_by_field("field")?;
                let object_type = self.infer_node_type(object)?;
                let type_name = Self::unwrap_to_type_name(&object_type)?;
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
            kind::CALL_EXPRESSION => self.infer_call_expression_type(node),
            _ => None,
        }
    }

    /// Infer the return type of a call expression.
    fn infer_call_expression_type(&self, node: Node) -> Option<TypeInfo> {
        let callee = node.child_by_field("function")?;
        if callee.kind() != kind::FIELD_ACCESS {
            return None;
        }
        let object = callee.child_by_field("object")?;
        let method = callee.child_by_field("field")?;
        let object_type = self.infer_node_type(object)?;
        let type_name = Self::unwrap_to_type_name(&object_type)?;
        let method_name = method.text(self.content);

        let type_def = self.api.get_type(&type_name)?;
        let method_def = type_def.methods.iter().find(|m| m.name == method_name)?;
        method_def
            .return_type
            .as_ref()
            .map(|t| parse_type_string(t))
    }

    fn unwrap_to_type_name(ty: &TypeInfo) -> Option<String> {
        match ty {
            TypeInfo::Struct { name, .. } | TypeInfo::Enum { name } => Some(name.clone()),
            TypeInfo::Reference { inner, .. } | TypeInfo::Pointer { inner, .. } => {
                Self::unwrap_to_type_name(inner)
            }
            TypeInfo::Generic { name, args } => {
                if args.is_empty() {
                    Some(name.clone())
                } else {
                    let arg_names: Vec<_> =
                        args.iter().filter_map(Self::unwrap_to_type_name).collect();
                    Some(format!("{}<{}>", name, arg_names.join(", ")))
                }
            }
            _ => None,
        }
    }

    /// Collect local variable types from the enclosing function.
    fn collect_local_types(&mut self) {
        let root = self.doc.tree().root_node();
        if let Some((params, bindings)) = self.find_function_locals(root) {
            for (name, ty) in params {
                self.local_types.insert(name, ty);
            }
            for (name, ty) in bindings {
                self.local_types.insert(name, ty);
            }
        }
    }

    fn find_function_locals(&self, node: Node) -> Option<(NameTypePairs, NameTypePairs)> {
        if node.kind() == kind::FUNCTION_DEFINITION {
            let start = node.start_byte();
            let end = node.end_byte();
            if start <= self.offset && self.offset <= end {
                let mut params = Vec::new();
                let mut bindings = Vec::new();

                // Extract parameters
                if let Some(params_node) = node.child_by_kind(kind::PARAMETERS) {
                    params = extract_params_with_types(params_node, self.content);
                }

                // Extract let bindings
                if let Some(body) = node.child_by_field("body") {
                    self.collect_bindings(body, &mut bindings);
                }

                return Some((params, bindings));
            }
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(result) = self.find_function_locals(child) {
                return Some(result);
            }
        }

        None
    }

    fn collect_bindings(&self, node: Node, bindings: &mut Vec<(String, TypeInfo)>) {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.start_byte() >= self.offset {
                break;
            }

            let is_let =
                child.kind() == kind::LET_STATEMENT || child.kind() == kind::LET_ELSE_STATEMENT;
            if is_let {
                if let Some(binding) = extract_binding_from_let(child, self.content) {
                    bindings.push(binding);
                }
            }

            // Recurse into blocks
            if child.kind() == kind::BLOCK {
                self.collect_bindings(child, bindings);
            }
        }
    }

    // ========================================================================
    // Formatting
    // ========================================================================

    /// Format hover information based on context.
    fn format_hover(&self, context: &HoverContext) -> String {
        match context {
            HoverContext::UserStruct {
                name,
                extends,
                fields,
            } => self.format_user_struct(name, extends.as_deref(), fields),

            HoverContext::UserFunction {
                name,
                params,
                return_type,
                doc,
            } => Self::format_user_function(name, params, return_type.as_deref(), doc.as_deref()),

            HoverContext::UserEnum { name, variants } => Self::format_user_enum(name, variants),

            HoverContext::ApiType(name) => self.format_api_type(name),

            HoverContext::ApiFunction(name) => self.format_api_function(name),

            HoverContext::ApiMethod {
                type_name,
                method_name,
            } => self.format_api_method(type_name, method_name),

            HoverContext::ApiEnumVariant {
                enum_name,
                variant_name,
            } => self.format_api_enum_variant(enum_name, variant_name),

            HoverContext::ApiField {
                type_name,
                field_name,
            } => self.format_api_field(type_name, field_name),

            HoverContext::UserField {
                struct_name,
                field_name,
                field_type,
            } => format!("**{struct_name}.{field_name}**: `{field_type}`"),

            HoverContext::Callback {
                struct_name,
                callback_name,
                extends_type,
            } => self.format_callback(struct_name, callback_name, extends_type),

            HoverContext::LocalVariable { name, type_name } => {
                format!("**{name}**: `{type_name}`")
            }
        }
    }

    fn format_user_struct(
        &self,
        name: &str,
        extends: Option<&str>,
        fields: &[(String, String)],
    ) -> String {
        let mut result = String::new();

        // Header
        let _ = write!(result, "**struct {name}**");
        if let Some(ext) = extends {
            let _ = write!(result, " extends **{ext}**");
        }

        // Fields
        if !fields.is_empty() {
            result.push_str("\n\n**Fields:**\n");
            for (field_name, field_type) in fields {
                let _ = writeln!(result, "- `{field_name}`: `{field_type}`");
            }
        }

        // Available callbacks (if extends a game type)
        if let Some(ext) = extends {
            let callbacks = self.api.callbacks_for_type(ext);
            if !callbacks.is_empty() {
                result.push_str("\n**Available callbacks:** ");
                let names: Vec<_> = callbacks.iter().map(|c| c.name.as_str()).collect();
                result.push_str(&names.join(", "));
            }
        }

        result
    }

    fn format_user_function(
        name: &str,
        params: &[(String, String)],
        return_type: Option<&str>,
        doc: Option<&str>,
    ) -> String {
        let mut result = String::new();

        // Signature
        let params_str = params
            .iter()
            .map(|(n, t)| format!("{n}: {t}"))
            .collect::<Vec<_>>()
            .join(", ");

        let _ = write!(result, "**fn {name}**");
        result.push_str("\n```nimbyscript\n");
        let _ = write!(result, "fn {name}({params_str})");
        if let Some(ret) = return_type {
            let _ = write!(result, " -> {ret}");
        }
        result.push_str("\n```");

        if let Some(d) = doc {
            let _ = write!(result, "\n\n{d}");
        }

        result
    }

    fn format_user_enum(name: &str, variants: &[String]) -> String {
        let mut result = format!("**enum {name}**\n\n**Variants:**\n");
        for variant in variants {
            let _ = writeln!(result, "- `{variant}`");
        }
        result
    }

    fn format_api_type(&self, name: &str) -> String {
        let mut result = String::new();

        // Check if it's a type
        if let Some(type_def) = self.api.get_type(name) {
            let _ = write!(result, "**{name}** (game type)");

            if let Some(doc) = &type_def.doc {
                let _ = write!(result, "\n\n{doc}");
            }

            // Fields
            if !type_def.fields.is_empty() {
                result.push_str("\n\n**Fields:** ");
                let field_names: Vec<_> = type_def.fields.keys().collect();
                result.push_str(
                    &field_names
                        .iter()
                        .map(|s| s.as_str())
                        .collect::<Vec<_>>()
                        .join(", "),
                );
            }

            // Methods
            if !type_def.methods.is_empty() {
                result.push_str("\n\n**Methods:** ");
                let method_names: Vec<_> =
                    type_def.methods.iter().map(|m| m.name.as_str()).collect();
                result.push_str(&method_names.join(", "));
            }

            return result;
        }

        // Check if it's an enum
        if let Some(enum_def) = self.api.get_enum(name) {
            let _ = write!(result, "**{name}** (enum)");

            if let Some(doc) = &enum_def.doc {
                let _ = write!(result, "\n\n{doc}");
            }

            result.push_str("\n\n**Variants:** ");
            let variant_names: Vec<_> = enum_def.variants.iter().map(|v| v.name.as_str()).collect();
            result.push_str(&variant_names.join(", "));

            return result;
        }

        format!("**{name}**")
    }

    fn format_api_function(&self, name: &str) -> String {
        let Some(func) = self.api.get_function(name) else {
            return format!("**{name}**");
        };

        let mut result = String::new();
        let _ = write!(result, "**fn {name}**{}", format_signature(func));

        if let Some(doc) = &func.doc {
            let _ = write!(result, "\n\n{doc}");
        }

        Self::append_params_doc(&mut result, func);

        result
    }

    fn format_api_method(&self, type_name: &str, method_name: &str) -> String {
        let Some(type_def) = self.api.get_type(type_name) else {
            return format!("**{type_name}.{method_name}**");
        };

        let Some(method) = type_def.methods.iter().find(|m| m.name == method_name) else {
            return format!("**{type_name}.{method_name}**");
        };

        let mut result = String::new();
        let _ = write!(
            result,
            "**{type_name}.{method_name}**{}",
            format_signature(method)
        );

        if let Some(doc) = &method.doc {
            let _ = write!(result, "\n\n{doc}");
        }

        Self::append_params_doc(&mut result, method);

        result
    }

    fn format_api_enum_variant(&self, enum_name: &str, variant_name: &str) -> String {
        let mut result = format!("**{enum_name}::{variant_name}**");

        if let Some(enum_def) = self.api.get_enum(enum_name) {
            if let Some(variant) = enum_def.variants.iter().find(|v| v.name == variant_name) {
                if let Some(doc) = &variant.doc {
                    let _ = write!(result, "\n\n{doc}");
                }
            }
        }

        result
    }

    fn format_api_field(&self, type_name: &str, field_name: &str) -> String {
        let Some(type_def) = self.api.get_type(type_name) else {
            return format!("**{type_name}.{field_name}**");
        };

        let Some(field) = type_def.fields.get(field_name) else {
            return format!("**{type_name}.{field_name}**");
        };

        let mut result = format!("**{type_name}.{field_name}**: `{}`", field.ty);

        if field.readonly {
            result.push_str(" (readonly)");
        }

        if let Some(doc) = &field.doc {
            let _ = write!(result, "\n\n{doc}");
        }

        result
    }

    fn format_callback(
        &self,
        struct_name: &str,
        callback_name: &str,
        extends_type: &str,
    ) -> String {
        let mut result = String::new();

        let _ = write!(
            result,
            "**fn {struct_name}::{callback_name}** (callback for {extends_type})"
        );

        if let Some(callback) = self.api.get_callback(callback_name) {
            // Format signature with struct_name instead of Self
            result.push_str("\n```nimbyscript\n");
            let _ = write!(result, "fn {struct_name}::{callback_name}");
            let _ = write!(
                result,
                "{}",
                format_callback_signature(callback, struct_name)
            );
            result.push_str("\n```");

            if let Some(doc) = &callback.doc {
                let _ = write!(result, "\n\n{doc}");
            }

            // Parameters with struct name substituted for &Self
            Self::append_callback_params(&mut result, callback, struct_name);

            if let Some(ret) = &callback.return_type {
                let _ = write!(result, "\n**Returns:** `{ret}`");
            }
        }

        result
    }

    /// Append callback parameters with &Self replaced by the struct name.
    fn append_callback_params(result: &mut String, callback: &FunctionDef, struct_name: &str) {
        if callback.params.is_empty() {
            return;
        }
        result.push_str("\n\n**Parameters:**\n");
        for p in &callback.params {
            let ty = if p.ty == "&Self" {
                format!("&{struct_name}")
            } else {
                p.ty.clone()
            };
            let _ = write!(result, "- `{}`: `{ty}`", p.name);
            if let Some(param_doc) = &p.doc {
                let _ = write!(result, " - {param_doc}");
            }
            result.push('\n');
        }
    }

    fn append_params_doc(result: &mut String, func: &FunctionDef) {
        if !func.params.is_empty() {
            result.push_str("\n\n**Parameters:**\n");
            for p in &func.params {
                let _ = write!(result, "- `{}`: `{}`", p.name, p.ty);
                if let Some(param_doc) = &p.doc {
                    let _ = write!(result, " - {param_doc}");
                }
                result.push('\n');
            }
        }

        if let Some(ret) = &func.return_type {
            let _ = write!(result, "\n**Returns:** `{ret}`");
        }
    }
}

// ============================================================================
// Helper Functions
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

    #[test]
    fn test_hover_on_type_annotation() {
        let api = make_api();

        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal {
    owner: ID<Train>,
}
";
        let doc = Document::new(code.to_string(), Some(&api));

        // Find position of "ID" in "owner: ID<Train>"
        let id_offset = code
            .find("ID<Train>")
            .expect("ID<Train> should exist in code");
        let id_pos = doc.offset_to_position(id_offset);

        let hover = get_hover(&doc, id_pos, &api);
        assert!(hover.is_some(), "Should have hover info for 'ID'");
        let hover_content = match hover.expect("hover should exist").contents {
            HoverContents::Markup(m) => m.value,
            _ => panic!("Expected markup content"),
        };
        assert!(
            hover_content.contains("ID<Train>"),
            "Hover should mention ID<Train>"
        );
    }

    #[test]
    fn test_hover_on_api_field_method() {
        let api = make_api();

        // Verify the API has the types we need
        assert!(api.get_type("Motion").is_some(), "Motion type should exist");
        assert!(
            api.get_type("std::optional").is_some(),
            "std::optional type should exist"
        );

        // Code where we want to hover on `presence` and `get`
        let code = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Train {}
pub fn Test::control_train(
    self: &Test,
    ctx: &ControlCtx,
    train: &Train,
    motion: &Motion,
    sc: &mut SimController
) {
    if is_null(motion.presence.get()) {
        return;
    }
}
";
        let doc = Document::new(code.to_string(), Some(&api));

        // Find position of "presence" in motion.presence.get()
        let base_offset = code
            .find("motion.presence")
            .expect("motion.presence should exist in code");
        let presence_offset = base_offset + "motion.".len();
        let presence_pos = doc.offset_to_position(presence_offset);

        let hover = get_hover(&doc, presence_pos, &api);
        assert!(hover.is_some(), "Should have hover info for 'presence'");
        let hover_content = match hover.expect("hover should exist").contents {
            HoverContents::Markup(m) => m.value,
            _ => panic!("Expected markup content"),
        };
        assert!(
            hover_content.contains("Motion.presence"),
            "Hover should mention Motion.presence"
        );
        assert!(
            hover_content.contains("std::optional"),
            "Hover should mention the type"
        );

        // Find position of "get" in motion.presence.get()
        let get_offset = code
            .find("motion.presence.get")
            .expect("motion.presence.get should exist in code")
            + "motion.presence.".len();
        let get_pos = doc.offset_to_position(get_offset);

        let hover = get_hover(&doc, get_pos, &api);
        assert!(hover.is_some(), "Should have hover info for 'get'");
        let hover_content = match hover.expect("hover should exist").contents {
            HoverContents::Markup(m) => m.value,
            _ => panic!("Expected markup content"),
        };
        assert!(
            hover_content.contains("std::optional.get"),
            "Hover should mention std::optional.get"
        );
    }
}
