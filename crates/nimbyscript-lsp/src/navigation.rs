//! Navigation providers for NimbyScript LSP.
//!
//! Provides go-to-definition, type definition, implementations, and references.

use std::collections::HashMap;

use tower_lsp::lsp_types::request::{GotoImplementationResponse, GotoTypeDefinitionResponse};
use tower_lsp::lsp_types::*;

use nimbyscript_analyzer::{
    collect_declarations, types::parse_type_string, ApiDefinitions, SemanticContext, TypeInfo,
};
use nimbyscript_parser::{kind, Node, NodeExt};

use crate::document::Document;

// ============================================================================
// Public API
// ============================================================================

/// Get the definition location for a symbol at the given position.
pub fn get_definition(
    doc: &Document,
    position: Position,
    uri: &Url,
    api: &ApiDefinitions,
) -> Option<GotoDefinitionResponse> {
    let engine = NavigationEngine::new(doc, api, position);
    engine.find_definition(uri)
}

/// Get the type definition location for a symbol at the given position.
pub fn get_type_definition(
    doc: &Document,
    position: Position,
    uri: &Url,
    api: &ApiDefinitions,
) -> Option<GotoTypeDefinitionResponse> {
    let engine = NavigationEngine::new(doc, api, position);
    engine.find_type_definition(uri)
}

/// Get implementation locations for a type at the given position.
pub fn get_implementations(
    doc: &Document,
    position: Position,
    uri: &Url,
    api: &ApiDefinitions,
) -> Option<GotoImplementationResponse> {
    let engine = NavigationEngine::new(doc, api, position);
    engine.find_implementations(uri)
}

/// Find all references to a symbol at the given position.
pub fn find_references(
    doc: &Document,
    position: Position,
    uri: &Url,
    api: &ApiDefinitions,
    include_declaration: bool,
) -> Option<Vec<Location>> {
    let engine = NavigationEngine::new(doc, api, position);
    engine.find_references(uri, include_declaration)
}

// ============================================================================
// Navigation Engine
// ============================================================================

/// Engine for computing navigation information.
struct NavigationEngine<'a> {
    doc: &'a Document,
    content: &'a str,
    api: &'a ApiDefinitions,
    offset: usize,
    /// Local variable types (collected from enclosing function)
    local_types: HashMap<String, TypeInfo>,
    /// User-defined struct fields (struct_name -> (field_name -> field_type))
    struct_fields: HashMap<String, HashMap<String, TypeInfo>>,
    /// User-defined structs (name -> extends type)
    user_structs: HashMap<String, Option<String>>,
    /// User-defined enums (name -> variants)
    user_enums: HashMap<String, Vec<String>>,
    /// User-defined functions (name -> return type)
    user_functions: HashMap<String, Option<TypeInfo>>,
}

impl<'a> NavigationEngine<'a> {
    fn new(doc: &'a Document, api: &'a ApiDefinitions, position: Position) -> Self {
        let offset = doc.position_to_offset(position);

        // Build semantic context to collect declarations
        let mut ctx = SemanticContext::new(&doc.content, doc.tree(), api);
        collect_declarations(&mut ctx);

        let struct_fields = ctx.struct_fields.clone();
        let user_structs = ctx.user_structs.clone();
        let user_enums = ctx.user_enums.clone();
        let user_functions = ctx.user_functions.clone();

        let mut engine = Self {
            doc,
            content: &doc.content,
            api,
            offset,
            local_types: HashMap::new(),
            struct_fields,
            user_structs,
            user_enums,
            user_functions,
        };

        engine.collect_local_types();
        engine
    }

    // ========================================================================
    // Definition Provider
    // ========================================================================

    fn find_definition(&self, uri: &Url) -> Option<GotoDefinitionResponse> {
        let root = self.doc.tree().root_node();
        let node = self.find_deepest_node_at(root)?;

        // Try various detection strategies
        self.definition_for_struct_reference(node, uri)
            .or_else(|| self.definition_for_enum_reference(node, uri))
            .or_else(|| self.definition_for_function_call(node, uri))
            .or_else(|| self.definition_for_field_access(node, uri))
            .or_else(|| self.definition_for_variable(node, uri))
            .or_else(|| self.definition_for_type_annotation(node, uri))
    }

    /// Find definition when on a struct name reference.
    fn definition_for_struct_reference(
        &self,
        node: Node,
        uri: &Url,
    ) -> Option<GotoDefinitionResponse> {
        if node.kind() != kind::IDENTIFIER {
            return None;
        }
        let name = node.text(self.content);

        // Check if it's a user-defined struct
        if self.user_structs.contains_key(name) {
            let location = self.find_struct_definition_location(name, uri)?;
            return Some(GotoDefinitionResponse::Scalar(location));
        }

        None
    }

    /// Find definition when on an enum name reference.
    fn definition_for_enum_reference(
        &self,
        node: Node,
        uri: &Url,
    ) -> Option<GotoDefinitionResponse> {
        if node.kind() != kind::IDENTIFIER {
            return None;
        }
        let name = node.text(self.content);

        // Check if it's a user-defined enum
        if self.user_enums.contains_key(name) {
            let location = self.find_enum_definition_location(name, uri)?;
            return Some(GotoDefinitionResponse::Scalar(location));
        }

        None
    }

    /// Find definition when on a function call.
    fn definition_for_function_call(
        &self,
        node: Node,
        uri: &Url,
    ) -> Option<GotoDefinitionResponse> {
        // Check if we're in a call expression
        let call_node = Self::find_ancestor_of_kind(node, kind::CALL_EXPRESSION)?;
        let func_node = call_node.child_by_field("function")?;

        // Get function name - handle both simple calls and path expressions
        // Skip method calls (field access) and other non-function expressions
        if func_node.kind() != kind::PATH_EXPRESSION && func_node.kind() != kind::IDENTIFIER {
            return None;
        }
        let func_name = func_node.text(self.content);

        // Only trigger if cursor is on the function name
        if !self.cursor_in_node(func_node) {
            return None;
        }

        // Check if it's a user-defined function
        if self.user_functions.contains_key(func_name) {
            let location = self.find_function_definition_location(func_name, uri)?;
            return Some(GotoDefinitionResponse::Scalar(location));
        }

        None
    }

    /// Find definition when on a field access.
    fn definition_for_field_access(&self, node: Node, uri: &Url) -> Option<GotoDefinitionResponse> {
        let field_node = Self::find_ancestor_of_kind(node, kind::FIELD_ACCESS)?;
        let field_name_node = field_node.child_by_field("field")?;

        // Only trigger if cursor is on the field name
        if !self.cursor_in_node(field_name_node) {
            return None;
        }

        let field_name = field_name_node.text(self.content);
        let object_node = field_node.child_by_field("object")?;
        let object_type = self.infer_node_type(object_node)?;
        let type_name = Self::unwrap_to_type_name(&object_type)?;

        // Check if it's a user struct field
        if self.struct_fields.contains_key(&type_name) {
            let location = self.find_field_definition_location(&type_name, field_name, uri)?;
            return Some(GotoDefinitionResponse::Scalar(location));
        }

        None
    }

    /// Find definition when on a variable reference.
    fn definition_for_variable(&self, node: Node, uri: &Url) -> Option<GotoDefinitionResponse> {
        if node.kind() != kind::IDENTIFIER {
            return None;
        }
        let name = node.text(self.content);

        // Check if it's a local variable
        if self.local_types.contains_key(name) {
            let location = self.find_variable_definition_location(name, uri)?;
            return Some(GotoDefinitionResponse::Scalar(location));
        }

        None
    }

    /// Find definition when on a type annotation.
    fn definition_for_type_annotation(
        &self,
        node: Node,
        uri: &Url,
    ) -> Option<GotoDefinitionResponse> {
        // Check if we're in a type_identifier context
        let type_id_node = Self::find_ancestor_of_kind(node, kind::TYPE_IDENTIFIER)?;

        // Get the base type name
        let base_type_name = if node.kind() == kind::IDENTIFIER {
            node.text(self.content)
        } else {
            let mut cursor = type_id_node.walk();
            let id_node = type_id_node
                .children(&mut cursor)
                .find(|c| c.kind() == kind::IDENTIFIER)?;
            id_node.text(self.content)
        };

        // Check user-defined struct
        if self.user_structs.contains_key(base_type_name) {
            let location = self.find_struct_definition_location(base_type_name, uri)?;
            return Some(GotoDefinitionResponse::Scalar(location));
        }

        // Check user-defined enum
        if self.user_enums.contains_key(base_type_name) {
            let location = self.find_enum_definition_location(base_type_name, uri)?;
            return Some(GotoDefinitionResponse::Scalar(location));
        }

        None
    }

    // ========================================================================
    // Type Definition Provider
    // ========================================================================

    fn find_type_definition(&self, uri: &Url) -> Option<GotoTypeDefinitionResponse> {
        let root = self.doc.tree().root_node();
        let node = self.find_deepest_node_at(root)?;

        // Get the type of the symbol at cursor
        let type_info = self.infer_type_at_cursor(node)?;
        let type_name = Self::unwrap_to_type_name(&type_info)?;
        let base_type_name = Self::base_type_name(&type_name);

        // Check if it's a user-defined struct
        if self.user_structs.contains_key(&base_type_name) {
            let location = self.find_struct_definition_location(&base_type_name, uri)?;
            return Some(GotoTypeDefinitionResponse::Scalar(location));
        }

        // Check if it's a user-defined enum
        if self.user_enums.contains_key(&base_type_name) {
            let location = self.find_enum_definition_location(&base_type_name, uri)?;
            return Some(GotoTypeDefinitionResponse::Scalar(location));
        }

        // API types don't have source locations
        None
    }

    fn infer_type_at_cursor(&self, node: Node) -> Option<TypeInfo> {
        match node.kind() {
            kind::IDENTIFIER => {
                let name = node.text(self.content);
                self.local_types.get(name).cloned()
            }
            kind::FIELD_ACCESS => self.infer_node_type(node),
            kind::CALL_EXPRESSION => self.infer_call_type(node),
            _ => {
                // Try walking up to find a meaningful node
                if let Some(parent) = node.parent() {
                    match parent.kind() {
                        kind::FIELD_ACCESS | kind::CALL_EXPRESSION => self.infer_node_type(parent),
                        _ => None,
                    }
                } else {
                    None
                }
            }
        }
    }

    // ========================================================================
    // Implementation Provider
    // ========================================================================

    fn find_implementations(&self, uri: &Url) -> Option<GotoImplementationResponse> {
        let root = self.doc.tree().root_node();
        let node = self.find_deepest_node_at(root)?;

        // Get the type name at cursor
        let type_name = self.get_type_name_at_cursor(node)?;

        // Find all user structs that extend this type
        let locations: Vec<_> = self
            .user_structs
            .iter()
            .filter_map(|(struct_name, extends)| {
                let ext = extends.as_ref()?;
                if ext == &type_name {
                    self.find_struct_definition_location(struct_name, uri)
                } else {
                    None
                }
            })
            .collect();

        if locations.is_empty() {
            None
        } else {
            Some(GotoImplementationResponse::Array(locations))
        }
    }

    fn get_type_name_at_cursor(&self, node: Node) -> Option<String> {
        if node.kind() == kind::IDENTIFIER {
            let name = node.text(self.content);
            // Check if it's a struct or enum or API type
            if self.user_structs.contains_key(name)
                || self.user_enums.contains_key(name)
                || self.api.get_type(name).is_some()
            {
                return Some(name.to_string());
            }
        }

        // Check if we're in a type_identifier
        if let Some(type_id_node) = Self::find_ancestor_of_kind(node, kind::TYPE_IDENTIFIER) {
            let mut cursor = type_id_node.walk();
            let id_node = type_id_node
                .children(&mut cursor)
                .find(|c| c.kind() == kind::IDENTIFIER);
            if let Some(id_node) = id_node {
                return Some(id_node.text(self.content).to_string());
            }
        }

        None
    }

    // ========================================================================
    // References Provider
    // ========================================================================

    fn find_references(&self, uri: &Url, include_declaration: bool) -> Option<Vec<Location>> {
        let root = self.doc.tree().root_node();
        let node = self.find_deepest_node_at(root)?;

        // Get the name of the symbol at cursor
        let name = self.get_symbol_name_at_cursor(node)?;

        // Determine if this is a global or local symbol
        let is_global = self.user_structs.contains_key(&name)
            || self.user_enums.contains_key(&name)
            || self.user_functions.contains_key(&name);

        // Find all references in the AST
        let mut references = Vec::new();
        let definition_location = if is_global {
            // For global symbols, find all identifier uses with this name
            self.collect_identifier_references(root, &name, uri, &mut references);

            // Get definition location for filtering
            self.find_struct_definition_location(&name, uri)
                .or_else(|| self.find_enum_definition_location(&name, uri))
                .or_else(|| self.find_function_definition_location(&name, uri))
        } else {
            // For local variables, need to find references within the same function scope
            if let Some(func_node) = self.find_enclosing_function(root) {
                self.collect_identifier_references(func_node, &name, uri, &mut references);
            }

            // Get definition location
            self.find_variable_definition_location(&name, uri)
        };

        // Filter out the declaration if not requested
        if !include_declaration {
            if let Some(def_loc) = &definition_location {
                references.retain(|loc| {
                    loc.range.start != def_loc.range.start || loc.range.end != def_loc.range.end
                });
            }
        }

        if references.is_empty() {
            None
        } else {
            Some(references)
        }
    }

    fn get_symbol_name_at_cursor(&self, node: Node) -> Option<String> {
        if node.kind() == kind::IDENTIFIER {
            return Some(node.text(self.content).to_string());
        }

        // Check if we're on a struct/enum/function definition name
        if let Some(struct_node) = Self::find_ancestor_of_kind(node, kind::STRUCT_DEFINITION) {
            if let Some(name_node) = struct_node.child_by_field("name") {
                if self.cursor_in_node(name_node) {
                    return Some(name_node.text(self.content).to_string());
                }
            }
        }

        if let Some(enum_node) = Self::find_ancestor_of_kind(node, kind::ENUM_DEFINITION) {
            if let Some(name_node) = enum_node.child_by_field("name") {
                if self.cursor_in_node(name_node) {
                    return Some(name_node.text(self.content).to_string());
                }
            }
        }

        if let Some(func_node) = Self::find_ancestor_of_kind(node, kind::FUNCTION_DEFINITION) {
            if let Some(name_node) = func_node.child_by_field("name") {
                if self.cursor_in_node(name_node) {
                    return Some(name_node.text(self.content).to_string());
                }
            }
        }

        None
    }

    fn collect_identifier_references(
        &self,
        node: Node,
        name: &str,
        uri: &Url,
        references: &mut Vec<Location>,
    ) {
        if node.kind() == kind::IDENTIFIER && node.text(self.content) == name {
            references.push(Location {
                uri: uri.clone(),
                range: self.node_to_range(node),
            });
        }

        // Also check for function definition names (which include :: for methods)
        if node.kind() == kind::FUNCTION_NAME {
            let func_name = node.text(self.content);
            // Check if the name matches (either exact match or as method)
            if func_name == name || func_name.ends_with(&format!("::{name}")) {
                references.push(Location {
                    uri: uri.clone(),
                    range: self.node_to_range(node),
                });
            }
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.collect_identifier_references(child, name, uri, references);
        }
    }

    // ========================================================================
    // Definition Location Finders
    // ========================================================================

    fn find_struct_definition_location(&self, name: &str, uri: &Url) -> Option<Location> {
        let root = self.doc.tree().root_node();
        self.find_struct_in_node(root, name, uri)
    }

    fn find_struct_in_node(&self, node: Node, name: &str, uri: &Url) -> Option<Location> {
        if node.kind() == kind::STRUCT_DEFINITION {
            if let Some(name_node) = node.child_by_field("name") {
                if name_node.text(self.content) == name {
                    return Some(Location {
                        uri: uri.clone(),
                        range: self.node_to_range(name_node),
                    });
                }
            }
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(loc) = self.find_struct_in_node(child, name, uri) {
                return Some(loc);
            }
        }

        None
    }

    fn find_enum_definition_location(&self, name: &str, uri: &Url) -> Option<Location> {
        let root = self.doc.tree().root_node();
        self.find_enum_in_node(root, name, uri)
    }

    fn find_enum_in_node(&self, node: Node, name: &str, uri: &Url) -> Option<Location> {
        if node.kind() == kind::ENUM_DEFINITION {
            if let Some(name_node) = node.child_by_field("name") {
                if name_node.text(self.content) == name {
                    return Some(Location {
                        uri: uri.clone(),
                        range: self.node_to_range(name_node),
                    });
                }
            }
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(loc) = self.find_enum_in_node(child, name, uri) {
                return Some(loc);
            }
        }

        None
    }

    fn find_function_definition_location(&self, name: &str, uri: &Url) -> Option<Location> {
        let root = self.doc.tree().root_node();
        self.find_function_in_node(root, name, uri)
    }

    fn find_function_in_node(&self, node: Node, name: &str, uri: &Url) -> Option<Location> {
        if node.kind() == kind::FUNCTION_DEFINITION {
            if let Some(name_node) = node.child_by_field("name") {
                if name_node.text(self.content) == name {
                    return Some(Location {
                        uri: uri.clone(),
                        range: self.node_to_range(name_node),
                    });
                }
            }
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(loc) = self.find_function_in_node(child, name, uri) {
                return Some(loc);
            }
        }

        None
    }

    fn find_field_definition_location(
        &self,
        struct_name: &str,
        field_name: &str,
        uri: &Url,
    ) -> Option<Location> {
        let root = self.doc.tree().root_node();
        self.find_field_in_struct(root, struct_name, field_name, uri)
    }

    fn find_field_in_struct(
        &self,
        node: Node,
        struct_name: &str,
        field_name: &str,
        uri: &Url,
    ) -> Option<Location> {
        if node.kind() == kind::STRUCT_DEFINITION {
            if let Some(loc) = self.search_struct_for_field(node, struct_name, field_name, uri) {
                return Some(loc);
            }
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(loc) = self.find_field_in_struct(child, struct_name, field_name, uri) {
                return Some(loc);
            }
        }

        None
    }

    fn search_struct_for_field(
        &self,
        struct_node: Node,
        struct_name: &str,
        field_name: &str,
        uri: &Url,
    ) -> Option<Location> {
        let name_node = struct_node.child_by_field("name")?;
        if name_node.text(self.content) != struct_name {
            return None;
        }

        let mut cursor = struct_node.walk();
        for child in struct_node.children(&mut cursor) {
            if child.kind() != kind::STRUCT_FIELD {
                continue;
            }
            let field_name_node = child.child_by_field("name")?;
            if field_name_node.text(self.content) == field_name {
                return Some(Location {
                    uri: uri.clone(),
                    range: self.node_to_range(field_name_node),
                });
            }
        }
        None
    }

    fn find_variable_definition_location(&self, name: &str, uri: &Url) -> Option<Location> {
        let root = self.doc.tree().root_node();

        // Find the enclosing function
        let func_node = self.find_enclosing_function(root)?;

        // Check parameters first
        if let Some(params_node) = func_node.child_by_kind(kind::PARAMETERS) {
            if let Some(loc) = self.find_param_with_name(params_node, name, uri) {
                return Some(loc);
            }
        }

        // Check let bindings in the function body
        if let Some(body) = func_node.child_by_kind(kind::BLOCK) {
            if let Some(loc) = self.find_let_binding_with_name(body, name, uri) {
                return Some(loc);
            }
        }

        None
    }

    fn find_param_with_name(&self, params_node: Node, name: &str, uri: &Url) -> Option<Location> {
        let mut cursor = params_node.walk();
        for param in params_node.children(&mut cursor) {
            if param.kind() != kind::PARAMETER {
                continue;
            }
            let name_node = param.child_by_field("name")?;
            if name_node.text(self.content) == name {
                return Some(Location {
                    uri: uri.clone(),
                    range: self.node_to_range(name_node),
                });
            }
        }
        None
    }

    fn find_let_binding_with_name(&self, node: Node, name: &str, uri: &Url) -> Option<Location> {
        // Check let statements - they have a binding child that contains the name
        if node.kind() == kind::LET_STATEMENT || node.kind() == kind::LET_ELSE_STATEMENT {
            if let Some(loc) = self.find_binding_name_location(node, name, uri) {
                return Some(loc);
            }
        }

        // Check for-loop bindings
        if node.kind() == kind::FOR_STATEMENT {
            if let Some(name_node) = node.child_by_field("variable") {
                if name_node.text(self.content) == name {
                    return Some(Location {
                        uri: uri.clone(),
                        range: self.node_to_range(name_node),
                    });
                }
            }
        }

        // Check if-let bindings
        if node.kind() == kind::IF_LET_STATEMENT {
            if let Some(name_node) = node.child_by_field("name") {
                if name_node.text(self.content) == name {
                    return Some(Location {
                        uri: uri.clone(),
                        range: self.node_to_range(name_node),
                    });
                }
            }
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            // Only search bindings that come before the cursor position
            if child.start_byte() <= self.offset {
                if let Some(loc) = self.find_let_binding_with_name(child, name, uri) {
                    return Some(loc);
                }
            }
        }

        None
    }

    fn find_binding_name_location(&self, node: Node, name: &str, uri: &Url) -> Option<Location> {
        let binding = node.child_by_kind(kind::BINDING)?;
        let name_node = binding.child_by_field("name")?;
        if name_node.text(self.content) == name {
            return Some(Location {
                uri: uri.clone(),
                range: self.node_to_range(name_node),
            });
        }
        None
    }

    fn find_enclosing_function<'b>(&self, node: Node<'b>) -> Option<Node<'b>> {
        if node.kind() == kind::FUNCTION_DEFINITION {
            let start = node.start_byte();
            let end = node.end_byte();
            if start <= self.offset && self.offset <= end {
                return Some(node);
            }
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(func) = self.find_enclosing_function(child) {
                return Some(func);
            }
        }

        None
    }

    // ========================================================================
    // Type Inference
    // ========================================================================

    fn infer_node_type(&self, node: Node) -> Option<TypeInfo> {
        match node.kind() {
            kind::IDENTIFIER => {
                let name = node.text(self.content);
                self.local_types.get(name).cloned()
            }
            kind::PATH_EXPRESSION => {
                let text = node.text(self.content);
                if text.contains("::") {
                    None
                } else {
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
            kind::CALL_EXPRESSION => self.infer_call_type(node),
            _ => None,
        }
    }

    fn infer_call_type(&self, node: Node) -> Option<TypeInfo> {
        let callee = node.child_by_field("function")?;

        if callee.kind() != kind::FIELD_ACCESS {
            // Check for user-defined function call
            let func_name = callee.text(self.content);
            if let Some(return_type) = self.user_functions.get(func_name) {
                return return_type.clone();
            }
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

    // ========================================================================
    // Helpers
    // ========================================================================

    fn find_deepest_node_at<'b>(&self, node: Node<'b>) -> Option<Node<'b>> {
        let start = node.start_byte();
        let end = node.end_byte();

        if self.offset < start || self.offset >= end {
            return None;
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(deeper) = self.find_deepest_node_at(child) {
                return Some(deeper);
            }
        }

        Some(node)
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

    fn cursor_in_node(&self, node: Node) -> bool {
        self.offset >= node.start_byte() && self.offset <= node.end_byte()
    }

    fn node_to_range(&self, node: Node) -> Range {
        let start = self.doc.offset_to_position(node.start_byte());
        let end = self.doc.offset_to_position(node.end_byte());
        Range::new(start, end)
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

    fn base_type_name(type_name: &str) -> String {
        if let Some(idx) = type_name.find('<') {
            type_name[..idx].to_string()
        } else {
            type_name.to_string()
        }
    }

    fn collect_local_types(&mut self) {
        let root = self.doc.tree().root_node();
        let Some(func_node) = Self::find_enclosing_function_static(root, self.offset) else {
            return;
        };

        // Extract parameters
        if let Some(params_node) = func_node.child_by_kind(kind::PARAMETERS) {
            self.collect_params_from_node(params_node);
        }

        // Extract let bindings (only those before cursor)
        if let Some(body) = func_node.child_by_kind(kind::BLOCK) {
            self.collect_bindings_from_block(body);
        }
    }

    fn collect_params_from_node(&mut self, params_node: Node) {
        let mut cursor = params_node.walk();
        for param in params_node.children(&mut cursor) {
            if param.kind() != kind::PARAMETER {
                continue;
            }
            let Some(name_node) = param.child_by_field("name") else {
                continue;
            };
            let Some(type_node) = param.child_by_field("type") else {
                continue;
            };
            let name = name_node.text(self.content).to_string();
            let type_info = parse_type_string(type_node.text(self.content));
            self.local_types.insert(name, type_info);
        }
    }

    fn find_enclosing_function_static(node: Node<'_>, offset: usize) -> Option<Node<'_>> {
        if node.kind() == kind::FUNCTION_DEFINITION {
            let start = node.start_byte();
            let end = node.end_byte();
            if start <= offset && offset <= end {
                return Some(node);
            }
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(func) = Self::find_enclosing_function_static(child, offset) {
                return Some(func);
            }
        }

        None
    }

    fn collect_binding_from_let(&mut self, node: Node) {
        let Some(binding) = node.child_by_kind(kind::BINDING) else {
            return;
        };
        let Some(name_node) = binding.child_by_field("name") else {
            return;
        };
        let name = name_node.text(self.content).to_string();
        let type_info = binding
            .child_by_field("type")
            .map_or(TypeInfo::Unknown, |t| parse_type_string(t.text(self.content)));
        self.local_types.insert(name, type_info);
    }

    fn collect_bindings_from_block(&mut self, node: Node) {
        // Only process nodes before the cursor
        if node.start_byte() > self.offset {
            return;
        }

        if node.kind() == kind::LET_STATEMENT || node.kind() == kind::LET_ELSE_STATEMENT {
            self.collect_binding_from_let(node);
        }

        if node.kind() == kind::FOR_STATEMENT {
            if let Some(var_node) = node.child_by_field("variable") {
                let name = var_node.text(self.content).to_string();
                // For-loop variable type is typically the element type of the iterator
                self.local_types.insert(name, TypeInfo::Unknown);
            }
        }

        if node.kind() == kind::IF_LET_STATEMENT {
            if let Some(name_node) = node.child_by_field("name") {
                let name = name_node.text(self.content).to_string();
                self.local_types.insert(name, TypeInfo::Unknown);
            }
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.collect_bindings_from_block(child);
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    fn make_doc(content: &str) -> Document {
        Document::new(content.to_string(), None)
    }

    fn pos(line: u32, character: u32) -> Position {
        Position { line, character }
    }

    #[test]
    fn test_definition_struct() {
        let content = r"
struct MyStruct {
    field1: i64,
}

fn test(): i64 {
    let x: MyStruct = todo();
    return 0;
}
";
        let doc = make_doc(content);
        let uri = Url::parse("file:///test.nimbyscript").expect("valid URL");
        let api = ApiDefinitions::default();

        // Position on "MyStruct" in the type annotation (line 6, after "let x: ")
        let result = get_definition(&doc, pos(6, 12), &uri, &api);
        assert!(result.is_some(), "Should find definition for struct usage");

        if let Some(GotoDefinitionResponse::Scalar(loc)) = result {
            // Should point to line 1 (struct definition)
            assert_eq!(loc.range.start.line, 1);
        }
    }

    #[test]
    fn test_definition_enum() {
        let content = r"
enum MyEnum {
    Variant1,
    Variant2,
}

fn test(): i64 {
    let x: MyEnum = todo();
    return 0;
}
";
        let doc = make_doc(content);
        let uri = Url::parse("file:///test.nimbyscript").expect("valid URL");
        let api = ApiDefinitions::default();

        // Position on "MyEnum" in the type annotation
        let result = get_definition(&doc, pos(7, 12), &uri, &api);
        assert!(result.is_some(), "Should find definition for enum usage");

        if let Some(GotoDefinitionResponse::Scalar(loc)) = result {
            assert_eq!(loc.range.start.line, 1);
        }
    }

    #[test]
    fn test_definition_function() {
        let content = r"
fn my_function(): i64 {
    return 42;
}

fn test(): i64 {
    return my_function();
}
";
        let doc = make_doc(content);
        let uri = Url::parse("file:///test.nimbyscript").expect("valid URL");
        let api = ApiDefinitions::default();

        // Position on "my_function" in the call
        let result = get_definition(&doc, pos(6, 12), &uri, &api);
        assert!(result.is_some(), "Should find definition for function call");

        if let Some(GotoDefinitionResponse::Scalar(loc)) = result {
            assert_eq!(loc.range.start.line, 1);
        }
    }

    #[test]
    fn test_definition_variable() {
        let content = r"
fn test(x: i64): i64 {
    let y = x + 1;
    return y;
}
";
        let doc = make_doc(content);
        let uri = Url::parse("file:///test.nimbyscript").expect("valid URL");
        let api = ApiDefinitions::default();

        // Position on "y" in the return statement
        let result = get_definition(&doc, pos(3, 11), &uri, &api);
        assert!(result.is_some(), "Should find definition for variable");

        if let Some(GotoDefinitionResponse::Scalar(loc)) = result {
            // Should point to the let binding on line 2
            assert_eq!(loc.range.start.line, 2);
        }
    }

    #[test]
    fn test_definition_parameter() {
        let content = r"
fn test(x: i64): i64 {
    return x;
}
";
        let doc = make_doc(content);
        let uri = Url::parse("file:///test.nimbyscript").expect("valid URL");
        let api = ApiDefinitions::default();

        // Position on "x" in the return statement
        let result = get_definition(&doc, pos(2, 11), &uri, &api);
        assert!(result.is_some(), "Should find definition for parameter");

        if let Some(GotoDefinitionResponse::Scalar(loc)) = result {
            // Should point to the parameter on line 1
            assert_eq!(loc.range.start.line, 1);
        }
    }

    #[test]
    fn test_references_struct() {
        let content = r"
struct MyStruct {
    field1: i64,
}

fn test(): i64 {
    let x: MyStruct = todo();
    let y: MyStruct = todo();
    return 0;
}
";
        let doc = make_doc(content);
        let uri = Url::parse("file:///test.nimbyscript").expect("valid URL");
        let api = ApiDefinitions::default();

        // Position on "MyStruct" definition
        let result = find_references(&doc, pos(1, 8), &uri, &api, true);
        assert!(result.is_some(), "Should find references for struct");

        let refs = result.expect("should have references");
        // Should find: definition + 2 usages
        assert!(refs.len() >= 3, "Should have at least 3 references");
    }

    #[test]
    fn test_implementations() {
        let content = r"
struct BaseStruct {
    field1: i64,
}

struct ChildStruct1 extend BaseStruct {
    field2: i64,
}

struct ChildStruct2 extend BaseStruct {
    field3: i64,
}
";
        let doc = make_doc(content);
        let uri = Url::parse("file:///test.nimbyscript").expect("valid URL");
        let api = ApiDefinitions::default();

        // Position on "BaseStruct" - but implementations only works for game types
        // that user structs extend, so this test checks the mechanism works
        let result = get_implementations(&doc, pos(1, 8), &uri, &api);
        // In this case BaseStruct is a user struct, and other user structs extend it
        assert!(result.is_some(), "Should find implementations");

        if let Some(GotoImplementationResponse::Array(locs)) = result {
            assert_eq!(locs.len(), 2, "Should find 2 implementations");
        }
    }

    #[test]
    fn test_definition_field_access() {
        let content = r"
struct MyStruct {
    my_field: i64,
}

fn test(s: &MyStruct): i64 {
    return s.my_field;
}
";
        let doc = make_doc(content);
        let uri = Url::parse("file:///test.nimbyscript").expect("valid URL");
        let api = ApiDefinitions::default();

        // Position on "my_field" in the field access
        let result = get_definition(&doc, pos(6, 15), &uri, &api);
        assert!(result.is_some(), "Should find definition for field access");

        if let Some(GotoDefinitionResponse::Scalar(loc)) = result {
            // Should point to the field definition on line 2
            assert_eq!(loc.range.start.line, 2);
        }
    }
}
