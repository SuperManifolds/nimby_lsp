//! Semantic analysis context.
//!
//! Provides the shared state for all semantic analysis passes.

use std::collections::HashMap;

use nimbyscript_parser::ast::Span;
use nimbyscript_parser::{kind, Node, NodeExt, Tree};

use crate::api::ApiDefinitions;
use crate::scope::{ScopeManager, SymbolKind};
use crate::types::{parse_type_string, TypeInfo};

/// Context passed to all semantic passes.
///
/// Contains the source code, parsed tree, API definitions,
/// and accumulated semantic information.
pub struct SemanticContext<'a> {
    /// The source code being analyzed
    pub source: &'a str,

    /// The parsed tree-sitter tree
    pub tree: &'a Tree,

    /// Game API definitions
    pub api: &'a ApiDefinitions,

    /// Scope and symbol management
    pub scopes: ScopeManager,

    /// User-defined structs: name -> extends type (if any)
    pub user_structs: HashMap<String, Option<String>>,

    /// User-defined enums: name -> list of variant names
    pub user_enums: HashMap<String, Vec<String>>,

    /// User-defined functions: name -> return type (if any)
    pub user_functions: HashMap<String, Option<TypeInfo>>,

    /// Struct fields: struct_name -> (field_name -> field_type)
    pub struct_fields: HashMap<String, HashMap<String, TypeInfo>>,

    /// Public structs (entry points)
    pub pub_structs: std::collections::HashSet<String>,

    /// Public enums
    pub pub_enums: std::collections::HashSet<String>,

    /// Public functions (callbacks)
    pub pub_functions: std::collections::HashSet<String>,
}

impl<'a> SemanticContext<'a> {
    /// Create a new semantic context
    pub fn new(source: &'a str, tree: &'a Tree, api: &'a ApiDefinitions) -> Self {
        Self {
            source,
            tree,
            api,
            scopes: ScopeManager::new(),
            user_structs: HashMap::new(),
            user_enums: HashMap::new(),
            user_functions: HashMap::new(),
            struct_fields: HashMap::new(),
            pub_structs: std::collections::HashSet::new(),
            pub_enums: std::collections::HashSet::new(),
            pub_functions: std::collections::HashSet::new(),
        }
    }

    /// Resolve a type name to a TypeInfo.
    /// Checks API types, user-defined types, and primitives.
    pub fn resolve_type(&self, name: &str) -> TypeInfo {
        // Parse the type string first (handles references, generics, etc.)
        let parsed = parse_type_string(name);

        // If it parsed to a struct, check if we know more about it
        if let TypeInfo::Struct {
            name: type_name, ..
        } = &parsed
        {
            // Check if it's a known game type
            if self.api.get_type(type_name).is_some() {
                return TypeInfo::Struct {
                    name: type_name.clone(),
                    extends: None,
                };
            }

            // Check if it's a known game enum
            if self.api.get_enum(type_name).is_some() {
                return TypeInfo::Enum {
                    name: type_name.clone(),
                };
            }

            // Check if it's a user-defined struct
            if let Some(extends) = self.user_structs.get(type_name) {
                return TypeInfo::Struct {
                    name: type_name.clone(),
                    extends: extends.clone(),
                };
            }

            // Check if it's a user-defined enum
            if self.user_enums.contains_key(type_name) {
                return TypeInfo::Enum {
                    name: type_name.clone(),
                };
            }
        }

        parsed
    }

    /// Check if a type name is a valid game type that can be extended
    pub fn is_extendable_game_type(&self, name: &str) -> bool {
        // These are the valid game types that can be extended
        matches!(
            name,
            "Signal" | "Train" | "Script" | "Station" | "Line" | "Schedule" | "Tag"
        )
    }

    /// Check if a type name is a valid game type (including non-extendable ones)
    pub fn is_game_type(&self, name: &str) -> bool {
        self.api.get_type(name).is_some()
    }

    /// Check if a type name is a valid game enum
    pub fn is_game_enum(&self, name: &str) -> bool {
        self.api.get_enum(name).is_some()
    }

    /// Check if a type name is a valid user-defined struct
    pub fn is_user_struct(&self, name: &str) -> bool {
        self.user_structs.contains_key(name)
    }

    /// Check if a type name is a valid user-defined enum
    pub fn is_user_enum(&self, name: &str) -> bool {
        self.user_enums.contains_key(name)
    }

    /// Check if a type is valid for a pub struct field.
    /// Per NimbyScript spec: bool, i64, f64, script enums, and ID<>
    pub fn is_valid_pub_struct_field_type(&self, type_info: &TypeInfo) -> bool {
        match type_info {
            TypeInfo::Bool | TypeInfo::I64 | TypeInfo::F64 => true,

            // User-defined enums are allowed (parsed as Enum variant or Struct)
            TypeInfo::Enum { name } | TypeInfo::Struct { name, .. } => self.is_user_enum(name),

            // ID<T> is allowed for specific types
            TypeInfo::Generic { name, args } if name == "ID" => {
                if args.len() == 1 {
                    if let TypeInfo::Struct { name: inner, .. } = &args[0] {
                        return matches!(
                            inner.as_str(),
                            "Line" | "Train" | "Schedule" | "Signal" | "Tag"
                        );
                    }
                }
                false
            }

            _ => false,
        }
    }

    /// Get the fields of a struct (user-defined or game type)
    pub fn get_struct_fields(&self, name: &str) -> Option<HashMap<String, TypeInfo>> {
        // Check user-defined structs first
        if let Some(fields) = self.struct_fields.get(name) {
            return Some(fields.clone());
        }

        // Check game types
        if let Some(type_def) = self.api.get_type(name) {
            let fields: HashMap<_, _> = type_def
                .fields
                .iter()
                .map(|(name, field)| (name.clone(), parse_type_string(&field.ty)))
                .collect();
            return Some(fields);
        }

        None
    }

    /// Get the methods of a type (game type methods)
    pub fn get_type_methods(&self, type_name: &str) -> Vec<(&str, Option<&str>)> {
        if let Some(type_def) = self.api.get_type(type_name) {
            type_def
                .methods
                .iter()
                .map(|m| (m.name.as_str(), m.return_type.as_deref()))
                .collect()
        } else {
            Vec::new()
        }
    }

    /// Get a specific method on a type by name
    pub fn get_type_method(&self, type_name: &str, method_name: &str) -> Option<&crate::api::FunctionDef> {
        self.api
            .get_type(type_name)
            .and_then(|t| t.methods.iter().find(|m| m.name == method_name))
    }

    /// Check if a method exists on a type
    pub fn has_method(&self, type_name: &str, method_name: &str) -> bool {
        self.get_type_method(type_name, method_name).is_some()
    }

    /// Check if a static method exists on a game type.
    /// For ID<T> types, only "empty" is a valid static method.
    /// For other game types, check API definitions.
    pub fn has_static_method(&self, type_name: &str, method_name: &str) -> bool {
        // For ID<T> types, only "empty" is a static method
        if type_name.starts_with("ID<") {
            return method_name == "empty";
        }
        // Other game types - check API definitions for static methods
        // Currently, most game type methods are instance methods, not static
        // This may need to be expanded if more static methods are added
        false
    }

    /// Get the variants of an enum (user-defined or game enum)
    pub fn get_enum_variants(&self, name: &str) -> Option<Vec<String>> {
        // Check user-defined enums first
        if let Some(variants) = self.user_enums.get(name) {
            return Some(variants.clone());
        }

        // Check game enums
        if let Some(enum_def) = self.api.get_enum(name) {
            return Some(enum_def.variants.iter().map(|v| v.name.clone()).collect());
        }

        None
    }

    /// Check if a name is a valid module
    pub fn is_module(&self, name: &str) -> bool {
        self.api.get_module(name).is_some()
    }

    /// Get a function from a module
    pub fn get_module_function(&self, module: &str, func: &str) -> Option<&crate::api::FunctionDef> {
        self.api
            .get_module(module)
            .and_then(|m| m.functions.iter().find(|f| f.name == func))
    }

    /// Get a global function from the API
    pub fn get_global_function(&self, name: &str) -> Option<&crate::api::FunctionDef> {
        self.api.get_function(name)
    }

    /// Get parameter info for any callable: user function or API function
    pub fn get_function_params(&self, name: &str) -> Option<FunctionParamInfo> {
        // Check API functions (includes built-ins like abs, max, min, etc.)
        if let Some(func) = self.api.get_function(name) {
            return Some(FunctionParamInfo {
                min_params: func.params.len(),
                max_params: func.params.len(),
                param_types: func.params.iter().map(|p| p.ty.clone()).collect(),
                return_type: func.return_type.as_ref().map(|t| self.resolve_type(t)),
            });
        }

        // Check user functions - we'd need to store param info during collection
        // For now, skip validation for user functions
        if self.user_functions.contains_key(name) {
            return Some(FunctionParamInfo {
                min_params: 0,
                max_params: usize::MAX,
                param_types: vec![],
                return_type: self.user_functions.get(name).and_then(Clone::clone),
            });
        }

        None
    }

    /// Check if a struct is public
    pub fn is_pub_struct(&self, name: &str) -> bool {
        self.pub_structs.contains(name)
    }

    /// Check if an enum is public
    pub fn is_pub_enum(&self, name: &str) -> bool {
        self.pub_enums.contains(name)
    }

    /// Check if a function is public
    pub fn is_pub_function(&self, name: &str) -> bool {
        self.pub_functions.contains(name)
    }
}

/// Collect declarations from the tree into the context.
/// This should be run before other semantic passes.
pub fn collect_declarations(ctx: &mut SemanticContext) {
    let root = ctx.tree.root_node();
    collect_declarations_from_node(root, ctx);
}

fn collect_declarations_from_node(node: Node, ctx: &mut SemanticContext) {
    match node.kind() {
        kind::STRUCT_DEFINITION => collect_struct(node, ctx),
        kind::ENUM_DEFINITION => collect_enum(node, ctx),
        kind::FUNCTION_DEFINITION => collect_function(node, ctx),
        kind::CONST_DECLARATION => collect_const(node, ctx),
        _ => {
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                collect_declarations_from_node(child, ctx);
            }
        }
    }
}

fn collect_struct(node: Node, ctx: &mut SemanticContext) {
    let Some(name_node) = node.child_by_field("name") else {
        return;
    };
    let name = name_node.text(ctx.source).to_string();

    // Check if pub
    let is_pub = node.child_by_kind(kind::VISIBILITY_MODIFIER).is_some();
    if is_pub {
        ctx.pub_structs.insert(name.clone());
    }

    // Get extends clause
    let extends = node.child_by_kind(kind::EXTENDS_CLAUSE).and_then(|ext| {
        ext.child_by_field("type")
            .map(|t| t.text(ctx.source).to_string())
    });

    ctx.user_structs.insert(name.clone(), extends.clone());

    // Define in scope
    let type_info = if let Some(ext) = &extends {
        TypeInfo::struct_extending(&name, ext)
    } else {
        TypeInfo::struct_type(&name)
    };

    let _ = ctx.scopes.define_global(
        &name,
        SymbolKind::Struct,
        type_info,
        Span::new(node.start_byte(), node.end_byte()),
        Span::new(name_node.start_byte(), name_node.end_byte()),
    );

    // Collect fields
    let mut fields = HashMap::new();
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == kind::STRUCT_FIELD {
            if let (Some(field_name), Some(field_type)) =
                (child.child_by_field("name"), child.child_by_field("type"))
            {
                let field_name_str = field_name.text(ctx.source).to_string();
                let field_type_str = field_type.text(ctx.source);
                let type_info = ctx.resolve_type(field_type_str);
                fields.insert(field_name_str, type_info);
            }
        }
    }
    ctx.struct_fields.insert(name, fields);
}

fn collect_enum(node: Node, ctx: &mut SemanticContext) {
    let Some(name_node) = node.child_by_field("name") else {
        return;
    };
    let name = name_node.text(ctx.source).to_string();

    // Check if pub
    let is_pub = node.child_by_kind(kind::VISIBILITY_MODIFIER).is_some();
    if is_pub {
        ctx.pub_enums.insert(name.clone());
    }

    // Collect variants
    let mut variants = Vec::new();
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == kind::ENUM_VARIANT {
            if let Some(variant_name) = child.child_by_field("name") {
                variants.push(variant_name.text(ctx.source).to_string());
            }
        }
    }

    ctx.user_enums.insert(name.clone(), variants);

    // Define in scope
    let _ = ctx.scopes.define_global(
        &name,
        SymbolKind::Enum,
        TypeInfo::enum_type(&name),
        Span::new(node.start_byte(), node.end_byte()),
        Span::new(name_node.start_byte(), name_node.end_byte()),
    );
}

fn collect_function(node: Node, ctx: &mut SemanticContext) {
    let Some(name_node) = node.child_by_field("name") else {
        return;
    };
    let name = name_node.text(ctx.source).to_string();

    // Check if pub (or if it's a method/callback - methods with :: are effectively pub)
    let is_pub = node.child_by_kind(kind::VISIBILITY_MODIFIER).is_some() || name.contains("::");
    if is_pub {
        ctx.pub_functions.insert(name.clone());
    }

    // Get return type
    let return_type = node.child_by_field("return_type").map(|rt| {
        let type_str = rt.text(ctx.source);
        ctx.resolve_type(type_str)
    });

    ctx.user_functions.insert(name.clone(), return_type.clone());

    // Determine if it's a method (contains ::)
    let kind = if name.contains("::") {
        SymbolKind::Method
    } else {
        SymbolKind::Function
    };

    let type_info = return_type.clone().unwrap_or(TypeInfo::Void);

    let _ = ctx.scopes.define_global(
        &name,
        kind,
        type_info,
        Span::new(node.start_byte(), node.end_byte()),
        Span::new(name_node.start_byte(), name_node.end_byte()),
    );
}

fn collect_const(node: Node, ctx: &mut SemanticContext) {
    let Some(name_node) = node.child_by_field("name") else {
        return;
    };
    let name = name_node.text(ctx.source).to_string();

    // Get type
    let type_info = node
        .child_by_field("type")
        .map_or(TypeInfo::Unknown, |t| ctx.resolve_type(t.text(ctx.source)));

    let _ = ctx.scopes.define_global(
        &name,
        SymbolKind::Constant,
        type_info,
        Span::new(node.start_byte(), node.end_byte()),
        Span::new(name_node.start_byte(), name_node.end_byte()),
    );
}

/// Parameter info for function call validation
#[derive(Debug, Clone)]
pub struct FunctionParamInfo {
    pub min_params: usize,
    pub max_params: usize,
    pub param_types: Vec<String>,
    pub return_type: Option<TypeInfo>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use nimbyscript_parser::parse;

    fn load_api() -> ApiDefinitions {
        let toml = include_str!("../../../../api-definitions/nimbyrails.v1.toml");
        ApiDefinitions::load_from_str(toml).expect("should parse")
    }

    fn make_context(source: &str) -> (Tree, ApiDefinitions) {
        let tree = parse(source);
        let api = load_api();
        (tree, api)
    }

    #[test]
    fn test_resolve_primitive_types() {
        let source = "script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }";
        let (tree, api) = make_context(source);
        let ctx = SemanticContext::new(source, &tree, &api);

        assert_eq!(ctx.resolve_type("bool"), TypeInfo::Bool);
        assert_eq!(ctx.resolve_type("i64"), TypeInfo::I64);
        assert_eq!(ctx.resolve_type("f64"), TypeInfo::F64);
        assert_eq!(ctx.resolve_type("String"), TypeInfo::String);
    }

    #[test]
    fn test_resolve_reference_type() {
        let source = "script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }";
        let (tree, api) = make_context(source);
        let ctx = SemanticContext::new(source, &tree, &api);

        let ref_type = ctx.resolve_type("&i64");
        assert!(matches!(
            ref_type,
            TypeInfo::Reference {
                is_mut: false,
                ..
            }
        ));
    }

    #[test]
    fn test_resolve_game_type() {
        let source = "script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }";
        let (tree, api) = make_context(source);
        let ctx = SemanticContext::new(source, &tree, &api);

        let signal = ctx.resolve_type("Signal");
        assert!(matches!(signal, TypeInfo::Struct { name, .. } if name == "Signal"));
    }

    #[test]
    fn test_is_extendable_game_type() {
        let source = "script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }";
        let (tree, api) = make_context(source);
        let ctx = SemanticContext::new(source, &tree, &api);

        assert!(ctx.is_extendable_game_type("Signal"));
        assert!(ctx.is_extendable_game_type("Train"));
        assert!(ctx.is_extendable_game_type("Script"));
        assert!(!ctx.is_extendable_game_type("Motion")); // Not extendable
    }

    #[test]
    fn test_collect_declarations_struct() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct MyHandler extend Signal {
    count: i64,
}
";
        let (tree, api) = make_context(source);
        let mut ctx = SemanticContext::new(source, &tree, &api);
        collect_declarations(&mut ctx);

        assert!(ctx.user_structs.contains_key("MyHandler"));
        assert_eq!(ctx.user_structs.get("MyHandler"), Some(&Some("Signal".to_string())));

        let fields = ctx.struct_fields.get("MyHandler").expect("should have fields");
        assert!(fields.contains_key("count"));
    }

    #[test]
    fn test_collect_declarations_enum() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub enum Status { Active, Inactive, }
";
        let (tree, api) = make_context(source);
        let mut ctx = SemanticContext::new(source, &tree, &api);
        collect_declarations(&mut ctx);

        assert!(ctx.user_enums.contains_key("Status"));
        let variants = ctx.user_enums.get("Status").expect("should have variants");
        assert!(variants.contains(&"Active".to_string()));
        assert!(variants.contains(&"Inactive".to_string()));
    }

    #[test]
    fn test_collect_declarations_function() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn my_func(): i64 { return 0; }
";
        let (tree, api) = make_context(source);
        let mut ctx = SemanticContext::new(source, &tree, &api);
        collect_declarations(&mut ctx);

        assert!(ctx.user_functions.contains_key("my_func"));
        let ret_type = ctx.user_functions.get("my_func").expect("should exist");
        assert_eq!(ret_type, &Some(TypeInfo::I64));
    }

    #[test]
    fn test_is_valid_pub_struct_field_type() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub enum MyEnum { A, }
";
        let (tree, api) = make_context(source);
        let mut ctx = SemanticContext::new(source, &tree, &api);
        collect_declarations(&mut ctx);

        assert!(ctx.is_valid_pub_struct_field_type(&TypeInfo::Bool));
        assert!(ctx.is_valid_pub_struct_field_type(&TypeInfo::I64));
        assert!(ctx.is_valid_pub_struct_field_type(&TypeInfo::F64));

        // User enum is valid
        assert!(ctx.is_valid_pub_struct_field_type(&TypeInfo::enum_type("MyEnum")));

        // ID<Signal> is valid
        let id_signal = TypeInfo::generic("ID", vec![TypeInfo::struct_type("Signal")]);
        assert!(ctx.is_valid_pub_struct_field_type(&id_signal));

        // String is NOT valid for pub struct
        assert!(!ctx.is_valid_pub_struct_field_type(&TypeInfo::String));
    }

    #[test]
    fn test_get_enum_variants_user() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub enum Color { Red, Green, Blue, }
";
        let (tree, api) = make_context(source);
        let mut ctx = SemanticContext::new(source, &tree, &api);
        collect_declarations(&mut ctx);

        let variants = ctx.get_enum_variants("Color").expect("should have variants");
        assert!(variants.contains(&"Red".to_string()));
        assert!(variants.contains(&"Green".to_string()));
        assert!(variants.contains(&"Blue".to_string()));
    }

    #[test]
    fn test_get_enum_variants_game() {
        let source = "script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }";
        let (tree, api) = make_context(source);
        let ctx = SemanticContext::new(source, &tree, &api);

        let variants = ctx.get_enum_variants("SignalCheck").expect("should have variants");
        assert!(variants.contains(&"Pass".to_string()));
        assert!(variants.contains(&"Stop".to_string()));
    }

    #[test]
    fn test_has_type_db_sim() {
        let source = "script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }";
        let (tree, api) = make_context(source);
        let ctx = SemanticContext::new(source, &tree, &api);

        // DB, Sim, Extrapolator are types (not modules)
        assert!(!ctx.resolve_type("DB").is_unknown());
        assert!(!ctx.resolve_type("Sim").is_unknown());
        assert!(!ctx.resolve_type("Extrapolator").is_unknown());
        assert!(!ctx.resolve_type("ControlCtx").is_unknown());
    }

    #[test]
    fn test_type_has_method() {
        let source = "script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }";
        let (tree, api) = make_context(source);
        let ctx = SemanticContext::new(source, &tree, &api);

        // DB should have view method
        assert!(ctx.has_method("DB", "view"));
        // Sim should have view method
        assert!(ctx.has_method("Sim", "view"));
        // Extrapolator should have clock_us method
        assert!(ctx.has_method("Extrapolator", "clock_us"));
        // Invalid method should return false
        assert!(!ctx.has_method("DB", "not_a_method"));
    }
}
