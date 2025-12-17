use serde::Deserialize;
use std::collections::HashMap;

/// API definitions loaded from external TOML files
#[derive(Debug, Clone, Default, Deserialize)]
pub struct ApiDefinitions {
    #[serde(default)]
    pub version: String,
    #[serde(default)]
    pub api_version: String,
    #[serde(default)]
    pub types: HashMap<String, TypeDef>,
    #[serde(default)]
    pub enums: HashMap<String, EnumDef>,
    #[serde(default)]
    pub functions: Vec<FunctionDef>,
    #[serde(default)]
    pub modules: HashMap<String, ModuleDef>,
    #[serde(default)]
    pub callbacks: Vec<FunctionDef>,
    /// Methods that all private (non-pub) user structs automatically have
    #[serde(default)]
    pub default_struct_methods: Vec<FunctionDef>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct TypeDef {
    pub kind: TypeKind,
    #[serde(default)]
    pub doc: Option<String>,
    #[serde(default)]
    pub extends: Option<String>,
    #[serde(default)]
    pub fields: HashMap<String, FieldDef>,
    #[serde(default)]
    pub methods: Vec<FunctionDef>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum TypeKind {
    Struct,
    Alias,
}

#[derive(Debug, Clone, Deserialize)]
pub struct FieldDef {
    #[serde(rename = "type")]
    pub ty: String,
    #[serde(default)]
    pub doc: Option<String>,
    #[serde(default)]
    pub readonly: bool,
}

#[derive(Debug, Clone, Deserialize)]
pub struct EnumDef {
    #[serde(default)]
    pub doc: Option<String>,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct EnumVariant {
    pub name: String,
    #[serde(default)]
    pub value: Option<i64>,
    #[serde(default)]
    pub doc: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct FunctionDef {
    pub name: String,
    #[serde(default)]
    pub doc: Option<String>,
    #[serde(default)]
    pub params: Vec<ParamDef>,
    #[serde(default)]
    pub return_type: Option<String>,
    #[serde(default)]
    pub type_params: Vec<String>,
    /// For callbacks: which game type this callback applies to (e.g., "Signal", "Train")
    #[serde(default)]
    pub for_type: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct ParamDef {
    pub name: String,
    #[serde(rename = "type")]
    pub ty: String,
    #[serde(default)]
    pub doc: Option<String>,
    #[serde(default)]
    pub is_mut: bool,
    #[serde(default)]
    pub is_ref: bool,
}

#[derive(Debug, Clone, Deserialize)]
pub struct ModuleDef {
    #[serde(default)]
    pub doc: Option<String>,
    #[serde(default)]
    pub functions: Vec<FunctionDef>,
}

impl ApiDefinitions {
    /// Load API definitions from a TOML file
    pub fn load_from_file(path: &std::path::Path) -> Result<Self, Box<dyn std::error::Error>> {
        let content = std::fs::read_to_string(path)?;
        let api: ApiDefinitions = toml::from_str(&content)?;
        Ok(api)
    }

    /// Load API definitions from a TOML string
    pub fn load_from_str(content: &str) -> Result<Self, toml::de::Error> {
        toml::from_str(content)
    }

    /// Get all type names for completion
    pub fn type_names(&self) -> impl Iterator<Item = &str> {
        self.types.keys().map(std::string::String::as_str)
    }

    /// Get all enum names for completion
    pub fn enum_names(&self) -> impl Iterator<Item = &str> {
        self.enums.keys().map(std::string::String::as_str)
    }

    /// Get all function names for completion
    pub fn function_names(&self) -> impl Iterator<Item = &str> {
        self.functions.iter().map(|f| f.name.as_str())
    }

    /// Get all module names for completion
    pub fn module_names(&self) -> impl Iterator<Item = &str> {
        self.modules.keys().map(std::string::String::as_str)
    }

    /// Get function by name
    pub fn get_function(&self, name: &str) -> Option<&FunctionDef> {
        self.functions.iter().find(|f| f.name == name)
    }

    /// Get type by name
    pub fn get_type(&self, name: &str) -> Option<&TypeDef> {
        self.types.get(name)
    }

    /// Get enum by name
    pub fn get_enum(&self, name: &str) -> Option<&EnumDef> {
        self.enums.get(name)
    }

    /// Get module by name
    pub fn get_module(&self, name: &str) -> Option<&ModuleDef> {
        self.modules.get(name)
    }

    /// Check if a function name is a valid game callback
    pub fn is_valid_callback(&self, name: &str) -> bool {
        self.callbacks.iter().any(|c| c.name == name)
    }

    /// Get callback by name
    pub fn get_callback(&self, name: &str) -> Option<&FunctionDef> {
        self.callbacks.iter().find(|c| c.name == name)
    }

    /// Get all callback names
    pub fn callback_names(&self) -> impl Iterator<Item = &str> {
        self.callbacks.iter().map(|c| c.name.as_str())
    }

    /// Get callbacks that apply to a specific game type (e.g., "Signal", "Train")
    pub fn callbacks_for_type(&self, game_type: &str) -> Vec<&FunctionDef> {
        self.callbacks
            .iter()
            .filter(|c| c.for_type.as_deref() == Some(game_type))
            .collect()
    }

    /// Get a default struct method by name
    pub fn get_default_struct_method(&self, name: &str) -> Option<&FunctionDef> {
        self.default_struct_methods.iter().find(|m| m.name == name)
    }

    /// Check if a method name is a default struct method
    pub fn is_default_struct_method(&self, name: &str) -> bool {
        self.default_struct_methods.iter().any(|m| m.name == name)
    }

    /// Get all default struct method names
    pub fn default_struct_method_names(&self) -> impl Iterator<Item = &str> {
        self.default_struct_methods.iter().map(|m| m.name.as_str())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn load_api() -> ApiDefinitions {
        let toml_content = include_str!("../../../api-definitions/nimbyrails.v1.toml");
        ApiDefinitions::load_from_str(toml_content).expect("Failed to parse TOML")
    }

    // Basic loading tests

    #[test]
    fn test_load_callbacks() {
        let api = load_api();
        assert!(!api.callbacks.is_empty(), "Callbacks should not be empty");
        assert!(
            api.is_valid_callback("event_signal_check"),
            "event_signal_check should be valid"
        );
    }

    #[test]
    fn test_load_from_str_empty() {
        let api = ApiDefinitions::load_from_str("").expect("should parse empty");
        assert!(api.types.is_empty());
        assert!(api.enums.is_empty());
        assert!(api.functions.is_empty());
    }

    #[test]
    fn test_load_from_str_minimal() {
        let toml = r#"
            version = "1.0"
            api_version = "test"
        "#;
        let api = ApiDefinitions::load_from_str(toml).expect("should parse");
        assert_eq!(api.version, "1.0");
        assert_eq!(api.api_version, "test");
    }

    // Type lookup tests

    #[test]
    fn test_get_type_signal() {
        let api = load_api();
        let signal = api.get_type("Signal");
        assert!(signal.is_some(), "Signal type should exist");
        let signal = signal.expect("checked above");
        assert!(matches!(signal.kind, TypeKind::Struct));
    }

    #[test]
    fn test_get_type_nonexistent() {
        let api = load_api();
        assert!(api.get_type("NonExistentType").is_none());
    }

    #[test]
    fn test_type_names_iterator() {
        let api = load_api();
        let names: Vec<_> = api.type_names().collect();
        assert!(!names.is_empty());
        assert!(names.contains(&"Signal"));
        assert!(names.contains(&"Train"));
    }

    #[test]
    fn test_type_has_methods() {
        let api = load_api();
        let signal = api.get_type("Signal").expect("Signal should exist");
        assert!(!signal.methods.is_empty(), "Signal should have methods");
    }

    // Enum lookup tests

    #[test]
    fn test_get_enum_signal_check() {
        let api = load_api();
        let signal_check = api.get_enum("SignalCheck");
        assert!(signal_check.is_some(), "SignalCheck enum should exist");
    }

    #[test]
    fn test_get_enum_nonexistent() {
        let api = load_api();
        assert!(api.get_enum("FakeEnum").is_none());
    }

    #[test]
    fn test_enum_names_iterator() {
        let api = load_api();
        let names: Vec<_> = api.enum_names().collect();
        assert!(!names.is_empty());
        assert!(names.contains(&"SignalCheck"));
        assert!(names.contains(&"SignalAspect"));
    }

    #[test]
    fn test_enum_variants() {
        let api = load_api();
        let signal_check = api.get_enum("SignalCheck").expect("should exist");
        assert!(!signal_check.variants.is_empty());
        let variant_names: Vec<_> = signal_check
            .variants
            .iter()
            .map(|v| v.name.as_str())
            .collect();
        assert!(variant_names.contains(&"Pass"));
        assert!(variant_names.contains(&"Stop"));
    }

    // Function lookup tests

    #[test]
    fn test_get_function_abs() {
        let api = load_api();
        let abs = api.get_function("abs");
        assert!(abs.is_some(), "abs function should exist");
        let abs = abs.expect("checked above");
        assert!(!abs.params.is_empty());
    }

    #[test]
    fn test_get_function_nonexistent() {
        let api = load_api();
        assert!(api.get_function("not_a_real_function").is_none());
    }

    #[test]
    fn test_function_names_iterator() {
        let api = load_api();
        let names: Vec<_> = api.function_names().collect();
        assert!(!names.is_empty());
        assert!(names.contains(&"abs"));
        assert!(names.contains(&"sqrt"));
    }

    // DB/Sim/Extrapolator type tests (formerly modules)

    #[test]
    fn test_get_type_db() {
        let api = load_api();
        let db = api.get_type("DB").expect("DB type should exist");
        assert!(!db.methods.is_empty(), "DB should have methods");
    }

    #[test]
    fn test_get_type_sim() {
        let api = load_api();
        let sim = api.get_type("Sim").expect("Sim type should exist");
        assert!(!sim.methods.is_empty(), "Sim should have methods");
    }

    #[test]
    fn test_get_module_nonexistent() {
        let api = load_api();
        assert!(api.get_module("FakeModule").is_none());
    }

    #[test]
    fn test_context_type_db() {
        let api = load_api();
        let ctx_type = api
            .get_type("ControlCtx")
            .expect("ControlCtx type should exist");
        // ControlCtx should have db, sim, extrapolator fields
        assert!(ctx_type.fields.contains_key("db"));
        assert!(ctx_type.fields.contains_key("sim"));
        assert!(ctx_type.fields.contains_key("extrapolator"));
    }

    #[test]
    fn test_type_db_methods() {
        let api = load_api();
        let db = api.get_type("DB").expect("DB should exist");
        assert!(!db.methods.is_empty(), "DB should have methods");
        let method_names: Vec<_> = db.methods.iter().map(|m| m.name.as_str()).collect();
        assert!(method_names.contains(&"view"), "DB should have view method");
    }

    // Callback lookup tests

    #[test]
    fn test_callbacks_for_type_signal() {
        let api = load_api();
        let signal_callbacks = api.callbacks_for_type("Signal");
        assert!(!signal_callbacks.is_empty(), "Signal should have callbacks");
        let names: Vec<_> = signal_callbacks.iter().map(|c| c.name.as_str()).collect();
        assert!(names.contains(&"event_signal_check"));
    }

    #[test]
    fn test_callbacks_for_type_train() {
        let api = load_api();
        let train_callbacks = api.callbacks_for_type("Train");
        assert!(!train_callbacks.is_empty(), "Train should have callbacks");
    }

    #[test]
    fn test_callbacks_for_type_nonexistent() {
        let api = load_api();
        let callbacks = api.callbacks_for_type("NotARealType");
        assert!(callbacks.is_empty());
    }

    #[test]
    fn test_is_valid_callback_true() {
        let api = load_api();
        assert!(api.is_valid_callback("event_signal_check"));
        assert!(api.is_valid_callback("event_signal_lookahead"));
    }

    #[test]
    fn test_is_valid_callback_false() {
        let api = load_api();
        assert!(!api.is_valid_callback("not_a_callback"));
        assert!(!api.is_valid_callback("random_name"));
    }

    #[test]
    fn test_get_callback() {
        let api = load_api();
        let cb = api.get_callback("event_signal_check");
        assert!(cb.is_some());
        let cb = cb.expect("checked above");
        assert_eq!(cb.name, "event_signal_check");
        assert!(cb.return_type.is_some());
    }

    #[test]
    fn test_callback_names_iterator() {
        let api = load_api();
        let names: Vec<_> = api.callback_names().collect();
        assert!(!names.is_empty());
        assert!(names.contains(&"event_signal_check"));
    }

    // Deserialization edge cases

    #[test]
    fn test_param_def_fields() {
        let api = load_api();
        let cb = api
            .get_callback("event_signal_check")
            .expect("should exist");
        // Check that params have expected structure
        assert!(!cb.params.is_empty());
        for param in &cb.params {
            assert!(!param.name.is_empty());
            assert!(!param.ty.is_empty());
        }
    }

    #[test]
    fn test_function_return_type() {
        let api = load_api();
        let cb = api
            .get_callback("event_signal_check")
            .expect("should exist");
        assert!(cb.return_type.is_some());
        assert_eq!(cb.return_type.as_deref(), Some("SignalCheck"));
    }
}
