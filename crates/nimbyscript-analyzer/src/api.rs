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
        self.types.keys().map(|s| s.as_str())
    }

    /// Get all enum names for completion
    pub fn enum_names(&self) -> impl Iterator<Item = &str> {
        self.enums.keys().map(|s| s.as_str())
    }

    /// Get all function names for completion
    pub fn function_names(&self) -> impl Iterator<Item = &str> {
        self.functions.iter().map(|f| f.name.as_str())
    }

    /// Get all module names for completion
    pub fn module_names(&self) -> impl Iterator<Item = &str> {
        self.modules.keys().map(|s| s.as_str())
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_load_callbacks() {
        let toml_content = include_str!("../../../api-definitions/nimbyrails.v1.toml");
        let api = ApiDefinitions::load_from_str(toml_content).expect("Failed to parse TOML");
        println!("Loaded {} callbacks", api.callbacks.len());
        for cb in &api.callbacks {
            println!("  - {}", cb.name);
        }
        assert!(!api.callbacks.is_empty(), "Callbacks should not be empty");
        assert!(api.is_valid_callback("event_signal_check"), "event_signal_check should be valid");
    }
}
