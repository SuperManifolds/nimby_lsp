//! Semantic analysis passes for NimbyScript.
//!
//! Each pass implements specific checks in a particular category.

mod control_flow;
mod name_resolution;
mod nimbyscript_rules;
mod scope_analysis;
mod struct_validation;
mod type_checking;

pub use control_flow::ControlFlowPass;
pub use name_resolution::NameResolutionPass;
pub use nimbyscript_rules::NimbyScriptRulesPass;
pub use scope_analysis::ScopeAnalysisPass;
pub use struct_validation::StructValidationPass;
pub use type_checking::TypeCheckingPass;

use crate::semantic::{SemanticContext, SemanticPass};
use crate::Diagnostic;

/// Registry of all semantic passes.
///
/// Runs passes in the correct dependency order.
pub struct PassRegistry {
    passes: Vec<Box<dyn SemanticPass>>,
}

impl Default for PassRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl PassRegistry {
    /// Create a new registry with all standard passes
    pub fn new() -> Self {
        let mut registry = Self { passes: Vec::new() };

        // Add passes in dependency order:
        // 1. Struct validation first (validates type definitions)
        registry.register(Box::new(StructValidationPass));
        // 2. Name resolution (checks all references)
        registry.register(Box::new(NameResolutionPass));
        // 3. Type checking (validates type compatibility)
        registry.register(Box::new(TypeCheckingPass));
        // 4. Scope analysis (usage tracking, shadowing)
        registry.register(Box::new(ScopeAnalysisPass));
        // 5. Control flow (break/continue, return)
        registry.register(Box::new(ControlFlowPass));
        // 6. NimbyScript-specific rules
        registry.register(Box::new(NimbyScriptRulesPass));

        registry
    }

    /// Register a pass
    pub fn register(&mut self, pass: Box<dyn SemanticPass>) {
        self.passes.push(pass);
    }

    /// Run all passes and collect diagnostics
    pub fn run_all(&self, ctx: &mut SemanticContext) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        for pass in &self.passes {
            pass.run(ctx, &mut diagnostics);
        }

        diagnostics
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::api::ApiDefinitions;
    use crate::semantic::context::collect_declarations;
    use nimbyscript_parser::parse;

    fn load_api() -> ApiDefinitions {
        let toml = include_str!("../../../../../api-definitions/nimbyrails.v1.toml");
        ApiDefinitions::load_from_str(toml).expect("should parse")
    }

    #[test]
    fn test_registry_creation() {
        let registry = PassRegistry::new();
        assert!(!registry.passes.is_empty());
    }

    #[test]
    fn test_run_all_on_valid_code() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal { }
";
        let tree = parse(source);
        let api = load_api();
        let mut ctx = SemanticContext::new(source, &tree, &api);
        collect_declarations(&mut ctx);

        let registry = PassRegistry::new();
        let diagnostics = registry.run_all(&mut ctx);

        // Should have minimal/no errors for valid simple code
        // (There might be warnings like unused struct)
        let errors: Vec<_> = diagnostics
            .iter()
            .filter(|d| matches!(d.severity, crate::diagnostics::Severity::Error))
            .collect();
        assert!(
            errors.is_empty(),
            "Valid code should have no errors: {errors:?}"
        );
    }
}
