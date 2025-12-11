//! Semantic analysis framework for NimbyScript.
//!
//! Provides the infrastructure for running semantic analysis passes
//! on NimbyScript code.

pub mod context;
pub mod passes;

pub use context::{collect_declarations, SemanticContext};
pub use passes::PassRegistry;

use crate::Diagnostic;

/// A semantic analysis pass.
///
/// Each pass implements a specific category of semantic checks
/// (e.g., name resolution, type checking, control flow).
pub trait SemanticPass: Send + Sync {
    /// Name of this pass for debugging/logging
    fn name(&self) -> &'static str;

    /// Run the pass on the semantic context and collect diagnostics
    fn run(&self, ctx: &mut SemanticContext, diagnostics: &mut Vec<Diagnostic>);
}
