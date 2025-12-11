pub mod api;
pub mod diagnostics;
pub mod scope;
pub mod semantic;
pub mod symbols;
pub mod types;

pub use api::{ApiDefinitions, FunctionDef, ParamDef};
pub use diagnostics::Diagnostic;
pub use scope::ScopeManager;
pub use semantic::{collect_declarations, SemanticContext, SemanticPass};
pub use symbols::SymbolTable;
pub use types::TypeInfo;
