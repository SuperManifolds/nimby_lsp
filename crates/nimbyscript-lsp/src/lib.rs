//! NimbyScript Language Server Protocol implementation.
//!
//! This crate provides the LSP server for NimbyScript, offering:
//! - Diagnostics (errors and warnings)
//! - Completions
//! - Hover information
//! - Go to definition
//! - Find references
//! - Inlay hints
//! - Semantic tokens
//! - Signature help

pub mod backend;
mod completions;
pub mod document;
mod folding_range;
mod formatting;
pub mod hover;
pub mod inlay_hints;
mod linked_editing;
mod navigation;
mod rename;
mod selection_range;
pub mod semantic_tokens;
mod signature_help;
mod type_hierarchy;
mod type_inference;
mod validation;
mod workspace_symbol;

#[cfg(test)]
mod test_helpers;
