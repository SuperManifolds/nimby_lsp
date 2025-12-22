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
mod hover;
mod inlay_hints;
mod navigation;
mod semantic_tokens;
mod signature_help;
mod type_hierarchy;
mod type_inference;
mod validation;
