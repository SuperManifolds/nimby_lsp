use dashmap::DashMap;
use nimbyscript_parser::ast::{Identifier, Span};

/// Symbol kinds
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Const,
    Struct,
    Enum,
    EnumVariant,
    Function,
    Method,
    Parameter,
    Variable,
    Field,
}

/// A symbol in the symbol table
#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    pub span: Span,
    pub type_name: Option<String>,
    pub doc: Option<String>,
}

impl Symbol {
    pub fn new(name: impl Into<String>, kind: SymbolKind, span: Span) -> Self {
        Self {
            name: name.into(),
            kind,
            span,
            type_name: None,
            doc: None,
        }
    }

    pub fn with_type(mut self, type_name: impl Into<String>) -> Self {
        self.type_name = Some(type_name.into());
        self
    }

    pub fn with_doc(mut self, doc: impl Into<String>) -> Self {
        self.doc = Some(doc.into());
        self
    }
}

/// Thread-safe symbol table for a document
#[derive(Debug, Default)]
pub struct SymbolTable {
    /// Global symbols (structs, enums, functions, constants)
    globals: DashMap<String, Symbol>,
    /// Local symbols by scope (function name -> symbols)
    locals: DashMap<String, Vec<Symbol>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a global symbol
    pub fn add_global(&self, symbol: Symbol) {
        self.globals.insert(symbol.name.clone(), symbol);
    }

    /// Add a local symbol to a scope
    pub fn add_local(&self, scope: &str, symbol: Symbol) {
        self.locals
            .entry(scope.to_string())
            .or_default()
            .push(symbol);
    }

    /// Get a global symbol by name
    pub fn get_global(&self, name: &str) -> Option<Symbol> {
        self.globals.get(name).map(|r| r.clone())
    }

    /// Get all global symbols
    pub fn all_globals(&self) -> Vec<Symbol> {
        self.globals.iter().map(|r| r.value().clone()).collect()
    }

    /// Get local symbols for a scope
    pub fn get_locals(&self, scope: &str) -> Vec<Symbol> {
        self.locals
            .get(scope)
            .map(|r| r.clone())
            .unwrap_or_default()
    }

    /// Find symbol at position
    pub fn find_at_position(&self, pos: usize) -> Option<Symbol> {
        // Check globals
        for entry in &self.globals {
            let symbol = entry.value();
            if pos >= symbol.span.start && pos < symbol.span.end {
                return Some(symbol.clone());
            }
        }

        // Check all locals
        for entry in &self.locals {
            for symbol in entry.value() {
                if pos >= symbol.span.start && pos < symbol.span.end {
                    return Some(symbol.clone());
                }
            }
        }

        None
    }

    /// Clear the symbol table
    pub fn clear(&self) {
        self.globals.clear();
        self.locals.clear();
    }

    /// Build symbols from an identifier
    pub fn symbol_from_ident(ident: &Identifier, kind: SymbolKind) -> Symbol {
        Symbol::new(&ident.name, kind, ident.span)
    }
}
