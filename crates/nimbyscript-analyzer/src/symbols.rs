use dashmap::DashMap;
use nimbyscript_parser::ast::{Identifier, Span};

// Re-export LSP's SymbolKind for use by consumers
pub use lsp_types::SymbolKind;

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

#[cfg(test)]
mod tests {
    use super::*;

    fn span(start: usize, end: usize) -> Span {
        Span::new(start, end)
    }

    // Symbol creation and builder pattern tests

    #[test]
    fn test_symbol_new() {
        let sym = Symbol::new("foo", SymbolKind::FUNCTION, span(0, 10));
        assert_eq!(sym.name, "foo");
        assert_eq!(sym.kind, SymbolKind::FUNCTION);
        assert_eq!(sym.span.start, 0);
        assert_eq!(sym.span.end, 10);
        assert!(sym.type_name.is_none());
        assert!(sym.doc.is_none());
    }

    #[test]
    fn test_symbol_with_type() {
        let sym = Symbol::new("x", SymbolKind::VARIABLE, span(5, 6)).with_type("i64");
        assert_eq!(sym.type_name, Some("i64".to_string()));
    }

    #[test]
    fn test_symbol_with_doc() {
        let sym = Symbol::new("foo", SymbolKind::FUNCTION, span(0, 10)).with_doc("Does stuff");
        assert_eq!(sym.doc, Some("Does stuff".to_string()));
    }

    #[test]
    fn test_symbol_builder_chain() {
        let sym = Symbol::new("bar", SymbolKind::STRUCT, span(0, 20))
            .with_type("Signal")
            .with_doc("A signal handler");
        assert_eq!(sym.name, "bar");
        assert_eq!(sym.type_name, Some("Signal".to_string()));
        assert_eq!(sym.doc, Some("A signal handler".to_string()));
    }

    // SymbolTable global operations tests

    #[test]
    fn test_add_global() {
        let table = SymbolTable::new();
        let sym = Symbol::new("MyStruct", SymbolKind::STRUCT, span(0, 50));
        table.add_global(sym);
        assert!(table.get_global("MyStruct").is_some());
    }

    #[test]
    fn test_get_global_exists() {
        let table = SymbolTable::new();
        table.add_global(Symbol::new("foo", SymbolKind::FUNCTION, span(10, 20)));
        let result = table.get_global("foo");
        assert!(result.is_some());
        let sym = result.expect("symbol should exist");
        assert_eq!(sym.name, "foo");
        assert_eq!(sym.kind, SymbolKind::FUNCTION);
    }

    #[test]
    fn test_get_global_missing() {
        let table = SymbolTable::new();
        assert!(table.get_global("nonexistent").is_none());
    }

    #[test]
    fn test_all_globals() {
        let table = SymbolTable::new();
        table.add_global(Symbol::new("A", SymbolKind::STRUCT, span(0, 10)));
        table.add_global(Symbol::new("B", SymbolKind::ENUM, span(20, 30)));
        table.add_global(Symbol::new("C", SymbolKind::FUNCTION, span(40, 50)));

        let globals = table.all_globals();
        assert_eq!(globals.len(), 3);

        let names: Vec<_> = globals.iter().map(|s| s.name.as_str()).collect();
        assert!(names.contains(&"A"));
        assert!(names.contains(&"B"));
        assert!(names.contains(&"C"));
    }

    #[test]
    fn test_global_overwrite() {
        let table = SymbolTable::new();
        table.add_global(Symbol::new("foo", SymbolKind::VARIABLE, span(0, 5)));
        table.add_global(Symbol::new("foo", SymbolKind::FUNCTION, span(10, 20)));

        // Should have overwritten with the new symbol
        let sym = table.get_global("foo").expect("should exist");
        assert_eq!(sym.kind, SymbolKind::FUNCTION);
        assert_eq!(sym.span.start, 10);
    }

    // SymbolTable local operations tests

    #[test]
    fn test_add_local() {
        let table = SymbolTable::new();
        table.add_local("my_fn", Symbol::new("x", SymbolKind::VARIABLE, span(5, 6)));
        let locals = table.get_locals("my_fn");
        assert_eq!(locals.len(), 1);
        assert_eq!(locals[0].name, "x");
    }

    #[test]
    fn test_get_locals_multiple() {
        let table = SymbolTable::new();
        table.add_local("scope1", Symbol::new("a", SymbolKind::VARIABLE, span(0, 1)));
        table.add_local("scope1", Symbol::new("b", SymbolKind::VARIABLE, span(5, 6)));
        table.add_local("scope2", Symbol::new("c", SymbolKind::VARIABLE, span(10, 11)));

        let scope1 = table.get_locals("scope1");
        assert_eq!(scope1.len(), 2);

        let scope2 = table.get_locals("scope2");
        assert_eq!(scope2.len(), 1);
    }

    #[test]
    fn test_get_locals_empty_scope() {
        let table = SymbolTable::new();
        let locals = table.get_locals("nonexistent");
        assert!(locals.is_empty());
    }

    // Position-based lookup tests

    #[test]
    fn test_find_at_position_exact_start() {
        let table = SymbolTable::new();
        table.add_global(Symbol::new("foo", SymbolKind::FUNCTION, span(10, 20)));
        let result = table.find_at_position(10);
        assert!(result.is_some());
        assert_eq!(result.expect("should find symbol").name, "foo");
    }

    #[test]
    fn test_find_at_position_middle() {
        let table = SymbolTable::new();
        table.add_global(Symbol::new("bar", SymbolKind::STRUCT, span(0, 50)));
        let result = table.find_at_position(25);
        assert!(result.is_some());
        assert_eq!(result.expect("should find symbol").name, "bar");
    }

    #[test]
    fn test_find_at_position_before_end() {
        let table = SymbolTable::new();
        table.add_global(Symbol::new("baz", SymbolKind::VARIABLE, span(5, 10)));
        // Position 9 is inside [5, 10)
        let result = table.find_at_position(9);
        assert!(result.is_some());
    }

    #[test]
    fn test_find_at_position_at_end_excluded() {
        let table = SymbolTable::new();
        table.add_global(Symbol::new("qux", SymbolKind::VARIABLE, span(5, 10)));
        // Position 10 is outside [5, 10)
        let result = table.find_at_position(10);
        assert!(result.is_none());
    }

    #[test]
    fn test_find_at_position_outside() {
        let table = SymbolTable::new();
        table.add_global(Symbol::new("foo", SymbolKind::FUNCTION, span(10, 20)));
        assert!(table.find_at_position(5).is_none());
        assert!(table.find_at_position(25).is_none());
    }

    #[test]
    fn test_find_at_position_in_local() {
        let table = SymbolTable::new();
        table.add_local("my_fn", Symbol::new("local_var", SymbolKind::VARIABLE, span(100, 110)));
        let result = table.find_at_position(105);
        assert!(result.is_some());
        assert_eq!(result.expect("should find local").name, "local_var");
    }

    // Clear tests

    #[test]
    fn test_clear() {
        let table = SymbolTable::new();
        table.add_global(Symbol::new("A", SymbolKind::STRUCT, span(0, 10)));
        table.add_local("fn", Symbol::new("x", SymbolKind::VARIABLE, span(20, 21)));

        assert!(!table.all_globals().is_empty());
        assert!(!table.get_locals("fn").is_empty());

        table.clear();

        assert!(table.all_globals().is_empty());
        assert!(table.get_locals("fn").is_empty());
    }

    // symbol_from_ident test

    #[test]
    fn test_symbol_from_ident() {
        let ident = Identifier {
            name: "my_func".to_string(),
            span: span(50, 57),
        };
        let sym = SymbolTable::symbol_from_ident(&ident, SymbolKind::FUNCTION);
        assert_eq!(sym.name, "my_func");
        assert_eq!(sym.kind, SymbolKind::FUNCTION);
        assert_eq!(sym.span.start, 50);
        assert_eq!(sym.span.end, 57);
    }
}
