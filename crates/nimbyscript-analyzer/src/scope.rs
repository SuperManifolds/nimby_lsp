//! Scope management for semantic analysis.
//!
//! Provides hierarchical scope tracking for name resolution, variable binding,
//! and usage analysis in NimbyScript.

use nimbyscript_parser::ast::Span;
use std::collections::HashMap;

use crate::types::TypeInfo;

/// Unique identifier for a scope
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub usize);

/// Unique identifier for a symbol
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub usize);

/// The kind of scope
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScopeKind {
    /// Global/module scope (top-level definitions)
    Global,
    /// Struct scope (for fields)
    Struct { name: String },
    /// Enum scope (for variants)
    Enum { name: String },
    /// Function scope
    Function {
        name: String,
        return_type: Option<TypeInfo>,
    },
    /// Block scope (e.g., if body, else body)
    Block,
    /// Loop scope (for break/continue tracking)
    Loop,
}

/// The kind of symbol
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Variable,
    Parameter,
    Constant,
    Struct,
    Enum,
    EnumVariant,
    Field,
    Function,
    Method,
}

/// A symbol in the scope
#[derive(Debug, Clone)]
pub struct ScopedSymbol {
    pub id: SymbolId,
    pub name: String,
    pub kind: SymbolKind,
    pub type_info: TypeInfo,
    /// Full span of the symbol definition
    pub span: Span,
    /// Span of just the identifier (for precise error highlighting)
    pub name_span: Span,
    pub is_mutable: bool,
    pub is_used: bool,
    pub scope_id: ScopeId,
}

/// A scope in the scope chain
#[derive(Debug)]
pub struct Scope {
    pub id: ScopeId,
    pub kind: ScopeKind,
    pub parent: Option<ScopeId>,
    /// Map from symbol name to symbol ID
    symbols: HashMap<String, SymbolId>,
    pub span: Span,
}

/// Error when defining a symbol
#[derive(Debug, Clone)]
pub enum DefineError {
    /// Symbol already exists in this scope
    AlreadyDefined {
        name: String,
        existing_span: Span,
    },
}

/// Manages hierarchical scopes for semantic analysis
#[derive(Debug)]
pub struct ScopeManager {
    scopes: Vec<Scope>,
    symbols: Vec<ScopedSymbol>,
    current_scope: ScopeId,
}

impl Default for ScopeManager {
    fn default() -> Self {
        Self::new()
    }
}

impl ScopeManager {
    /// Create a new scope manager with a global scope
    pub fn new() -> Self {
        let global_scope = Scope {
            id: ScopeId(0),
            kind: ScopeKind::Global,
            parent: None,
            symbols: HashMap::new(),
            span: Span::new(0, 0),
        };

        Self {
            scopes: vec![global_scope],
            symbols: Vec::new(),
            current_scope: ScopeId(0),
        }
    }

    /// Enter a new scope
    pub fn enter_scope(&mut self, kind: ScopeKind, span: Span) -> ScopeId {
        let id = ScopeId(self.scopes.len());
        let scope = Scope {
            id,
            kind,
            parent: Some(self.current_scope),
            symbols: HashMap::new(),
            span,
        };
        self.scopes.push(scope);
        self.current_scope = id;
        id
    }

    /// Exit the current scope and return to the parent
    pub fn exit_scope(&mut self) -> Option<ScopeId> {
        let current = &self.scopes[self.current_scope.0];
        if let Some(parent) = current.parent {
            self.current_scope = parent;
            Some(parent)
        } else {
            None // Can't exit global scope
        }
    }

    /// Get the current scope ID
    pub fn current_scope_id(&self) -> ScopeId {
        self.current_scope
    }

    /// Get the current scope kind
    pub fn current_scope_kind(&self) -> &ScopeKind {
        &self.scopes[self.current_scope.0].kind
    }

    /// Define a symbol in the current scope
    pub fn define(
        &mut self,
        name: &str,
        kind: SymbolKind,
        type_info: TypeInfo,
        span: Span,
        name_span: Span,
        is_mutable: bool,
    ) -> Result<SymbolId, DefineError> {
        // Check if symbol already exists in current scope
        if let Some(&existing_id) = self.scopes[self.current_scope.0].symbols.get(name) {
            let existing = &self.symbols[existing_id.0];
            return Err(DefineError::AlreadyDefined {
                name: name.to_string(),
                existing_span: existing.name_span,
            });
        }

        let id = SymbolId(self.symbols.len());
        let symbol = ScopedSymbol {
            id,
            name: name.to_string(),
            kind,
            type_info,
            span,
            name_span,
            is_mutable,
            is_used: false,
            scope_id: self.current_scope,
        };

        self.symbols.push(symbol);
        self.scopes[self.current_scope.0]
            .symbols
            .insert(name.to_string(), id);

        Ok(id)
    }

    /// Define a symbol in the global scope (for top-level definitions)
    pub fn define_global(
        &mut self,
        name: &str,
        kind: SymbolKind,
        type_info: TypeInfo,
        span: Span,
        name_span: Span,
    ) -> Result<SymbolId, DefineError> {
        // Check if symbol already exists in global scope
        if let Some(&existing_id) = self.scopes[0].symbols.get(name) {
            let existing = &self.symbols[existing_id.0];
            return Err(DefineError::AlreadyDefined {
                name: name.to_string(),
                existing_span: existing.name_span,
            });
        }

        let id = SymbolId(self.symbols.len());
        let symbol = ScopedSymbol {
            id,
            name: name.to_string(),
            kind,
            type_info,
            span,
            name_span,
            is_mutable: false,
            is_used: false,
            scope_id: ScopeId(0),
        };

        self.symbols.push(symbol);
        self.scopes[0].symbols.insert(name.to_string(), id);

        Ok(id)
    }

    /// Look up a symbol by name, searching up the scope chain
    pub fn lookup(&self, name: &str) -> Option<&ScopedSymbol> {
        let mut scope_id = self.current_scope;

        loop {
            let scope = &self.scopes[scope_id.0];
            if let Some(&symbol_id) = scope.symbols.get(name) {
                return Some(&self.symbols[symbol_id.0]);
            }

            match scope.parent {
                Some(parent) => scope_id = parent,
                None => return None,
            }
        }
    }

    /// Look up a symbol in the current scope only (for shadowing detection)
    pub fn lookup_local(&self, name: &str) -> Option<&ScopedSymbol> {
        let scope = &self.scopes[self.current_scope.0];
        scope
            .symbols
            .get(name)
            .map(|&id| &self.symbols[id.0])
    }

    /// Look up a symbol in the global scope only
    pub fn lookup_global(&self, name: &str) -> Option<&ScopedSymbol> {
        self.scopes[0]
            .symbols
            .get(name)
            .map(|&id| &self.symbols[id.0])
    }

    /// Get a symbol by ID
    pub fn get_symbol(&self, id: SymbolId) -> Option<&ScopedSymbol> {
        self.symbols.get(id.0)
    }

    /// Get a mutable symbol by ID
    pub fn get_symbol_mut(&mut self, id: SymbolId) -> Option<&mut ScopedSymbol> {
        self.symbols.get_mut(id.0)
    }

    /// Mark a symbol as used
    pub fn mark_used(&mut self, id: SymbolId) {
        if let Some(symbol) = self.symbols.get_mut(id.0) {
            symbol.is_used = true;
        }
    }

    /// Mark a symbol as used by name (searches scope chain)
    pub fn mark_used_by_name(&mut self, name: &str) {
        if let Some(symbol) = self.lookup(name) {
            let id = symbol.id;
            self.mark_used(id);
        }
    }

    /// Check if currently inside a loop
    pub fn in_loop(&self) -> bool {
        let mut scope_id = self.current_scope;

        loop {
            let scope = &self.scopes[scope_id.0];
            if matches!(scope.kind, ScopeKind::Loop) {
                return true;
            }

            match scope.parent {
                Some(parent) => scope_id = parent,
                None => return false,
            }
        }
    }

    /// Get the return type of the enclosing function, if any
    pub fn enclosing_return_type(&self) -> Option<&TypeInfo> {
        let mut scope_id = self.current_scope;

        loop {
            let scope = &self.scopes[scope_id.0];
            if let ScopeKind::Function { return_type, .. } = &scope.kind {
                return return_type.as_ref();
            }

            match scope.parent {
                Some(parent) => scope_id = parent,
                None => return None,
            }
        }
    }

    /// Get the name of the enclosing function, if any
    pub fn enclosing_function_name(&self) -> Option<&str> {
        let mut scope_id = self.current_scope;

        loop {
            let scope = &self.scopes[scope_id.0];
            if let ScopeKind::Function { name, .. } = &scope.kind {
                return Some(name.as_str());
            }

            match scope.parent {
                Some(parent) => scope_id = parent,
                None => return None,
            }
        }
    }

    /// Check if we're inside a function
    pub fn in_function(&self) -> bool {
        self.enclosing_function_name().is_some()
    }

    /// Get all unused symbols (for warnings)
    pub fn unused_symbols(&self) -> Vec<&ScopedSymbol> {
        self.symbols
            .iter()
            .filter(|s| !s.is_used)
            .collect()
    }

    /// Get all symbols in the global scope
    pub fn global_symbols(&self) -> Vec<&ScopedSymbol> {
        self.scopes[0]
            .symbols
            .values()
            .map(|&id| &self.symbols[id.0])
            .collect()
    }

    /// Get all symbols in the current scope
    pub fn current_scope_symbols(&self) -> Vec<&ScopedSymbol> {
        self.scopes[self.current_scope.0]
            .symbols
            .values()
            .map(|&id| &self.symbols[id.0])
            .collect()
    }

    /// Check if a name exists in the scope chain (for shadowing checks)
    pub fn shadows_existing(&self, name: &str) -> Option<&ScopedSymbol> {
        // Only check parent scopes, not current scope
        let current = &self.scopes[self.current_scope.0];
        let mut scope_id = current.parent?;

        loop {
            let scope = &self.scopes[scope_id.0];
            if let Some(&symbol_id) = scope.symbols.get(name) {
                return Some(&self.symbols[symbol_id.0]);
            }

            match scope.parent {
                Some(parent) => scope_id = parent,
                None => return None,
            }
        }
    }

    /// Get the depth of the current scope (0 = global)
    pub fn scope_depth(&self) -> usize {
        let mut depth = 0;
        let mut scope_id = self.current_scope;

        while let Some(parent) = self.scopes[scope_id.0].parent {
            depth += 1;
            scope_id = parent;
        }

        depth
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn span(start: usize, end: usize) -> Span {
        Span::new(start, end)
    }

    // Basic scope tests

    #[test]
    fn test_new_scope_manager() {
        let mgr = ScopeManager::new();
        assert_eq!(mgr.current_scope_id(), ScopeId(0));
        assert!(matches!(mgr.current_scope_kind(), ScopeKind::Global));
    }

    #[test]
    fn test_enter_exit_scope() {
        let mut mgr = ScopeManager::new();

        let func_scope = mgr.enter_scope(
            ScopeKind::Function {
                name: "test".to_string(),
                return_type: None,
            },
            span(0, 100),
        );
        assert_eq!(mgr.current_scope_id(), func_scope);
        assert_eq!(mgr.scope_depth(), 1);

        let block_scope = mgr.enter_scope(ScopeKind::Block, span(10, 90));
        assert_eq!(mgr.current_scope_id(), block_scope);
        assert_eq!(mgr.scope_depth(), 2);

        mgr.exit_scope();
        assert_eq!(mgr.current_scope_id(), func_scope);

        mgr.exit_scope();
        assert_eq!(mgr.current_scope_id(), ScopeId(0));
    }

    #[test]
    fn test_cannot_exit_global() {
        let mut mgr = ScopeManager::new();
        assert!(mgr.exit_scope().is_none());
    }

    // Symbol definition tests

    #[test]
    fn test_define_symbol() {
        let mut mgr = ScopeManager::new();
        let id = mgr
            .define(
                "x",
                SymbolKind::Variable,
                TypeInfo::I64,
                span(0, 10),
                span(4, 5),
                false,
            )
            .expect("should succeed");

        let sym = mgr.get_symbol(id).expect("should find");
        assert_eq!(sym.name, "x");
        assert_eq!(sym.type_info, TypeInfo::I64);
        assert!(!sym.is_mutable);
        assert!(!sym.is_used);
    }

    #[test]
    fn test_define_duplicate_error() {
        let mut mgr = ScopeManager::new();
        mgr.define(
            "x",
            SymbolKind::Variable,
            TypeInfo::I64,
            span(0, 10),
            span(4, 5),
            false,
        )
        .expect("first should succeed");

        let result = mgr.define(
            "x",
            SymbolKind::Variable,
            TypeInfo::F64,
            span(20, 30),
            span(24, 25),
            false,
        );

        assert!(matches!(result, Err(DefineError::AlreadyDefined { .. })));
    }

    #[test]
    fn test_define_in_different_scopes() {
        let mut mgr = ScopeManager::new();

        // Define x in global
        mgr.define(
            "x",
            SymbolKind::Variable,
            TypeInfo::I64,
            span(0, 10),
            span(4, 5),
            false,
        )
        .expect("should succeed");

        // Enter function scope
        mgr.enter_scope(
            ScopeKind::Function {
                name: "test".to_string(),
                return_type: None,
            },
            span(20, 100),
        );

        // Define x again in function scope (shadowing)
        let result = mgr.define(
            "x",
            SymbolKind::Variable,
            TypeInfo::F64,
            span(25, 35),
            span(29, 30),
            false,
        );
        assert!(result.is_ok());
    }

    // Lookup tests

    #[test]
    fn test_lookup_in_current_scope() {
        let mut mgr = ScopeManager::new();
        mgr.define(
            "x",
            SymbolKind::Variable,
            TypeInfo::I64,
            span(0, 10),
            span(4, 5),
            false,
        )
        .expect("should succeed");

        let sym = mgr.lookup("x").expect("should find");
        assert_eq!(sym.name, "x");
    }

    #[test]
    fn test_lookup_in_parent_scope() {
        let mut mgr = ScopeManager::new();
        mgr.define(
            "global_var",
            SymbolKind::Variable,
            TypeInfo::I64,
            span(0, 10),
            span(4, 14),
            false,
        )
        .expect("should succeed");

        mgr.enter_scope(
            ScopeKind::Function {
                name: "test".to_string(),
                return_type: None,
            },
            span(20, 100),
        );

        // Should still find global_var from function scope
        let sym = mgr.lookup("global_var").expect("should find");
        assert_eq!(sym.name, "global_var");
    }

    #[test]
    fn test_lookup_not_found() {
        let mgr = ScopeManager::new();
        assert!(mgr.lookup("nonexistent").is_none());
    }

    #[test]
    fn test_lookup_local_only() {
        let mut mgr = ScopeManager::new();
        mgr.define(
            "x",
            SymbolKind::Variable,
            TypeInfo::I64,
            span(0, 10),
            span(4, 5),
            false,
        )
        .expect("should succeed");

        mgr.enter_scope(ScopeKind::Block, span(20, 100));

        // lookup_local should NOT find parent scope symbols
        assert!(mgr.lookup_local("x").is_none());

        // But regular lookup should
        assert!(mgr.lookup("x").is_some());
    }

    // Usage tracking tests

    #[test]
    fn test_mark_used() {
        let mut mgr = ScopeManager::new();
        let id = mgr
            .define(
                "x",
                SymbolKind::Variable,
                TypeInfo::I64,
                span(0, 10),
                span(4, 5),
                false,
            )
            .expect("should succeed");

        assert!(!mgr.get_symbol(id).expect("exists").is_used);

        mgr.mark_used(id);

        assert!(mgr.get_symbol(id).expect("exists").is_used);
    }

    #[test]
    fn test_mark_used_by_name() {
        let mut mgr = ScopeManager::new();
        mgr.define(
            "x",
            SymbolKind::Variable,
            TypeInfo::I64,
            span(0, 10),
            span(4, 5),
            false,
        )
        .expect("should succeed");

        mgr.enter_scope(ScopeKind::Block, span(20, 100));
        mgr.mark_used_by_name("x");

        // Exit and check
        mgr.exit_scope();
        let sym = mgr.lookup("x").expect("should find");
        assert!(sym.is_used);
    }

    #[test]
    fn test_unused_symbols() {
        let mut mgr = ScopeManager::new();
        mgr.define(
            "used",
            SymbolKind::Variable,
            TypeInfo::I64,
            span(0, 10),
            span(4, 8),
            false,
        )
        .expect("should succeed");
        mgr.define(
            "unused",
            SymbolKind::Variable,
            TypeInfo::I64,
            span(15, 25),
            span(19, 25),
            false,
        )
        .expect("should succeed");

        mgr.mark_used_by_name("used");

        let unused = mgr.unused_symbols();
        assert_eq!(unused.len(), 1);
        assert_eq!(unused[0].name, "unused");
    }

    // Loop/function context tests

    #[test]
    fn test_in_loop() {
        let mut mgr = ScopeManager::new();
        assert!(!mgr.in_loop());

        mgr.enter_scope(
            ScopeKind::Function {
                name: "test".to_string(),
                return_type: None,
            },
            span(0, 100),
        );
        assert!(!mgr.in_loop());

        mgr.enter_scope(ScopeKind::Loop, span(10, 90));
        assert!(mgr.in_loop());

        mgr.enter_scope(ScopeKind::Block, span(20, 80));
        // Still in loop even inside a block
        assert!(mgr.in_loop());
    }

    #[test]
    fn test_enclosing_return_type() {
        let mut mgr = ScopeManager::new();
        assert!(mgr.enclosing_return_type().is_none());

        mgr.enter_scope(
            ScopeKind::Function {
                name: "test".to_string(),
                return_type: Some(TypeInfo::I64),
            },
            span(0, 100),
        );
        assert_eq!(mgr.enclosing_return_type(), Some(&TypeInfo::I64));

        mgr.enter_scope(ScopeKind::Block, span(10, 90));
        // Still should find the function return type
        assert_eq!(mgr.enclosing_return_type(), Some(&TypeInfo::I64));
    }

    #[test]
    fn test_enclosing_function_name() {
        let mut mgr = ScopeManager::new();
        assert!(mgr.enclosing_function_name().is_none());

        mgr.enter_scope(
            ScopeKind::Function {
                name: "my_func".to_string(),
                return_type: None,
            },
            span(0, 100),
        );
        assert_eq!(mgr.enclosing_function_name(), Some("my_func"));
    }

    #[test]
    fn test_in_function() {
        let mut mgr = ScopeManager::new();
        assert!(!mgr.in_function());

        mgr.enter_scope(
            ScopeKind::Function {
                name: "test".to_string(),
                return_type: None,
            },
            span(0, 100),
        );
        assert!(mgr.in_function());
    }

    // Shadowing tests

    #[test]
    fn test_shadows_existing() {
        let mut mgr = ScopeManager::new();
        mgr.define(
            "x",
            SymbolKind::Variable,
            TypeInfo::I64,
            span(0, 10),
            span(4, 5),
            false,
        )
        .expect("should succeed");

        mgr.enter_scope(ScopeKind::Block, span(20, 100));

        let shadowed = mgr.shadows_existing("x");
        assert!(shadowed.is_some());
        assert_eq!(shadowed.expect("exists").type_info, TypeInfo::I64);

        assert!(mgr.shadows_existing("nonexistent").is_none());
    }

    // Global definition tests

    #[test]
    fn test_define_global() {
        let mut mgr = ScopeManager::new();

        // Enter a function scope
        mgr.enter_scope(
            ScopeKind::Function {
                name: "test".to_string(),
                return_type: None,
            },
            span(0, 100),
        );

        // Define a global from inside a function
        mgr.define_global(
            "CONST",
            SymbolKind::Constant,
            TypeInfo::I64,
            span(10, 30),
            span(16, 21),
        )
        .expect("should succeed");

        // Should be findable via lookup_global
        let sym = mgr.lookup_global("CONST").expect("should find");
        assert_eq!(sym.name, "CONST");
    }

    // Collection tests

    #[test]
    fn test_global_symbols() {
        let mut mgr = ScopeManager::new();
        mgr.define_global(
            "A",
            SymbolKind::Struct,
            TypeInfo::struct_type("A"),
            span(0, 10),
            span(7, 8),
        )
        .expect("should succeed");
        mgr.define_global(
            "B",
            SymbolKind::Struct,
            TypeInfo::struct_type("B"),
            span(15, 25),
            span(22, 23),
        )
        .expect("should succeed");

        let globals = mgr.global_symbols();
        assert_eq!(globals.len(), 2);
    }

    #[test]
    fn test_current_scope_symbols() {
        let mut mgr = ScopeManager::new();
        mgr.define(
            "global",
            SymbolKind::Variable,
            TypeInfo::I64,
            span(0, 10),
            span(4, 10),
            false,
        )
        .expect("should succeed");

        mgr.enter_scope(ScopeKind::Block, span(20, 100));
        mgr.define(
            "local",
            SymbolKind::Variable,
            TypeInfo::F64,
            span(25, 35),
            span(29, 34),
            false,
        )
        .expect("should succeed");

        let current = mgr.current_scope_symbols();
        assert_eq!(current.len(), 1);
        assert_eq!(current[0].name, "local");
    }
}
