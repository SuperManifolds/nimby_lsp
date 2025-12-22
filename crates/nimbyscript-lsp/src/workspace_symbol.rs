//! Workspace symbol search for NimbyScript LSP.
//!
//! Provides symbol search across all open documents for the workspace/symbol request.

use dashmap::DashMap;
use nimbyscript_parser::ast::Span;
use tower_lsp::lsp_types::{SymbolKind, Url};

use crate::document::Document;

/// A symbol match from workspace search.
#[derive(Debug, Clone)]
pub struct WorkspaceSymbolMatch {
    pub name: String,
    pub kind: SymbolKind,
    pub uri: Url,
    pub span: Span,
}

/// Search for symbols across all documents matching a query.
///
/// If query is empty, returns all symbols from all documents.
/// Otherwise, filters symbols by case-insensitive substring match on name.
pub fn search_workspace_symbols(
    documents: &DashMap<Url, Document>,
    query: &str,
) -> Vec<WorkspaceSymbolMatch> {
    let query_lower = query.to_lowercase();
    let mut results = Vec::new();

    for entry in documents {
        let uri = entry.key().clone();
        let doc = entry.value();

        for symbol in doc.document_symbols() {
            // Filter by query (empty query = all symbols)
            if query.is_empty() || symbol.name.to_lowercase().contains(&query_lower) {
                results.push(WorkspaceSymbolMatch {
                    name: symbol.name.clone(),
                    kind: symbol.kind,
                    uri: uri.clone(),
                    span: symbol.span,
                });
            }
        }
    }

    results
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::{make_doc, make_uri};

    #[test]
    fn test_search_empty_query_returns_all() {
        let documents = DashMap::new();

        let source1 = "pub struct Foo extend Signal { }";
        documents.insert(make_uri("file1"), make_doc(source1));

        let source2 = "pub struct Bar extend Signal { }";
        documents.insert(make_uri("file2"), make_doc(source2));

        let results = search_workspace_symbols(&documents, "");

        // Should return symbols from both documents
        assert!(results.len() >= 2);
        let names: Vec<_> = results.iter().map(|r| r.name.as_str()).collect();
        assert!(names.contains(&"Foo"));
        assert!(names.contains(&"Bar"));
    }

    #[test]
    fn test_search_filters_by_query() {
        let documents = DashMap::new();

        let source = r"
pub struct TestStruct extend Signal { }
pub struct OtherThing extend Signal { }
fn test_function() { }
";
        documents.insert(make_uri("file1"), make_doc(source));

        let results = search_workspace_symbols(&documents, "test");

        // Should match TestStruct and test_function but not OtherThing
        let names: Vec<_> = results.iter().map(|r| r.name.as_str()).collect();
        assert!(names.contains(&"TestStruct"));
        assert!(names.contains(&"test_function"));
        assert!(!names.contains(&"OtherThing"));
    }

    #[test]
    fn test_search_case_insensitive() {
        let documents = DashMap::new();

        let source = "pub struct MyStruct extend Signal { }";
        documents.insert(make_uri("file1"), make_doc(source));

        // Lowercase query should match PascalCase name
        let results = search_workspace_symbols(&documents, "mystruct");
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].name, "MyStruct");

        // Uppercase query should also match
        let results = search_workspace_symbols(&documents, "MYSTRUCT");
        assert_eq!(results.len(), 1);
    }

    #[test]
    fn test_search_includes_uri() {
        let documents = DashMap::new();

        let uri = make_uri("testfile");
        let source = "pub struct Foo extend Signal { }";
        documents.insert(uri.clone(), make_doc(source));

        let results = search_workspace_symbols(&documents, "Foo");

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].uri, uri);
    }

    #[test]
    fn test_search_includes_span() {
        let documents = DashMap::new();

        let source = "pub struct Foo extend Signal { }";
        documents.insert(make_uri("file1"), make_doc(source));

        let results = search_workspace_symbols(&documents, "Foo");

        assert_eq!(results.len(), 1);
        // Span should be valid (start < end)
        assert!(results[0].span.start < results[0].span.end);
    }

    #[test]
    fn test_search_different_symbol_kinds() {
        let documents = DashMap::new();

        let source = r"
pub struct MyStruct extend Signal {
    field: i64,
}
enum MyEnum { A, B }
fn my_function() { }
";
        documents.insert(make_uri("file1"), make_doc(source));

        let results = search_workspace_symbols(&documents, "My");

        // Should find struct, enum, and function
        let kinds: Vec<_> = results.iter().map(|r| r.kind).collect();
        assert!(kinds.contains(&SymbolKind::STRUCT));
        assert!(kinds.contains(&SymbolKind::ENUM));
        assert!(kinds.contains(&SymbolKind::FUNCTION));
    }

    #[test]
    fn test_search_no_results() {
        let documents = DashMap::new();

        let source = "pub struct Foo extend Signal { }";
        documents.insert(make_uri("file1"), make_doc(source));

        let results = search_workspace_symbols(&documents, "nonexistent");

        assert!(results.is_empty());
    }

    #[test]
    fn test_search_empty_documents() {
        let documents: DashMap<Url, Document> = DashMap::new();

        let results = search_workspace_symbols(&documents, "anything");

        assert!(results.is_empty());
    }
}
