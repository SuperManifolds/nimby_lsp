use std::collections::HashSet;

use tower_lsp::lsp_types::*;

use nimbyscript_analyzer::ApiDefinitions;
use nimbyscript_parser::{parse, kind, Node, NodeExt};

use crate::document::Document;

/// Define the semantic token types we support
pub const TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::KEYWORD,
    SemanticTokenType::TYPE,
    SemanticTokenType::FUNCTION,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::PROPERTY,
    SemanticTokenType::STRING,
    SemanticTokenType::NUMBER,
    SemanticTokenType::COMMENT,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::PARAMETER,
    SemanticTokenType::ENUM_MEMBER,
    SemanticTokenType::STRUCT,
    SemanticTokenType::ENUM,
];

/// Define semantic token modifiers
pub const TOKEN_MODIFIERS: &[SemanticTokenModifier] = &[
    SemanticTokenModifier::DECLARATION,
    SemanticTokenModifier::DEFINITION,
    SemanticTokenModifier::READONLY,
    SemanticTokenModifier::STATIC,
    SemanticTokenModifier::DEFAULT_LIBRARY,
];

fn token_type_index(tt: &SemanticTokenType) -> u32 {
    TOKEN_TYPES.iter().position(|t| t == tt).map_or(0, |i| i as u32)
}

fn modifier_bitset(modifiers: &[SemanticTokenModifier]) -> u32 {
    modifiers.iter().fold(0, |bits, m| {
        bits | TOKEN_MODIFIERS.iter().position(|t| t == m).map_or(0, |i| 1 << i)
    })
}

// Static modifier slices for returning from functions
const MOD_NONE: &[SemanticTokenModifier] = &[];
const MOD_DEFAULT_LIBRARY: &[SemanticTokenModifier] = &[SemanticTokenModifier::DEFAULT_LIBRARY];

pub fn semantic_token_legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: TOKEN_TYPES.to_vec(),
        token_modifiers: TOKEN_MODIFIERS.to_vec(),
    }
}

pub fn compute_semantic_tokens(doc: &Document, api: &ApiDefinitions) -> Vec<SemanticToken> {
    let content = &doc.content;
    let tree = parse(content);
    let mut collector = TokenCollector::new(content, api);
    collector.collect_tokens(tree.root_node());
    collector.into_tokens()
}

struct TokenCollector<'a> {
    content: &'a str,
    raw_tokens: Vec<RawToken>,
    game_types: HashSet<&'a str>,
    game_enums: HashSet<&'a str>,
    game_functions: HashSet<&'a str>,
    game_modules: HashSet<&'a str>,
}

struct RawToken {
    start: usize,
    length: usize,
    token_type: SemanticTokenType,
    modifiers: Vec<SemanticTokenModifier>,
}

impl<'a> TokenCollector<'a> {
    fn new(content: &'a str, api: &'a ApiDefinitions) -> Self {
        Self {
            content,
            raw_tokens: Vec::new(),
            game_types: api.type_names().collect(),
            game_enums: api.enum_names().collect(),
            game_functions: api.function_names().collect(),
            game_modules: api.module_names().collect(),
        }
    }

    fn add_token(&mut self, start: usize, end: usize, token_type: SemanticTokenType, modifiers: &[SemanticTokenModifier]) {
        self.raw_tokens.push(RawToken {
            start,
            length: end - start,
            token_type,
            modifiers: modifiers.to_vec(),
        });
    }

    fn collect_tokens(&mut self, node: Node) {
        let start = node.start_byte();
        let end = node.end_byte();

        match node.kind() {
            // Keywords (including boolean literals true/false)
            kind::VISIBILITY_MODIFIER
            | kind::STORAGE_MODIFIER
            | kind::MUTABILITY_MODIFIER
            | kind::BOOLEAN => {
                self.add_token(start, end, SemanticTokenType::KEYWORD, &[]);
            }

            // Comments
            kind::COMMENT => {
                self.add_token(start, end, SemanticTokenType::COMMENT, &[]);
            }

            // Type definitions
            kind::STRUCT_DEFINITION => {
                self.collect_struct_tokens(node);
            }
            kind::ENUM_DEFINITION => {
                self.collect_enum_tokens(node);
            }

            // Function definitions
            kind::FUNCTION_DEFINITION => {
                self.collect_function_tokens(node);
            }

            // Type references
            kind::TYPE_IDENTIFIER => {
                self.collect_type_tokens(node);
            }

            // Literals
            kind::NUMBER | kind::TIME_LITERAL => {
                self.add_token(start, end, SemanticTokenType::NUMBER, &[]);
            }
            kind::STRING_LITERAL => {
                self.add_token(start, end, SemanticTokenType::STRING, &[]);
            }

            // Function calls
            kind::CALL_EXPRESSION => {
                self.collect_call_tokens(node);
            }

            // Path expressions (could be enum variants or module access)
            kind::PATH_EXPRESSION => {
                self.collect_path_tokens(node);
            }

            // Let bindings
            kind::LET_STATEMENT | kind::LET_ELSE_STATEMENT => {
                if let Some(binding) = node.child_by_kind("binding") {
                    if let Some(name_node) = binding.child_by_field("name") {
                        self.add_token(name_node.start_byte(), name_node.end_byte(), SemanticTokenType::VARIABLE, &[SemanticTokenModifier::DECLARATION]);
                    }
                }
                // Recurse for children
                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    self.collect_tokens(child);
                }
            }

            // Parameters
            kind::PARAMETER => {
                if let Some(name_node) = node.child_by_field("name") {
                    self.add_token(name_node.start_byte(), name_node.end_byte(), SemanticTokenType::PARAMETER, &[SemanticTokenModifier::DECLARATION]);
                }
                // Recurse for type
                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    self.collect_tokens(child);
                }
            }

            // Recurse into other nodes
            _ => {
                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    self.collect_tokens(child);
                }
            }
        }
    }

    fn game_type_modifiers(&self, name: &str) -> &'static [SemanticTokenModifier] {
        if self.game_types.contains(name) { MOD_DEFAULT_LIBRARY } else { MOD_NONE }
    }

    fn collect_struct_tokens(&mut self, node: Node) {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            match child.kind() {
                kind::VISIBILITY_MODIFIER => {
                    self.add_token(child.start_byte(), child.end_byte(), SemanticTokenType::KEYWORD, &[]);
                }
                kind::IDENTIFIER => {
                    // Struct name
                    if node.child_by_field("name").map(|n| n.id()) == Some(child.id()) {
                        self.add_token(child.start_byte(), child.end_byte(), SemanticTokenType::STRUCT, &[SemanticTokenModifier::DEFINITION]);
                    }
                }
                kind::EXTENDS_CLAUSE => {
                    if let Some(type_node) = child.child_by_field("type") {
                        let modifiers = self.game_type_modifiers(type_node.text(self.content));
                        self.add_token(type_node.start_byte(), type_node.end_byte(), SemanticTokenType::TYPE, modifiers);
                    }
                }
                kind::STRUCT_FIELD => {
                    if let Some(name_node) = child.child_by_field("name") {
                        self.add_token(name_node.start_byte(), name_node.end_byte(), SemanticTokenType::PROPERTY, &[SemanticTokenModifier::DEFINITION]);
                    }
                    // Recurse for type
                    self.collect_tokens(child);
                }
                _ => {
                    self.collect_tokens(child);
                }
            }
        }
    }

    fn collect_enum_tokens(&mut self, node: Node) {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            match child.kind() {
                kind::VISIBILITY_MODIFIER => {
                    self.add_token(child.start_byte(), child.end_byte(), SemanticTokenType::KEYWORD, &[]);
                }
                kind::IDENTIFIER => {
                    // Enum name
                    if node.child_by_field("name").map(|n| n.id()) == Some(child.id()) {
                        self.add_token(child.start_byte(), child.end_byte(), SemanticTokenType::ENUM, &[SemanticTokenModifier::DEFINITION]);
                    }
                }
                kind::ENUM_VARIANT => {
                    if let Some(name_node) = child.child_by_field("name") {
                        self.add_token(name_node.start_byte(), name_node.end_byte(), SemanticTokenType::ENUM_MEMBER, &[SemanticTokenModifier::DEFINITION]);
                    }
                }
                _ => {
                    self.collect_tokens(child);
                }
            }
        }
    }

    fn collect_function_tokens(&mut self, node: Node) {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            match child.kind() {
                kind::VISIBILITY_MODIFIER => {
                    self.add_token(child.start_byte(), child.end_byte(), SemanticTokenType::KEYWORD, &[]);
                }
                kind::FUNCTION_NAME => {
                    self.tokenize_function_name(child);
                }
                _ => {
                    self.collect_tokens(child);
                }
            }
        }
    }

    fn tokenize_function_name(&mut self, child: Node) {
        let text = child.text(self.content);
        let Some(sep_pos) = text.find("::") else {
            self.add_token(child.start_byte(), child.end_byte(), SemanticTokenType::FUNCTION, &[SemanticTokenModifier::DEFINITION]);
            return;
        };

        let pos = child.start_byte();
        let type_name = &text[..sep_pos];
        let method_name = &text[sep_pos + 2..];
        self.add_token(pos, pos + type_name.len(), SemanticTokenType::TYPE, &[]);
        let method_start = pos + type_name.len() + 2;
        self.add_token(method_start, method_start + method_name.len(), SemanticTokenType::FUNCTION, &[SemanticTokenModifier::DEFINITION]);
    }

    fn collect_type_tokens(&mut self, node: Node) {
        // Get the first identifier in the type
        if let Some(id_node) = node.child_by_kind(kind::IDENTIFIER) {
            let name = id_node.text(self.content);
            let (tok_type, modifiers): (SemanticTokenType, &[SemanticTokenModifier]) = if self.game_types.contains(name) {
                (SemanticTokenType::TYPE, &[SemanticTokenModifier::DEFAULT_LIBRARY])
            } else if self.game_enums.contains(name) {
                (SemanticTokenType::ENUM, &[SemanticTokenModifier::DEFAULT_LIBRARY])
            } else {
                (SemanticTokenType::TYPE, &[])
            };
            self.add_token(id_node.start_byte(), id_node.end_byte(), tok_type, modifiers);
        }

        // Recurse for nested types (generics)
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == kind::TYPE_IDENTIFIER {
                self.collect_type_tokens(child);
            }
        }
    }

    fn collect_call_tokens(&mut self, node: Node) {
        if let Some(func_node) = node.child_by_field("function") {
            let text = func_node.text(self.content);
            if text.contains("::") {
                self.collect_path_tokens(func_node);
            } else {
                // Simple function call
                let modifiers: &[SemanticTokenModifier] = if self.game_functions.contains(text) {
                    &[SemanticTokenModifier::DEFAULT_LIBRARY]
                } else {
                    &[]
                };
                self.add_token(func_node.start_byte(), func_node.end_byte(), SemanticTokenType::FUNCTION, modifiers);
            }
        }

        // Recurse for arguments
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() != kind::PATH_EXPRESSION {
                self.collect_tokens(child);
            }
        }
    }

    fn path_prefix_token(&self, part: &str) -> (SemanticTokenType, &'static [SemanticTokenModifier]) {
        if self.game_types.contains(part) {
            (SemanticTokenType::TYPE, MOD_DEFAULT_LIBRARY)
        } else if self.game_enums.contains(part) {
            (SemanticTokenType::ENUM, MOD_DEFAULT_LIBRARY)
        } else if self.game_modules.contains(part) {
            (SemanticTokenType::TYPE, MOD_DEFAULT_LIBRARY)
        } else {
            (SemanticTokenType::TYPE, MOD_NONE)
        }
    }

    fn collect_path_tokens(&mut self, node: Node) {
        let text = node.text(self.content);
        if !text.contains("::") {
            self.add_token(node.start_byte(), node.end_byte(), SemanticTokenType::VARIABLE, &[]);
            return;
        }

        let parts: Vec<&str> = text.split("::").collect();
        let mut pos = node.start_byte();
        for (i, part) in parts.iter().enumerate() {
            let is_last = i == parts.len() - 1;
            let (tok_type, modifiers): (SemanticTokenType, &[SemanticTokenModifier]) = if is_last {
                (SemanticTokenType::ENUM_MEMBER, &[])
            } else {
                self.path_prefix_token(part)
            };
            self.add_token(pos, pos + part.len(), tok_type, modifiers);
            pos += part.len() + 2;
        }
    }

    fn into_tokens(mut self) -> Vec<SemanticToken> {
        // Sort by position
        self.raw_tokens.sort_by_key(|t| t.start);

        // Convert to delta-encoded format
        let mut result = Vec::new();
        let mut prev_line = 0u32;
        let mut prev_char = 0u32;

        for token in &self.raw_tokens {
            let (line, char) = self.offset_to_line_char(token.start);

            let delta_line = line - prev_line;
            let delta_start = if delta_line == 0 {
                char - prev_char
            } else {
                char
            };

            result.push(SemanticToken {
                delta_line,
                delta_start,
                length: token.length as u32,
                token_type: token_type_index(&token.token_type),
                token_modifiers_bitset: modifier_bitset(&token.modifiers),
            });

            prev_line = line;
            prev_char = char;
        }

        result
    }

    fn offset_to_line_char(&self, offset: usize) -> (u32, u32) {
        let mut line = 0u32;
        let mut line_start = 0usize;

        for (i, c) in self.content.char_indices() {
            if i >= offset {
                break;
            }
            if c == '\n' {
                line += 1;
                line_start = i + 1;
            }
        }

        let char = (offset - line_start) as u32;
        (line, char)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_api() -> ApiDefinitions {
        let toml_content = include_str!("../../../api-definitions/nimbyrails.v1.toml");
        ApiDefinitions::load_from_str(toml_content).expect("Failed to parse API")
    }

    // Token legend tests

    #[test]
    fn test_semantic_token_legend_types() {
        let legend = semantic_token_legend();
        assert!(!legend.token_types.is_empty());
        assert!(legend.token_types.contains(&SemanticTokenType::KEYWORD));
        assert!(legend.token_types.contains(&SemanticTokenType::TYPE));
        assert!(legend.token_types.contains(&SemanticTokenType::FUNCTION));
        assert!(legend.token_types.contains(&SemanticTokenType::VARIABLE));
        assert!(legend.token_types.contains(&SemanticTokenType::COMMENT));
        assert!(legend.token_types.contains(&SemanticTokenType::STRING));
        assert!(legend.token_types.contains(&SemanticTokenType::NUMBER));
    }

    #[test]
    fn test_semantic_token_legend_modifiers() {
        let legend = semantic_token_legend();
        assert!(!legend.token_modifiers.is_empty());
        assert!(legend.token_modifiers.contains(&SemanticTokenModifier::DECLARATION));
        assert!(legend.token_modifiers.contains(&SemanticTokenModifier::DEFINITION));
        assert!(legend.token_modifiers.contains(&SemanticTokenModifier::DEFAULT_LIBRARY));
    }

    // Token type index tests

    #[test]
    fn test_token_type_index_keyword() {
        let idx = token_type_index(&SemanticTokenType::KEYWORD);
        assert_eq!(idx, 0);
    }

    #[test]
    fn test_token_type_index_type() {
        let idx = token_type_index(&SemanticTokenType::TYPE);
        assert_eq!(idx, 1);
    }

    #[test]
    fn test_token_type_index_function() {
        let idx = token_type_index(&SemanticTokenType::FUNCTION);
        assert_eq!(idx, 2);
    }

    #[test]
    fn test_token_type_index_unknown() {
        // Unknown types default to 0
        let idx = token_type_index(&SemanticTokenType::new("unknown"));
        assert_eq!(idx, 0);
    }

    // Modifier bitset tests

    #[test]
    fn test_modifier_bitset_empty() {
        let bits = modifier_bitset(&[]);
        assert_eq!(bits, 0);
    }

    #[test]
    fn test_modifier_bitset_declaration() {
        let bits = modifier_bitset(&[SemanticTokenModifier::DECLARATION]);
        assert_eq!(bits, 1); // First modifier, bit 0
    }

    #[test]
    fn test_modifier_bitset_definition() {
        let bits = modifier_bitset(&[SemanticTokenModifier::DEFINITION]);
        assert_eq!(bits, 2); // Second modifier, bit 1
    }

    #[test]
    fn test_modifier_bitset_multiple() {
        let bits = modifier_bitset(&[
            SemanticTokenModifier::DECLARATION,
            SemanticTokenModifier::DEFINITION,
        ]);
        assert_eq!(bits, 3); // bits 0 and 1
    }

    #[test]
    fn test_modifier_bitset_default_library() {
        let bits = modifier_bitset(&[SemanticTokenModifier::DEFAULT_LIBRARY]);
        assert_eq!(bits, 16); // Fifth modifier, bit 4
    }

    // Snapshot tests for full tokenization

    #[test]
    fn test_snapshot_minimal() {
        let content = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }";
        let api = test_api();
        let doc = Document::new(content.to_string(), Some(&api));
        let tokens = compute_semantic_tokens(&doc, &api);

        // Convert to readable format for snapshot
        let readable: Vec<_> = tokens.iter().map(|t| {
            (t.delta_line, t.delta_start, t.length, t.token_type, t.token_modifiers_bitset)
        }).collect();

        insta::assert_debug_snapshot!(readable);
    }

    #[test]
    fn test_snapshot_struct_definition() {
        let content = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }

pub struct Test extend Signal {
    count: i64,
}";
        let api = test_api();
        let doc = Document::new(content.to_string(), Some(&api));
        let tokens = compute_semantic_tokens(&doc, &api);

        let readable: Vec<_> = tokens.iter().map(|t| {
            (t.delta_line, t.delta_start, t.length, t.token_type, t.token_modifiers_bitset)
        }).collect();

        insta::assert_debug_snapshot!(readable);
    }

    #[test]
    fn test_snapshot_function_definition() {
        let content = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }

pub fn test_func(x: i64): i64 {
    return x + 1;
}";
        let api = test_api();
        let doc = Document::new(content.to_string(), Some(&api));
        let tokens = compute_semantic_tokens(&doc, &api);

        let readable: Vec<_> = tokens.iter().map(|t| {
            (t.delta_line, t.delta_start, t.length, t.token_type, t.token_modifiers_bitset)
        }).collect();

        insta::assert_debug_snapshot!(readable);
    }

    #[test]
    fn test_snapshot_method_definition() {
        let content = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }

pub struct Test extend Signal { }

pub fn Test::do_something(self: &Test, value: i64): i64 {
    return value * 2;
}";
        let api = test_api();
        let doc = Document::new(content.to_string(), Some(&api));
        let tokens = compute_semantic_tokens(&doc, &api);

        let readable: Vec<_> = tokens.iter().map(|t| {
            (t.delta_line, t.delta_start, t.length, t.token_type, t.token_modifiers_bitset)
        }).collect();

        insta::assert_debug_snapshot!(readable);
    }

    #[test]
    fn test_snapshot_enum_definition() {
        let content = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }

pub enum Status {
    Active,
    Inactive,
    Pending = 5,
}";
        let api = test_api();
        let doc = Document::new(content.to_string(), Some(&api));
        let tokens = compute_semantic_tokens(&doc, &api);

        let readable: Vec<_> = tokens.iter().map(|t| {
            (t.delta_line, t.delta_start, t.length, t.token_type, t.token_modifiers_bitset)
        }).collect();

        insta::assert_debug_snapshot!(readable);
    }

    #[test]
    fn test_snapshot_let_statement() {
        let content = r#"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }

pub fn test() {
    let x: i64 = 42;
    let name: String = "hello";
}"#;
        let api = test_api();
        let doc = Document::new(content.to_string(), Some(&api));
        let tokens = compute_semantic_tokens(&doc, &api);

        let readable: Vec<_> = tokens.iter().map(|t| {
            (t.delta_line, t.delta_start, t.length, t.token_type, t.token_modifiers_bitset)
        }).collect();

        insta::assert_debug_snapshot!(readable);
    }

    #[test]
    fn test_snapshot_path_expression() {
        let content = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }

pub fn test(): SignalCheck {
    return SignalCheck::Pass;
}";
        let api = test_api();
        let doc = Document::new(content.to_string(), Some(&api));
        let tokens = compute_semantic_tokens(&doc, &api);

        let readable: Vec<_> = tokens.iter().map(|t| {
            (t.delta_line, t.delta_start, t.length, t.token_type, t.token_modifiers_bitset)
        }).collect();

        insta::assert_debug_snapshot!(readable);
    }

    #[test]
    fn test_snapshot_function_call() {
        let content = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }

pub fn test() {
    let x: f64 = abs(-5.0);
    let y: f64 = sqrt(16.0);
}";
        let api = test_api();
        let doc = Document::new(content.to_string(), Some(&api));
        let tokens = compute_semantic_tokens(&doc, &api);

        let readable: Vec<_> = tokens.iter().map(|t| {
            (t.delta_line, t.delta_start, t.length, t.token_type, t.token_modifiers_bitset)
        }).collect();

        insta::assert_debug_snapshot!(readable);
    }

    #[test]
    fn test_snapshot_comments() {
        let content = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }

// This is a comment
pub fn test() {
    // Another comment
    let x: i64 = 1;
}";
        let api = test_api();
        let doc = Document::new(content.to_string(), Some(&api));
        let tokens = compute_semantic_tokens(&doc, &api);

        let readable: Vec<_> = tokens.iter().map(|t| {
            (t.delta_line, t.delta_start, t.length, t.token_type, t.token_modifiers_bitset)
        }).collect();

        insta::assert_debug_snapshot!(readable);
    }

    // Game type modifier tests

    #[test]
    fn test_game_type_has_library_modifier() {
        let content = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }

pub struct Test extend Signal { }";
        let api = test_api();
        let doc = Document::new(content.to_string(), Some(&api));
        let tokens = compute_semantic_tokens(&doc, &api);

        // Find the token for "Signal" - it should have DEFAULT_LIBRARY modifier
        // Signal appears at some delta position with library modifier (bit 4 = 16)
        let has_library_type = tokens.iter().any(|t| {
            t.token_modifiers_bitset & 16 != 0 // DEFAULT_LIBRARY is bit 4
        });
        assert!(has_library_type, "Signal should have DEFAULT_LIBRARY modifier");
    }

    #[test]
    fn test_user_type_no_library_modifier() {
        let content = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }

pub struct UserType { }

pub fn test(x: UserType) { }";
        let api = test_api();
        let doc = Document::new(content.to_string(), Some(&api));
        let tokens = compute_semantic_tokens(&doc, &api);

        // The struct definition token should have DEFINITION modifier but not DEFAULT_LIBRARY
        // DEFINITION is bit 1 = 2, DEFAULT_LIBRARY is bit 4 = 16
        let struct_def_token = tokens.iter().find(|t| {
            t.token_modifiers_bitset & 2 != 0 && // Has DEFINITION
            t.token_type == token_type_index(&SemanticTokenType::STRUCT)
        });
        assert!(struct_def_token.is_some(), "Should find struct definition");
        let token = struct_def_token.expect("struct definition token should exist");
        assert_eq!(token.token_modifiers_bitset & 16, 0, "User type should not have DEFAULT_LIBRARY modifier");
    }

    // Delta encoding tests

    #[test]
    fn test_delta_encoding_same_line() {
        let content = r"let x: i64 = 42;";
        let api = test_api();
        let doc = Document::new(content.to_string(), Some(&api));
        let tokens = compute_semantic_tokens(&doc, &api);

        // All tokens on same line should have delta_line = 0 (except first)
        for (i, token) in tokens.iter().enumerate() {
            if i > 0 {
                assert_eq!(token.delta_line, 0, "Token {i} should be on same line");
            }
        }
    }

    #[test]
    fn test_delta_encoding_different_lines() {
        let content = r"let x: i64 = 1;
let y: i64 = 2;";
        let api = test_api();
        let doc = Document::new(content.to_string(), Some(&api));
        let tokens = compute_semantic_tokens(&doc, &api);

        // Some token should have delta_line > 0
        let has_line_delta = tokens.iter().any(|t| t.delta_line > 0);
        assert!(has_line_delta, "Should have tokens on different lines");
    }

    // Offset to line/char conversion tests

    #[test]
    fn test_offset_to_line_char_start() {
        let api = test_api();
        let collector = TokenCollector::new("hello\nworld", &api);
        let (line, char) = collector.offset_to_line_char(0);
        assert_eq!(line, 0);
        assert_eq!(char, 0);
    }

    #[test]
    fn test_offset_to_line_char_same_line() {
        let api = test_api();
        let collector = TokenCollector::new("hello\nworld", &api);
        let (line, char) = collector.offset_to_line_char(3);
        assert_eq!(line, 0);
        assert_eq!(char, 3);
    }

    #[test]
    fn test_offset_to_line_char_second_line() {
        let api = test_api();
        let collector = TokenCollector::new("hello\nworld", &api);
        let (line, char) = collector.offset_to_line_char(6); // 'w' in world
        assert_eq!(line, 1);
        assert_eq!(char, 0);
    }

    #[test]
    fn test_offset_to_line_char_middle_second_line() {
        let api = test_api();
        let collector = TokenCollector::new("hello\nworld", &api);
        let (line, char) = collector.offset_to_line_char(8); // 'r' in world
        assert_eq!(line, 1);
        assert_eq!(char, 2);
    }

    // Edge case tests

    #[test]
    fn test_empty_content() {
        let content = "";
        let api = test_api();
        let doc = Document::new(content.to_string(), Some(&api));
        let tokens = compute_semantic_tokens(&doc, &api);
        assert!(tokens.is_empty());
    }

    #[test]
    fn test_only_whitespace() {
        let content = "   \n\n   ";
        let api = test_api();
        let doc = Document::new(content.to_string(), Some(&api));
        let tokens = compute_semantic_tokens(&doc, &api);
        assert!(tokens.is_empty());
    }

    #[test]
    fn test_only_comment() {
        let content = "// This is just a comment";
        let api = test_api();
        let doc = Document::new(content.to_string(), Some(&api));
        let tokens = compute_semantic_tokens(&doc, &api);

        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].token_type, token_type_index(&SemanticTokenType::COMMENT));
    }
}
