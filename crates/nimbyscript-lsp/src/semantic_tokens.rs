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
