//! Semantic token highlighting using the analyzer's SemanticContext.
//!
//! This module provides accurate syntax highlighting by leveraging the semantic
//! analysis that's already performed on the document. Instead of string-based
//! heuristics, we use the resolved symbol information from the analyzer.

use tower_lsp::lsp_types::*;

use nimbyscript_analyzer::scope::SymbolKind as ScopedSymbolKind;
use nimbyscript_analyzer::semantic::SemanticContext;
use nimbyscript_analyzer::ApiDefinitions;
use nimbyscript_parser::{kind, Node, NodeExt};

use crate::document::Document;

/// Define the semantic token types we support
pub const TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::KEYWORD,      // 0
    SemanticTokenType::TYPE,         // 1
    SemanticTokenType::FUNCTION,     // 2
    SemanticTokenType::VARIABLE,     // 3
    SemanticTokenType::PROPERTY,     // 4
    SemanticTokenType::STRING,       // 5
    SemanticTokenType::NUMBER,       // 6
    SemanticTokenType::COMMENT,      // 7
    SemanticTokenType::OPERATOR,     // 8
    SemanticTokenType::PARAMETER,    // 9
    SemanticTokenType::ENUM_MEMBER,  // 10
    SemanticTokenType::STRUCT,       // 11
    SemanticTokenType::ENUM,         // 12
];

/// Define semantic token modifiers
pub const TOKEN_MODIFIERS: &[SemanticTokenModifier] = &[
    SemanticTokenModifier::DECLARATION,    // bit 0
    SemanticTokenModifier::DEFINITION,     // bit 1
    SemanticTokenModifier::READONLY,       // bit 2
    SemanticTokenModifier::STATIC,         // bit 3
    SemanticTokenModifier::DEFAULT_LIBRARY, // bit 4
];

fn token_type_index(tt: &SemanticTokenType) -> u32 {
    TOKEN_TYPES
        .iter()
        .position(|t| t == tt)
        .map_or(0, |i| i as u32)
}

fn modifier_bitset(modifiers: &[SemanticTokenModifier]) -> u32 {
    modifiers.iter().fold(0, |bits, m| {
        bits | TOKEN_MODIFIERS
            .iter()
            .position(|t| t == m)
            .map_or(0, |i| 1 << i)
    })
}

pub fn semantic_token_legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: TOKEN_TYPES.to_vec(),
        token_modifiers: TOKEN_MODIFIERS.to_vec(),
    }
}

pub fn compute_semantic_tokens(doc: &Document, api: &ApiDefinitions) -> Vec<SemanticToken> {
    let ctx = doc.create_semantic_context(api);
    let mut collector = TokenCollector::new(&doc.content, &ctx);
    collector.collect_tokens(ctx.tree.root_node());
    collector.into_tokens(&doc.content)
}

struct RawToken {
    start: usize,
    length: usize,
    token_type: SemanticTokenType,
    modifiers: Vec<SemanticTokenModifier>,
}

struct TokenCollector<'a> {
    content: &'a str,
    ctx: &'a SemanticContext<'a>,
    raw_tokens: Vec<RawToken>,
}

impl<'a> TokenCollector<'a> {
    fn new(content: &'a str, ctx: &'a SemanticContext<'a>) -> Self {
        Self {
            content,
            ctx,
            raw_tokens: Vec::new(),
        }
    }

    fn add_token(
        &mut self,
        start: usize,
        end: usize,
        token_type: SemanticTokenType,
        modifiers: &[SemanticTokenModifier],
    ) {
        if start < end {
            self.raw_tokens.push(RawToken {
                start,
                length: end - start,
                token_type,
                modifiers: modifiers.to_vec(),
            });
        }
    }

    fn collect_tokens(&mut self, node: Node) {
        match node.kind() {
            // Keywords
            kind::VISIBILITY_MODIFIER
            | kind::STORAGE_MODIFIER
            | kind::MUTABILITY_MODIFIER
            | kind::BOOLEAN => {
                self.add_token(
                    node.start_byte(),
                    node.end_byte(),
                    SemanticTokenType::KEYWORD,
                    &[],
                );
            }

            // Comments
            kind::COMMENT => {
                self.add_token(
                    node.start_byte(),
                    node.end_byte(),
                    SemanticTokenType::COMMENT,
                    &[],
                );
            }

            // Literals
            kind::NUMBER | kind::TIME_LITERAL => {
                self.add_token(
                    node.start_byte(),
                    node.end_byte(),
                    SemanticTokenType::NUMBER,
                    &[],
                );
            }
            kind::STRING_LITERAL => {
                self.add_token(
                    node.start_byte(),
                    node.end_byte(),
                    SemanticTokenType::STRING,
                    &[],
                );
            }

            // Type definitions
            kind::STRUCT_DEFINITION => self.collect_struct_definition(node),
            kind::ENUM_DEFINITION => self.collect_enum_definition(node),

            // Function definitions
            kind::FUNCTION_DEFINITION => self.collect_function_definition(node),

            // Type references
            kind::TYPE_IDENTIFIER => self.collect_type_identifier(node),

            // Path expressions (enum variants, static methods, module access)
            kind::PATH_EXPRESSION => self.collect_path_expression(node),

            // Call expressions
            kind::CALL_EXPRESSION => self.collect_call_expression(node),

            // Field access
            kind::FIELD_ACCESS => self.collect_field_access(node),

            // Let bindings
            kind::LET_STATEMENT | kind::LET_ELSE_STATEMENT => {
                self.collect_let_statement(node);
            }

            // For statement variable
            kind::FOR_STATEMENT => self.collect_for_statement(node),

            // Parameters
            kind::PARAMETER => self.collect_parameter(node),

            // Const declarations
            kind::CONST_DECLARATION => self.collect_const_declaration(node),

            // Meta entries
            kind::META_ENTRY => self.collect_meta_entry(node),

            // Recurse into other nodes
            _ => {
                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    self.collect_tokens(child);
                }
            }
        }
    }

    fn collect_struct_definition(&mut self, node: Node) {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            match child.kind() {
                kind::VISIBILITY_MODIFIER => {
                    self.add_token(
                        child.start_byte(),
                        child.end_byte(),
                        SemanticTokenType::KEYWORD,
                        &[],
                    );
                }
                kind::IDENTIFIER => {
                    // Struct name
                    if node.child_by_field("name").map(|n| n.id()) == Some(child.id()) {
                        self.add_token(
                            child.start_byte(),
                            child.end_byte(),
                            SemanticTokenType::STRUCT,
                            &[SemanticTokenModifier::DEFINITION],
                        );
                    }
                }
                kind::EXTENDS_CLAUSE => {
                    if let Some(type_node) = child.child_by_field("type") {
                        let type_name = type_node.text(self.content);
                        let modifiers = self.type_modifiers(type_name);
                        self.add_token(
                            type_node.start_byte(),
                            type_node.end_byte(),
                            SemanticTokenType::TYPE,
                            &modifiers,
                        );
                    }
                }
                kind::STRUCT_FIELD => {
                    if let Some(name_node) = child.child_by_field("name") {
                        self.add_token(
                            name_node.start_byte(),
                            name_node.end_byte(),
                            SemanticTokenType::PROPERTY,
                            &[SemanticTokenModifier::DEFINITION],
                        );
                    }
                    // Recurse for type
                    self.collect_tokens(child);
                }
                _ => self.collect_tokens(child),
            }
        }
    }

    fn collect_enum_definition(&mut self, node: Node) {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            match child.kind() {
                kind::VISIBILITY_MODIFIER => {
                    self.add_token(
                        child.start_byte(),
                        child.end_byte(),
                        SemanticTokenType::KEYWORD,
                        &[],
                    );
                }
                kind::IDENTIFIER => {
                    // Enum name
                    if node.child_by_field("name").map(|n| n.id()) == Some(child.id()) {
                        self.add_token(
                            child.start_byte(),
                            child.end_byte(),
                            SemanticTokenType::ENUM,
                            &[SemanticTokenModifier::DEFINITION],
                        );
                    }
                }
                kind::ENUM_VARIANT => {
                    if let Some(name_node) = child.child_by_field("name") {
                        self.add_token(
                            name_node.start_byte(),
                            name_node.end_byte(),
                            SemanticTokenType::ENUM_MEMBER,
                            &[SemanticTokenModifier::DEFINITION],
                        );
                    }
                    // Recurse for value expression
                    self.collect_tokens(child);
                }
                _ => self.collect_tokens(child),
            }
        }
    }

    fn collect_function_definition(&mut self, node: Node) {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            match child.kind() {
                kind::VISIBILITY_MODIFIER => {
                    self.add_token(
                        child.start_byte(),
                        child.end_byte(),
                        SemanticTokenType::KEYWORD,
                        &[],
                    );
                }
                kind::FUNCTION_NAME => {
                    self.collect_function_name(child);
                }
                _ => self.collect_tokens(child),
            }
        }
    }

    fn collect_function_name(&mut self, node: Node) {
        // Function name can be "func" or "Type::method"
        let mut identifiers: Vec<Node> = Vec::new();
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == kind::IDENTIFIER {
                identifiers.push(child);
            }
        }

        match identifiers.len() {
            1 => {
                // Simple function
                let id = identifiers[0];
                self.add_token(
                    id.start_byte(),
                    id.end_byte(),
                    SemanticTokenType::FUNCTION,
                    &[SemanticTokenModifier::DEFINITION],
                );
            }
            2 => {
                // Method: Type::method
                let type_id = identifiers[0];
                let method_id = identifiers[1];

                // Type part
                self.add_token(
                    type_id.start_byte(),
                    type_id.end_byte(),
                    SemanticTokenType::TYPE,
                    &[],
                );
                // Method part
                self.add_token(
                    method_id.start_byte(),
                    method_id.end_byte(),
                    SemanticTokenType::FUNCTION,
                    &[SemanticTokenModifier::DEFINITION],
                );
            }
            _ => {}
        }
    }

    fn collect_type_identifier(&mut self, node: Node) {
        // Type identifier can contain identifier and generic_arguments
        if let Some(id_node) = node.child_by_kind(kind::IDENTIFIER) {
            let type_name = id_node.text(self.content);
            let modifiers = self.type_modifiers(type_name);

            // Determine token type based on whether it's an enum or struct
            let token_type = if self.ctx.is_game_enum(type_name) || self.ctx.is_user_enum(type_name)
            {
                SemanticTokenType::ENUM
            } else {
                SemanticTokenType::TYPE
            };

            self.add_token(id_node.start_byte(), id_node.end_byte(), token_type, &modifiers);
        }

        // Handle generic arguments recursively
        if let Some(generics) = node.child_by_kind(kind::GENERIC_ARGUMENTS) {
            let mut cursor = generics.walk();
            for child in generics.children(&mut cursor) {
                if child.kind() == kind::TYPE_IDENTIFIER {
                    self.collect_type_identifier(child);
                }
            }
        }
    }

    fn collect_path_expression(&mut self, node: Node) {
        // Path expressions: Type::variant, Module::func, ID<T>::empty
        let text = node.text(self.content);

        // If no ::, it's just an identifier - handle elsewhere
        if !text.contains("::") {
            // Just a simple identifier, not a path - check if it's a known symbol
            if let Some(id) = node.child_by_kind(kind::IDENTIFIER) {
                self.collect_identifier(id);
            }
            return;
        }

        // Walk the path segments
        let mut cursor = node.walk();
        let segments: Vec<Node> = node
            .children(&mut cursor)
            .filter(|c| c.kind() == kind::PATH_SEGMENT)
            .collect();

        let num_segments = segments.len();
        for (i, segment) in segments.iter().enumerate() {
            let is_last = i == num_segments - 1;
            self.collect_path_segment(*segment, is_last);
        }
    }

    fn collect_path_segment(&mut self, segment: Node, is_last: bool) {
        // Check for nested path_segment (happens with generics like ID<Train>::new)
        if let Some(inner) = segment.child_by_kind(kind::PATH_SEGMENT) {
            self.collect_path_segment(inner, false);
            // Don't return - also process this segment's identifier if it's the last
            if !is_last {
                return;
            }
        }

        // Get the identifier
        if let Some(id_node) = segment.child_by_kind(kind::IDENTIFIER) {
            let name = id_node.text(self.content);

            let (token_type, modifiers) = if is_last {
                // Last segment could be:
                // - Enum variant (most common)
                // - Static method (like ID<T>::empty)
                // - Module function
                (SemanticTokenType::ENUM_MEMBER, vec![])
            } else {
                // Not last - it's a type, enum, or module
                self.classify_path_prefix(name)
            };

            self.add_token(id_node.start_byte(), id_node.end_byte(), token_type, &modifiers);
        }

        // Handle generic arguments in path segment
        self.collect_generic_arguments(segment);
    }

    fn collect_call_expression(&mut self, node: Node) {
        if let Some(func_node) = node.child_by_field("function") {
            match func_node.kind() {
                kind::PATH_EXPRESSION => self.collect_path_call_or_simple(func_node),
                kind::FIELD_ACCESS => self.collect_method_call(func_node),
                kind::IDENTIFIER => self.collect_simple_function_call(func_node),
                _ => {}
            }
        }

        // Recurse for arguments
        if let Some(args) = node.child_by_field("arguments") {
            self.collect_tokens(args);
        }
    }

    fn collect_path_call_or_simple(&mut self, func_node: Node) {
        let text = func_node.text(self.content);
        if text.contains("::") {
            self.collect_path_call(func_node);
        } else if let Some(id) = func_node.child_by_kind(kind::IDENTIFIER) {
            self.collect_simple_function_call(id);
        }
    }

    fn collect_simple_function_call(&mut self, func_node: Node) {
        let name = func_node.text(self.content);
        let modifiers = if self.ctx.api.get_function(name).is_some() {
            vec![SemanticTokenModifier::DEFAULT_LIBRARY]
        } else {
            vec![]
        };
        self.add_token(
            func_node.start_byte(),
            func_node.end_byte(),
            SemanticTokenType::FUNCTION,
            &modifiers,
        );
    }

    fn collect_path_call(&mut self, node: Node) {
        // Like collect_path_expression but last segment is FUNCTION
        let mut cursor = node.walk();
        let segments: Vec<Node> = node
            .children(&mut cursor)
            .filter(|c| c.kind() == kind::PATH_SEGMENT)
            .collect();

        let num_segments = segments.len();
        for (i, segment) in segments.iter().enumerate() {
            let is_last = i == num_segments - 1;
            self.collect_path_call_segment(*segment, is_last);
        }
    }

    fn collect_path_call_segment(&mut self, segment: Node, is_last: bool) {
        // Handle nested segments
        if let Some(inner) = segment.child_by_kind(kind::PATH_SEGMENT) {
            self.collect_path_segment(inner, false);
        }

        if let Some(id_node) = segment.child_by_kind(kind::IDENTIFIER) {
            let name = id_node.text(self.content);

            let (token_type, modifiers) = if is_last {
                // Last segment in call is a function/method
                (SemanticTokenType::FUNCTION, vec![])
            } else {
                self.classify_path_prefix(name)
            };

            self.add_token(id_node.start_byte(), id_node.end_byte(), token_type, &modifiers);
        }

        // Handle generics
        self.collect_generic_arguments(segment);
    }

    fn collect_generic_arguments(&mut self, node: Node) {
        let Some(generics) = node.child_by_kind(kind::GENERIC_ARGUMENTS) else {
            return;
        };
        let mut cursor = generics.walk();
        for child in generics.children(&mut cursor) {
            if child.kind() == kind::TYPE_IDENTIFIER {
                self.collect_type_identifier(child);
            }
        }
    }

    fn collect_method_call(&mut self, field_access: Node) {
        // field_access has object and field
        // We need to tokenize the object and mark field as function

        if let Some(object) = field_access.child_by_field("object") {
            self.collect_tokens(object);
        }

        if let Some(field) = field_access.child_by_field("field") {
            self.add_token(
                field.start_byte(),
                field.end_byte(),
                SemanticTokenType::FUNCTION,
                &[],
            );
        }
    }

    fn collect_field_access(&mut self, node: Node) {
        // field_access that's NOT part of a call expression
        if let Some(object) = node.child_by_field("object") {
            self.collect_tokens(object);
        }

        if let Some(field) = node.child_by_field("field") {
            self.add_token(
                field.start_byte(),
                field.end_byte(),
                SemanticTokenType::PROPERTY,
                &[],
            );
        }
    }

    fn collect_let_statement(&mut self, node: Node) {
        if let Some(binding) = node.child_by_kind(kind::BINDING) {
            if let Some(name_node) = binding.child_by_field("name") {
                self.add_token(
                    name_node.start_byte(),
                    name_node.end_byte(),
                    SemanticTokenType::VARIABLE,
                    &[SemanticTokenModifier::DECLARATION],
                );
            }
        }

        // Recurse for type and value
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.collect_tokens(child);
        }
    }

    fn collect_for_statement(&mut self, node: Node) {
        if let Some(var_node) = node.child_by_field("variable") {
            self.add_token(
                var_node.start_byte(),
                var_node.end_byte(),
                SemanticTokenType::VARIABLE,
                &[SemanticTokenModifier::DECLARATION],
            );
        }

        // Recurse for iterable and body
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.collect_tokens(child);
        }
    }

    fn collect_parameter(&mut self, node: Node) {
        if let Some(name_node) = node.child_by_field("name") {
            self.add_token(
                name_node.start_byte(),
                name_node.end_byte(),
                SemanticTokenType::PARAMETER,
                &[SemanticTokenModifier::DECLARATION],
            );
        }

        // Recurse for type
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.collect_tokens(child);
        }
    }

    fn collect_const_declaration(&mut self, node: Node) {
        if let Some(name_node) = node.child_by_field("name") {
            self.add_token(
                name_node.start_byte(),
                name_node.end_byte(),
                SemanticTokenType::VARIABLE,
                &[
                    SemanticTokenModifier::DECLARATION,
                    SemanticTokenModifier::READONLY,
                ],
            );
        }

        // Recurse for type and value
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.collect_tokens(child);
        }
    }

    fn collect_meta_entry(&mut self, node: Node) {
        if let Some(key) = node.child_by_field("key") {
            self.add_token(
                key.start_byte(),
                key.end_byte(),
                SemanticTokenType::PROPERTY,
                &[],
            );
        }

        // Recurse for value
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.collect_tokens(child);
        }
    }

    fn collect_identifier(&mut self, node: Node) {
        let name = node.text(self.content);

        // Look up in scope
        if let Some(symbol) = self.ctx.scopes.lookup_global(name) {
            let (token_type, modifiers) = match symbol.kind {
                ScopedSymbolKind::Variable => (SemanticTokenType::VARIABLE, vec![]),
                ScopedSymbolKind::Parameter => (SemanticTokenType::PARAMETER, vec![]),
                ScopedSymbolKind::Constant => {
                    (SemanticTokenType::VARIABLE, vec![SemanticTokenModifier::READONLY])
                }
                ScopedSymbolKind::Struct => (SemanticTokenType::STRUCT, vec![]),
                ScopedSymbolKind::Enum => (SemanticTokenType::ENUM, vec![]),
                ScopedSymbolKind::EnumVariant => (SemanticTokenType::ENUM_MEMBER, vec![]),
                ScopedSymbolKind::Field => (SemanticTokenType::PROPERTY, vec![]),
                ScopedSymbolKind::Function | ScopedSymbolKind::Method => {
                    (SemanticTokenType::FUNCTION, vec![])
                }
            };
            self.add_token(node.start_byte(), node.end_byte(), token_type, &modifiers);
            return;
        }

        // Check if it's a game type/enum
        if self.ctx.is_game_type(name) {
            self.add_token(
                node.start_byte(),
                node.end_byte(),
                SemanticTokenType::TYPE,
                &[SemanticTokenModifier::DEFAULT_LIBRARY],
            );
            return;
        }

        if self.ctx.is_game_enum(name) {
            self.add_token(
                node.start_byte(),
                node.end_byte(),
                SemanticTokenType::ENUM,
                &[SemanticTokenModifier::DEFAULT_LIBRARY],
            );
            return;
        }

        // Default to variable
        self.add_token(
            node.start_byte(),
            node.end_byte(),
            SemanticTokenType::VARIABLE,
            &[],
        );
    }

    fn classify_path_prefix(&self, name: &str) -> (SemanticTokenType, Vec<SemanticTokenModifier>) {
        // Determine what kind of thing a path prefix is
        if self.ctx.is_game_type(name) {
            (
                SemanticTokenType::TYPE,
                vec![SemanticTokenModifier::DEFAULT_LIBRARY],
            )
        } else if self.ctx.is_game_enum(name) {
            (
                SemanticTokenType::ENUM,
                vec![SemanticTokenModifier::DEFAULT_LIBRARY],
            )
        } else if self.ctx.is_module(name) {
            (
                SemanticTokenType::TYPE,
                vec![SemanticTokenModifier::DEFAULT_LIBRARY],
            )
        } else if self.ctx.is_user_struct(name) {
            (SemanticTokenType::STRUCT, vec![])
        } else if self.ctx.is_user_enum(name) {
            (SemanticTokenType::ENUM, vec![])
        } else {
            (SemanticTokenType::TYPE, vec![])
        }
    }

    fn type_modifiers(&self, name: &str) -> Vec<SemanticTokenModifier> {
        if self.ctx.is_game_type(name) || self.ctx.is_game_enum(name) {
            vec![SemanticTokenModifier::DEFAULT_LIBRARY]
        } else {
            vec![]
        }
    }

    fn into_tokens(mut self, content: &str) -> Vec<SemanticToken> {
        // Sort by position
        self.raw_tokens.sort_by_key(|t| t.start);

        // Convert to delta-encoded format
        let mut result = Vec::new();
        let mut prev_line = 0u32;
        let mut prev_char = 0u32;

        for token in &self.raw_tokens {
            let (line, char) = offset_to_line_char(content, token.start);

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
}

fn offset_to_line_char(content: &str, offset: usize) -> (u32, u32) {
    let mut line = 0u32;
    let mut line_start = 0usize;

    for (i, c) in content.char_indices() {
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

#[cfg(test)]
mod tests {
    use super::*;

    fn test_api() -> ApiDefinitions {
        let toml_content = include_str!("../../../api-definitions/nimbyrails.v1.toml");
        ApiDefinitions::load_from_str(toml_content).expect("Failed to parse API")
    }

    #[test]
    fn test_semantic_token_legend_types() {
        let legend = semantic_token_legend();
        assert!(!legend.token_types.is_empty());
        assert!(legend.token_types.contains(&SemanticTokenType::KEYWORD));
        assert!(legend.token_types.contains(&SemanticTokenType::TYPE));
        assert!(legend.token_types.contains(&SemanticTokenType::FUNCTION));
    }

    #[test]
    fn test_basic_struct() {
        let content = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }

pub struct Test extend Signal {
    count: i64,
}";
        let api = test_api();
        let doc = Document::new(content.to_string(), Some(&api));
        let tokens = compute_semantic_tokens(&doc, &api);

        // Should have tokens for pub, struct name, Signal, field name, i64
        assert!(!tokens.is_empty());
    }

    #[test]
    fn test_generic_type_in_struct() {
        let content = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }

pub struct Test extend Signal {
    train_id: ID<Train>,
}";
        let api = test_api();
        let doc = Document::new(content.to_string(), Some(&api));
        let tokens = compute_semantic_tokens(&doc, &api);

        // Should tokenize both ID and Train as types
        assert!(!tokens.is_empty());
    }

    #[test]
    fn test_path_expression() {
        let content = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }

pub fn test(): SignalCheck {
    return SignalCheck::Pass;
}";
        let api = test_api();
        let doc = Document::new(content.to_string(), Some(&api));
        let tokens = compute_semantic_tokens(&doc, &api);

        assert!(!tokens.is_empty());
    }

    #[test]
    fn test_generic_path_call() {
        let content = r"script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }

pub fn test(): ID<Train> {
    return ID<Train>::empty();
}";
        let api = test_api();
        let doc = Document::new(content.to_string(), Some(&api));
        let tokens = compute_semantic_tokens(&doc, &api);

        // Should tokenize ID, Train in return type and in path expression
        assert!(!tokens.is_empty());
    }
}
