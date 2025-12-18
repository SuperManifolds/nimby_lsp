//! Document formatting for NimbyScript.
//!
//! Provides three LSP formatting capabilities:
//! - Full document formatting
//! - Range formatting
//! - On-type formatting

use std::fmt::Write;

use nimbyscript_parser::{kind, Node, NodeExt};
use tower_lsp::lsp_types::{FormattingOptions, Position, Range, TextEdit};

use crate::document::Document;

/// Configuration for the formatter, derived from LSP FormattingOptions.
#[derive(Debug, Clone)]
pub struct FormattingConfig {
    pub tab_size: u32,
    pub insert_spaces: bool,
    pub insert_final_newline: bool,
}

impl From<&FormattingOptions> for FormattingConfig {
    fn from(opts: &FormattingOptions) -> Self {
        Self {
            tab_size: opts.tab_size,
            insert_spaces: opts.insert_spaces,
            insert_final_newline: opts.insert_final_newline.unwrap_or(true),
        }
    }
}

impl Default for FormattingConfig {
    fn default() -> Self {
        Self {
            tab_size: 4,
            insert_spaces: true,
            insert_final_newline: true,
        }
    }
}

/// Format an entire document.
pub fn format_document(doc: &Document, config: &FormattingConfig) -> Vec<TextEdit> {
    let content = &doc.content;
    let tree = doc.tree();
    let root = tree.root_node();

    let engine = FormattingEngine::new(content, config);
    let formatted = engine.format_source_file(root);

    // Return a single edit replacing the entire document
    let line_count = content.lines().count();
    let last_line_len = content.lines().last().map_or(0, str::len);

    vec![TextEdit {
        range: Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: line_count as u32,
                character: last_line_len as u32,
            },
        },
        new_text: formatted,
    }]
}

/// Format a specific range within a document.
pub fn format_range(doc: &Document, range: Range, config: &FormattingConfig) -> Vec<TextEdit> {
    let content = &doc.content;
    let tree = doc.tree();

    // Find the smallest complete node(s) that fully contain the range
    let start_offset = doc.position_to_offset(range.start);
    let end_offset = doc.position_to_offset(range.end);

    // Find nodes that intersect with the range
    let root = tree.root_node();
    let engine = FormattingEngine::new(content, config);

    // For range formatting, we find top-level items that overlap with the range
    // and format them completely
    let mut edits = Vec::new();
    let mut cursor = root.walk();

    for child in root.children(&mut cursor) {
        let child_start = child.start_byte();
        let child_end = child.end_byte();

        // Check if this node overlaps with the requested range
        if child_end > start_offset && child_start < end_offset {
            let formatted = engine.format_top_level_item(child, 0);
            let start_pos = doc.offset_to_position(child_start);
            let end_pos = doc.offset_to_position(child_end);

            edits.push(TextEdit {
                range: Range {
                    start: start_pos,
                    end: end_pos,
                },
                new_text: formatted,
            });
        }
    }

    edits
}

/// Format after typing a specific character.
pub fn format_on_type(
    doc: &Document,
    position: Position,
    ch: &str,
    config: &FormattingConfig,
) -> Vec<TextEdit> {
    let content = &doc.content;
    let tree = doc.tree();
    let offset = doc.position_to_offset(position);

    let engine = FormattingEngine::new(content, config);

    match ch {
        "}" => engine.format_after_close_brace(doc, tree, offset),
        ";" => engine.format_after_semicolon(doc, tree, offset),
        _ => Vec::new(),
    }
}

/// The formatting engine that walks the tree and produces formatted output.
struct FormattingEngine<'a> {
    content: &'a str,
    indent_str: String,
    config: &'a FormattingConfig,
}

impl<'a> FormattingEngine<'a> {
    fn new(content: &'a str, config: &'a FormattingConfig) -> Self {
        let indent_str = if config.insert_spaces {
            " ".repeat(config.tab_size as usize)
        } else {
            "\t".to_string()
        };

        Self {
            content,
            indent_str,
            config,
        }
    }

    fn indent(&self, level: usize) -> String {
        self.indent_str.repeat(level)
    }

    /// Format the entire source file.
    fn format_source_file(&self, node: Node) -> String {
        let mut result = String::new();
        let mut cursor = node.walk();
        let mut first = true;

        for child in node.children(&mut cursor) {
            if !first {
                result.push('\n');
            }
            first = false;

            let formatted = self.format_top_level_item(child, 0);
            result.push_str(&formatted);
        }

        // Ensure final newline
        if self.config.insert_final_newline && !result.ends_with('\n') {
            result.push('\n');
        }

        result
    }

    /// Format a top-level item (script_meta, struct, enum, function, const).
    fn format_top_level_item(&self, node: Node, indent_level: usize) -> String {
        match node.kind() {
            kind::SCRIPT_META => self.format_script_meta(node, indent_level),
            kind::STRUCT_DEFINITION => self.format_struct(node, indent_level),
            kind::ENUM_DEFINITION => self.format_enum(node, indent_level),
            kind::FUNCTION_DEFINITION => self.format_function(node, indent_level),
            kind::CONST_DECLARATION => self.format_const(node, indent_level),
            kind::COMMENT => format!("{}{}", self.indent(indent_level), node.text(self.content)),
            _ => node.text(self.content).to_string(),
        }
    }

    /// Format script meta block.
    fn format_script_meta(&self, node: Node, indent_level: usize) -> String {
        let indent = self.indent(indent_level);

        let mut result = format!("{indent}script meta {{\n");

        // Find the meta_block child which contains the meta_map
        let meta_map = node
            .child_by_kind(kind::META_BLOCK)
            .and_then(|mb| mb.child_by_kind(kind::META_MAP));

        if let Some(map) = meta_map {
            let mut cursor = map.walk();
            for child in map.children(&mut cursor) {
                if child.kind() == kind::META_ENTRY {
                    let formatted = self.format_meta_entry(child, indent_level + 1);
                    result.push_str(&formatted);
                    result.push_str(",\n");
                }
            }
        }

        let _ = writeln!(result, "{indent}}}");
        result
    }

    /// Format a meta entry (key: value).
    fn format_meta_entry(&self, node: Node, indent_level: usize) -> String {
        let indent = self.indent(indent_level);

        // Use field names as defined in grammar
        let key = node.child_by_field("key");
        let value = node.child_by_field("value");

        let key_str = key.map_or("", |n| n.text(self.content));

        if let Some(val) = value {
            let value_str = self.format_meta_value(val, indent_level);
            format!("{indent}{key_str}: {value_str}")
        } else {
            format!("{indent}{key_str}")
        }
    }

    /// Format a meta value (string, number, array, map).
    fn format_meta_value(&self, node: Node, indent_level: usize) -> String {
        let k = node.kind();

        if k == kind::META_ARRAY {
            self.format_meta_array(node, indent_level)
        } else if k == kind::META_MAP {
            self.format_meta_map(node, indent_level)
        } else {
            // Simple value - just use the text
            node.text(self.content).to_string()
        }
    }

    /// Format a meta array.
    fn format_meta_array(&self, node: Node, indent_level: usize) -> String {
        let close_indent = self.indent(indent_level);

        let mut result = String::from("[\n");
        let items = self.collect_meta_array_items(node, indent_level + 1);
        result.push_str(&items);
        let _ = write!(result, "{close_indent}]");
        result
    }

    /// Collect formatted meta array items.
    fn collect_meta_array_items(&self, node: Node, indent_level: usize) -> String {
        let inner_indent = self.indent(indent_level);
        let mut result = String::new();
        let mut cursor = node.walk();

        for child in node.children(&mut cursor) {
            if child.kind() != "[" && child.kind() != "]" && child.kind() != "," {
                let value_str = self.format_meta_value(child, indent_level);
                let _ = writeln!(result, "{inner_indent}{value_str},");
            }
        }

        result
    }

    /// Format a meta map.
    fn format_meta_map(&self, node: Node, indent_level: usize) -> String {
        let close_indent = self.indent(indent_level);

        let mut result = String::from("{\n");
        let mut cursor = node.walk();

        for child in node.children(&mut cursor) {
            if child.kind() == kind::META_ENTRY {
                let formatted = self.format_meta_entry(child, indent_level + 1);
                result.push_str(&formatted);
                result.push_str(",\n");
            }
        }

        let _ = write!(result, "{close_indent}}}");
        result
    }

    /// Format struct definition.
    fn format_struct(&self, node: Node, indent_level: usize) -> String {
        let indent = self.indent(indent_level);

        let mut result = String::new();

        // Visibility
        if node.child_by_kind(kind::VISIBILITY_MODIFIER).is_some() {
            let _ = write!(result, "{indent}pub ");
        } else {
            result.push_str(&indent);
        }

        result.push_str("struct ");

        // Name
        if let Some(name) = node.child_by_field("name") {
            result.push_str(name.text(self.content));
        }

        // Extends clause
        if let Some(extends) = node.child_by_kind(kind::EXTENDS_CLAUSE) {
            if let Some(type_node) = extends.child_by_field("type") {
                result.push_str(" extend ");
                result.push_str(type_node.text(self.content));
            }
        }

        result.push_str(" {\n");

        // Struct body - meta block and fields
        result.push_str(&self.format_struct_body(node, indent_level));

        let _ = writeln!(result, "{indent}}}");
        result
    }

    /// Format struct body contents (meta block and fields).
    fn format_struct_body(&self, node: Node, indent_level: usize) -> String {
        let inner_indent = self.indent(indent_level + 1);
        let mut result = String::new();
        let mut cursor = node.walk();

        for child in node.children(&mut cursor) {
            match child.kind() {
                kind::META_BLOCK => {
                    let _ = writeln!(result, "{inner_indent}meta {{");
                    result.push_str(&self.format_meta_entries(child, indent_level + 2));
                    let _ = writeln!(result, "{inner_indent}}},");
                }
                kind::STRUCT_FIELD => {
                    let formatted = self.format_struct_field(child, indent_level + 1);
                    result.push_str(&formatted);
                    result.push_str(",\n");
                }
                _ => {}
            }
        }

        result
    }

    /// Format meta entries from a meta block.
    fn format_meta_entries(&self, node: Node, indent_level: usize) -> String {
        let mut result = String::new();
        let mut cursor = node.walk();

        for entry in node.children(&mut cursor) {
            if entry.kind() == kind::META_ENTRY {
                let formatted = self.format_meta_entry(entry, indent_level);
                result.push_str(&formatted);
                result.push_str(",\n");
            }
        }

        result
    }

    /// Format a struct field.
    fn format_struct_field(&self, node: Node, indent_level: usize) -> String {
        let indent = self.indent(indent_level);

        let name = node.child_by_field("name");
        let ty = node.child_by_kind(kind::TYPE);
        let meta = node.child_by_kind(kind::META_BLOCK);

        let name_str = name.map_or("", |n| n.text(self.content));
        let type_str = ty.map(|t| self.format_type(t)).unwrap_or_default();

        let mut result = format!("{indent}{name_str}: {type_str}");

        // Inline meta
        if let Some(meta_node) = meta {
            result.push_str(" meta ");
            let meta_str = self.format_inline_meta(meta_node, indent_level);
            result.push_str(&meta_str);
        }

        result
    }

    /// Format a type expression.
    fn format_type(&self, node: Node) -> String {
        let mut result = String::new();

        // Storage modifier (& or *)
        if let Some(storage) = node.child_by_kind(kind::STORAGE_MODIFIER) {
            result.push_str(storage.text(self.content));
        }

        // Mutability modifier
        if node.child_by_kind(kind::MUTABILITY_MODIFIER).is_some() {
            result.push_str("mut ");
        }

        // Type identifier
        if let Some(type_id) = node.child_by_kind(kind::TYPE_IDENTIFIER) {
            result.push_str(type_id.text(self.content));
        }

        // Generic arguments
        if let Some(generics) = node.child_by_kind(kind::GENERIC_ARGUMENTS) {
            result.push_str(&self.format_generic_args(generics));
        }

        result
    }

    /// Format generic arguments like <T, U>.
    fn format_generic_args(&self, node: Node) -> String {
        let mut result = String::from("<");
        let mut first = true;
        let mut cursor = node.walk();

        for child in node.children(&mut cursor) {
            if child.kind() != kind::TYPE {
                continue;
            }
            if !first {
                result.push_str(", ");
            }
            first = false;
            result.push_str(&self.format_type(child));
        }

        result.push('>');
        result
    }

    /// Format inline meta block (on same line or compact multi-line).
    /// node is a meta_block which contains meta_map which contains meta_entry children.
    fn format_inline_meta(&self, node: Node, indent_level: usize) -> String {
        let mut entries = Vec::new();

        // meta_block contains meta_map
        if let Some(meta_map) = node.child_by_kind(kind::META_MAP) {
            let mut cursor = meta_map.walk();
            for child in meta_map.children(&mut cursor) {
                if child.kind() == kind::META_ENTRY {
                    let name = child.child_by_field("key");
                    let value = child.child_by_field("value");

                    let name_str = name.map_or("", |n| n.text(self.content));
                    let value_str = value
                        .map(|v| self.format_meta_value(v, indent_level))
                        .unwrap_or_default();

                    entries.push(format!("{name_str}: {value_str}"));
                }
            }
        }

        // For simple inline meta, put on one line
        // For complex meta, use multiple lines
        let single_line = format!("{{ {}, }}", entries.join(", "));
        if single_line.len() < 60 && !single_line.contains('\n') {
            single_line
        } else {
            let inner_indent = self.indent(indent_level + 1);
            let close_indent = self.indent(indent_level);
            let mut result = String::from("{\n");
            for entry in entries {
                let _ = writeln!(result, "{inner_indent}{entry},");
            }
            let _ = write!(result, "{close_indent}}}");
            result
        }
    }

    /// Format enum definition.
    fn format_enum(&self, node: Node, indent_level: usize) -> String {
        let indent = self.indent(indent_level);

        let mut result = String::new();

        // Visibility
        if node.child_by_kind(kind::VISIBILITY_MODIFIER).is_some() {
            let _ = write!(result, "{indent}pub ");
        } else {
            result.push_str(&indent);
        }

        result.push_str("enum ");

        // Name
        if let Some(name) = node.child_by_field("name") {
            result.push_str(name.text(self.content));
        }

        result.push_str(" {\n");

        // Variants
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == kind::ENUM_VARIANT {
                let formatted = self.format_enum_variant(child, indent_level + 1);
                result.push_str(&formatted);
                result.push_str(",\n");
            }
        }

        let _ = writeln!(result, "{indent}}}");
        result
    }

    /// Format an enum variant.
    fn format_enum_variant(&self, node: Node, indent_level: usize) -> String {
        let indent = self.indent(indent_level);

        let name = node.child_by_field("name");
        let meta = node.child_by_kind(kind::META_BLOCK);

        let name_str = name.map_or("", |n| n.text(self.content));

        let mut result = format!("{indent}{name_str}");

        if let Some(meta_node) = meta {
            result.push_str(" meta ");
            let meta_str = self.format_inline_meta(meta_node, indent_level);
            result.push_str(&meta_str);
        }

        result
    }

    /// Format function definition.
    fn format_function(&self, node: Node, indent_level: usize) -> String {
        let indent = self.indent(indent_level);

        let mut result = String::new();

        // Visibility
        if node.child_by_kind(kind::VISIBILITY_MODIFIER).is_some() {
            let _ = write!(result, "{indent}pub fn ");
        } else {
            let _ = write!(result, "{indent}fn ");
        }

        // Function name (may include receiver type like Foo::method)
        if let Some(name) = node.child_by_kind(kind::FUNCTION_NAME) {
            result.push_str(name.text(self.content));
        }

        // Parameters (always output parens, even if empty)
        if let Some(params) = node.child_by_kind(kind::PARAMETERS) {
            result.push_str(&self.format_parameters(params, indent_level));
        } else {
            result.push_str("()");
        }

        // Return type
        if let Some(ret_type) = node.child_by_field("return_type") {
            result.push_str(": ");
            result.push_str(&self.format_type(ret_type));
        }

        result.push_str(" {\n");

        // Function body
        if let Some(body) = node.child_by_kind(kind::BLOCK) {
            let body_str = self.format_block_contents(body, indent_level + 1);
            result.push_str(&body_str);
        }

        let _ = writeln!(result, "{indent}}}");
        result
    }

    /// Format function parameters.
    fn format_parameters(&self, node: Node, indent_level: usize) -> String {
        let mut params = Vec::new();
        let mut cursor = node.walk();

        for child in node.children(&mut cursor) {
            if child.kind() == kind::PARAMETER {
                let param_str = self.format_parameter(child);
                params.push(param_str);
            }
        }

        // Decide between single-line and multi-line based on total length
        let single_line = format!("({})", params.join(", "));
        if single_line.len() < 80 {
            single_line
        } else {
            let inner_indent = self.indent(indent_level + 1);
            let close_indent = self.indent(indent_level);
            let mut result = String::from("(\n");
            for (i, param) in params.iter().enumerate() {
                let _ = write!(result, "{inner_indent}{param}");
                if i < params.len() - 1 {
                    result.push(',');
                }
                result.push('\n');
            }
            let _ = write!(result, "{close_indent})");
            result
        }
    }

    /// Format a single parameter.
    fn format_parameter(&self, node: Node) -> String {
        let name = node.child_by_field("name");
        let ty = node.child_by_kind(kind::TYPE);

        let name_str = name.map_or("", |n| n.text(self.content));
        let type_str = ty.map(|t| self.format_type(t)).unwrap_or_default();

        format!("{name_str}: {type_str}")
    }

    /// Format the contents of a block (statements inside braces).
    fn format_block_contents(&self, node: Node, indent_level: usize) -> String {
        let mut result = String::new();
        let mut cursor = node.walk();

        for child in node.children(&mut cursor) {
            if child.kind() != "{" && child.kind() != "}" {
                let formatted = self.format_statement(child, indent_level);
                result.push_str(&formatted);
                result.push('\n');
            }
        }

        result
    }

    /// Format a statement.
    fn format_statement(&self, node: Node, indent_level: usize) -> String {
        let indent = self.indent(indent_level);

        match node.kind() {
            kind::LET_STATEMENT => self.format_let_statement(node, indent_level),
            kind::LET_ELSE_STATEMENT => self.format_let_else_statement(node, indent_level),
            kind::ASSIGNMENT_STATEMENT => self.format_assignment_statement(node, indent_level),
            kind::IF_STATEMENT => self.format_if_statement(node, indent_level),
            kind::IF_LET_STATEMENT => self.format_if_let_statement(node, indent_level),
            kind::FOR_STATEMENT => self.format_for_statement(node, indent_level),
            kind::RETURN_STATEMENT => self.format_return_statement(node, indent_level),
            kind::BREAK_STATEMENT => format!("{indent}break;"),
            kind::CONTINUE_STATEMENT => format!("{indent}continue;"),
            kind::LOG_STATEMENT => self.format_log_statement(node, indent_level),
            kind::EXPRESSION_STATEMENT => self.format_expression_statement(node, indent_level),
            _ => format!("{indent}{}", node.text(self.content)),
        }
    }

    /// Format let statement.
    fn format_let_statement(&self, node: Node, indent_level: usize) -> String {
        let indent = self.indent(indent_level);

        let binding = node.child_by_kind(kind::BINDING);

        let Some(bind) = binding else {
            return format!("{indent}{}", node.text(self.content));
        };

        let name = bind.child_by_field("name");
        let ty = bind.child_by_kind(kind::TYPE);
        let mutability = bind.child_by_kind(kind::MUTABILITY_MODIFIER);

        let name_str = name.map_or("", |n| n.text(self.content));

        let bind_text = bind.text(self.content);
        let mut result = if bind_text.contains("&=") {
            format!("{indent}let {name_str} &=")
        } else if bind_text.contains("mut=") {
            format!("{indent}let {name_str} mut=")
        } else if mutability.is_some() {
            format!("{indent}let mut {name_str}")
        } else {
            format!("{indent}let {name_str}")
        };

        if let Some(t) = ty {
            result.push_str(": ");
            result.push_str(&self.format_type(t));
        }

        // Value expression (on the binding node, not the let_statement)
        if let Some(value) = bind.child_by_field("value") {
            if !bind_text.contains("&=") && !bind_text.contains("mut=") {
                result.push_str(" = ");
            } else {
                result.push(' ');
            }
            result.push_str(&self.format_expression(value));
        }

        result.push(';');
        result
    }

    /// Format let-else statement.
    fn format_let_else_statement(&self, node: Node, indent_level: usize) -> String {
        let indent = self.indent(indent_level);

        let binding = node.child_by_kind(kind::BINDING);
        let else_block = node.child_by_kind(kind::BLOCK);

        let Some(bind) = binding else {
            return format!("{indent}{}", node.text(self.content));
        };

        let name = bind.child_by_field("name");
        let name_str = name.map_or("", |n| n.text(self.content));

        let bind_text = bind.text(self.content);

        let mut result = if bind_text.contains("&=") {
            format!("{indent}let {name_str} &=")
        } else if bind_text.contains("mut=") {
            format!("{indent}let {name_str} mut=")
        } else {
            format!("{indent}let {name_str} =")
        };

        // Value expression (on the binding node, not the let_else_statement)
        if let Some(value) = bind.child_by_field("value") {
            result.push(' ');
            result.push_str(&self.format_expression(value));
        }

        result.push_str(" else { ");

        // Else block - format inline if short
        if let Some(block) = else_block {
            let block_contents = self.format_block_contents(block, 0);
            let trimmed = block_contents.trim();
            if trimmed.len() < 40 && !trimmed.contains('\n') {
                result.push_str(trimmed);
                result.push_str(" }");
            } else {
                result.push('\n');
                result.push_str(&self.format_block_contents(block, indent_level + 1));
                let _ = write!(result, "{indent}}}");
            }
        } else {
            result.push('}');
        }

        result
    }

    /// Format assignment statement.
    fn format_assignment_statement(&self, node: Node, indent_level: usize) -> String {
        let indent = self.indent(indent_level);

        let target = node.child_by_field("target");
        let value = node.child_by_field("value");

        let target_str = target.map_or("", |t| t.text(self.content));
        let value_str = value.map(|v| self.format_expression(v)).unwrap_or_default();

        format!("{indent}{target_str} = {value_str};")
    }

    /// Format if statement.
    fn format_if_statement(&self, node: Node, indent_level: usize) -> String {
        let indent = self.indent(indent_level);

        let condition = node.child_by_field("condition");
        let consequence = node.child_by_field("consequence");
        let else_clause = node.child_by_kind(kind::ELSE_CLAUSE);

        let cond_str = condition
            .map(|c| self.format_expression(c))
            .unwrap_or_default();

        let mut result = format!("{indent}if {cond_str} {{\n");

        if let Some(block) = consequence {
            result.push_str(&self.format_block_contents(block, indent_level + 1));
        }

        let _ = write!(result, "{indent}}}");

        if let Some(else_cl) = else_clause {
            result.push_str(&self.format_else_clause(else_cl, indent_level));
        }

        result
    }

    /// Format if-let statement.
    fn format_if_let_statement(&self, node: Node, indent_level: usize) -> String {
        let indent = self.indent(indent_level);

        let binding = node.child_by_kind(kind::BINDING);
        let then_block = node.child_by_kind(kind::BLOCK);
        let else_clause = node.child_by_kind(kind::ELSE_CLAUSE);

        let mut result = format!("{indent}if let ");

        if let Some(bind) = binding {
            let name = bind.child_by_field("name");
            let name_str = name.map_or("", |n| n.text(self.content));
            let bind_text = bind.text(self.content);

            if bind_text.contains("&=") {
                let _ = write!(result, "{name_str} &=");
            } else {
                let _ = write!(result, "{name_str} =");
            }
        }

        if let Some(value) = node.child_by_field("value") {
            result.push(' ');
            result.push_str(&self.format_expression(value));
        }

        result.push_str(" {\n");

        if let Some(block) = then_block {
            result.push_str(&self.format_block_contents(block, indent_level + 1));
        }

        let _ = write!(result, "{indent}}}");

        if let Some(else_cl) = else_clause {
            result.push_str(&self.format_else_clause(else_cl, indent_level));
        }

        result
    }

    /// Format else clause.
    fn format_else_clause(&self, node: Node, indent_level: usize) -> String {
        let indent = self.indent(indent_level);

        // Check what kind of else this is
        if let Some(if_stmt) = node.child_by_kind(kind::IF_STATEMENT) {
            // else if
            let mut result = String::from("\nelse ");
            let if_formatted = self.format_if_statement(if_stmt, 0);
            result.push_str(if_formatted.trim_start());
            result
        } else if let Some(if_let_stmt) = node.child_by_kind(kind::IF_LET_STATEMENT) {
            // else if let
            let mut result = String::from("\nelse ");
            let if_let_formatted = self.format_if_let_statement(if_let_stmt, 0);
            result.push_str(if_let_formatted.trim_start());
            result
        } else if let Some(block) = node.child_by_kind(kind::BLOCK) {
            // plain else
            let mut result = format!("\n{indent}else {{\n");
            result.push_str(&self.format_block_contents(block, indent_level + 1));
            let _ = write!(result, "{indent}}}");
            result
        } else {
            String::new()
        }
    }

    /// Format for statement.
    fn format_for_statement(&self, node: Node, indent_level: usize) -> String {
        let indent = self.indent(indent_level);

        let var = node.child_by_field("variable");
        let iter = node.child_by_field("iterator");
        let body = node.child_by_field("body");

        let var_str = var.map_or("", |v| v.text(self.content));
        let iter_str = iter.map(|i| self.format_expression(i)).unwrap_or_default();

        let mut result = format!("{indent}for {var_str} in {iter_str} {{\n");

        if let Some(block) = body {
            result.push_str(&self.format_block_contents(block, indent_level + 1));
        }

        let _ = write!(result, "{indent}}}");
        result
    }

    /// Format return statement.
    fn format_return_statement(&self, node: Node, indent_level: usize) -> String {
        let indent = self.indent(indent_level);

        // Find the expression child (skip 'return' keyword and ';')
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            let k = child.kind();
            if k != "return" && k != ";" {
                let expr_str = self.format_expression(child);
                return format!("{indent}return {expr_str};");
            }
        }

        format!("{indent}return;")
    }

    /// Format log statement.
    fn format_log_statement(&self, node: Node, indent_level: usize) -> String {
        let indent = self.indent(indent_level);

        let mut args = Vec::new();
        let mut cursor = node.walk();
        let mut first = true;

        for child in node.children(&mut cursor) {
            // Skip 'log', '(', ')', ',', ';'
            let k = child.kind();
            if k != "log" && k != "(" && k != ")" && k != "," && k != ";" {
                if first {
                    // First is the format string
                    args.push(child.text(self.content).to_string());
                    first = false;
                } else {
                    args.push(self.format_expression(child));
                }
            }
        }

        format!("{indent}log({});", args.join(", "))
    }

    /// Format expression statement.
    fn format_expression_statement(&self, node: Node, indent_level: usize) -> String {
        let indent = self.indent(indent_level);

        // Get the expression child
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() != ";" {
                let expr_str = self.format_expression(child);
                return format!("{indent}{expr_str};");
            }
        }

        format!("{indent}{}", node.text(self.content))
    }

    /// Format const declaration.
    fn format_const(&self, node: Node, indent_level: usize) -> String {
        let indent = self.indent(indent_level);

        let name = node.child_by_field("name");
        let ty = node.child_by_kind(kind::TYPE);
        let value = node.child_by_field("value");

        let name_str = name.map_or("", |n| n.text(self.content));
        let type_str = ty.map(|t| self.format_type(t)).unwrap_or_default();
        let value_str = value.map(|v| self.format_expression(v)).unwrap_or_default();

        format!("{indent}const {name_str}: {type_str} = {value_str};\n")
    }

    /// Format an expression.
    fn format_expression(&self, node: Node) -> String {
        match node.kind() {
            kind::NUMBER | kind::BOOLEAN | kind::STRING_LITERAL | kind::TIME_LITERAL => {
                node.text(self.content).to_string()
            }
            kind::PATH_EXPRESSION => self.format_path_expression(node),
            kind::BINARY_EXPRESSION => self.format_binary_expression(node),
            kind::UNARY_EXPRESSION => self.format_unary_expression(node),
            kind::CALL_EXPRESSION => self.format_call_expression(node),
            kind::FIELD_ACCESS => self.format_field_access(node),
            kind::PARENTHESIZED_EXPRESSION => self.format_parenthesized_expression(node),
            _ => node.text(self.content).to_string(),
        }
    }

    /// Format path expression (e.g., Foo::Bar).
    fn format_path_expression(&self, node: Node) -> String {
        let mut segments = Vec::new();
        let mut cursor = node.walk();

        for child in node.children(&mut cursor) {
            if child.kind() == kind::PATH_SEGMENT {
                segments.push(child.text(self.content));
            }
        }

        segments.join("::")
    }

    /// Format binary expression.
    fn format_binary_expression(&self, node: Node) -> String {
        // Binary expressions have no named fields, just: left, operator, right
        // The operator is an anonymous token child
        let mut cursor = node.walk();
        let children: Vec<Node> = node.children(&mut cursor).collect();

        // Find the operator - it's the anonymous string node (like "+", "-", etc.)
        let mut left = None;
        let mut op = None;
        let mut right = None;

        for child in &children {
            if child.is_named() {
                if left.is_none() {
                    left = Some(*child);
                } else {
                    right = Some(*child);
                }
            } else if !child.kind().is_empty() && left.is_some() && op.is_none() {
                // This is the operator (anonymous token between left and right)
                op = Some(*child);
            }
        }

        let left_str = left.map(|l| self.format_expression(l)).unwrap_or_default();
        let op_str = op.map_or("", |o| o.text(self.content));
        let right_str = right.map(|r| self.format_expression(r)).unwrap_or_default();

        format!("{left_str} {op_str} {right_str}")
    }

    /// Format unary expression.
    fn format_unary_expression(&self, node: Node) -> String {
        // Unary expressions have no named fields: operator, operand
        let mut cursor = node.walk();
        let children: Vec<Node> = node.children(&mut cursor).collect();

        let mut op = None;
        let mut operand = None;

        for child in &children {
            if child.is_named() {
                operand = Some(*child);
            } else if !child.kind().is_empty() && op.is_none() {
                op = Some(*child);
            }
        }

        let op_str = op.map_or("", |o| o.text(self.content));
        let operand_str = operand
            .map(|o| self.format_expression(o))
            .unwrap_or_default();

        format!("{op_str}{operand_str}")
    }

    /// Format call expression.
    fn format_call_expression(&self, node: Node) -> String {
        let callee = node.child_by_field("function");
        let callee_str = callee
            .map(|c| self.format_expression(c))
            .unwrap_or_default();

        // Type arguments
        let type_args_str = node
            .child_by_kind(kind::GENERIC_ARGUMENTS)
            .map(|ta| self.format_generic_args(ta))
            .unwrap_or_default();

        // Arguments are in an 'arguments' child node
        let args = node
            .child_by_kind("arguments")
            .map(|args_node| self.collect_call_args(args_node))
            .unwrap_or_default();

        format!("{callee_str}{type_args_str}({})", args.join(", "))
    }

    /// Collect call expression arguments from the 'arguments' node.
    fn collect_call_args(&self, node: Node) -> Vec<String> {
        let mut args = Vec::new();
        let mut cursor = node.walk();

        for child in node.children(&mut cursor) {
            // Skip commas, collect expressions
            if child.kind() != "," {
                args.push(self.format_expression(child));
            }
        }

        args
    }

    /// Format field access expression.
    fn format_field_access(&self, node: Node) -> String {
        let object = node.child_by_field("object");
        let field = node.child_by_field("field");

        let obj_str = object
            .map(|o| self.format_expression(o))
            .unwrap_or_default();
        let field_str = field.map_or("", |f| f.text(self.content));

        format!("{obj_str}.{field_str}")
    }

    /// Format parenthesized expression.
    fn format_parenthesized_expression(&self, node: Node) -> String {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() != "(" && child.kind() != ")" {
                let inner = self.format_expression(child);
                return format!("({inner})");
            }
        }
        node.text(self.content).to_string()
    }

    /// Format after a closing brace was typed.
    fn format_after_close_brace(
        &self,
        doc: &Document,
        tree: &nimbyscript_parser::Tree,
        offset: usize,
    ) -> Vec<TextEdit> {
        // Find the node at the position
        let root = tree.root_node();

        // Find the node that contains this offset
        if let Some(node) = Self::find_node_ending_at(root, offset) {
            // Format the entire enclosing item
            let parent = Self::find_formattable_parent(node);
            if let Some(p) = parent {
                let formatted = self.format_top_level_item(p, 0);
                let start_pos = doc.offset_to_position(p.start_byte());
                let end_pos = doc.offset_to_position(p.end_byte());

                return vec![TextEdit {
                    range: Range {
                        start: start_pos,
                        end: end_pos,
                    },
                    new_text: formatted,
                }];
            }
        }

        Vec::new()
    }

    /// Format after a semicolon was typed.
    fn format_after_semicolon(
        &self,
        doc: &Document,
        tree: &nimbyscript_parser::Tree,
        offset: usize,
    ) -> Vec<TextEdit> {
        // Find and format the statement that just ended
        let root = tree.root_node();

        if let Some(node) = Self::find_node_ending_at(root, offset) {
            // Find the enclosing statement
            if let Some(stmt) = Self::find_enclosing_statement(node) {
                let start_pos = doc.offset_to_position(stmt.start_byte());
                let end_pos = doc.offset_to_position(stmt.end_byte());

                // Calculate indent level based on position
                let line_start = self.find_line_start(stmt.start_byte());
                let current_indent = self.count_leading_whitespace(line_start);
                let indent_level = current_indent / self.config.tab_size as usize;

                let formatted = self.format_statement(stmt, indent_level);

                return vec![TextEdit {
                    range: Range {
                        start: start_pos,
                        end: end_pos,
                    },
                    new_text: formatted,
                }];
            }
        }

        Vec::new()
    }

    /// Find a node that ends at or near the given offset.
    fn find_node_ending_at<'b>(root: Node<'b>, offset: usize) -> Option<Node<'b>> {
        let mut best: Option<Node<'b>> = None;
        Self::find_node_ending_at_recursive(root, offset, &mut best);
        best
    }

    fn find_node_ending_at_recursive<'b>(
        node: Node<'b>,
        offset: usize,
        best: &mut Option<Node<'b>>,
    ) {
        let end = node.end_byte();

        // If this node ends at or just before the offset, it's a candidate
        if end <= offset && end > offset.saturating_sub(2) {
            *best = Some(node);
        }

        // Search children
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.start_byte() <= offset {
                Self::find_node_ending_at_recursive(child, offset, best);
            }
        }
    }

    /// Find a parent node that can be formatted as a unit.
    fn find_formattable_parent(node: Node<'_>) -> Option<Node<'_>> {
        let mut current = Some(node);

        while let Some(n) = current {
            match n.kind() {
                kind::STRUCT_DEFINITION
                | kind::ENUM_DEFINITION
                | kind::FUNCTION_DEFINITION
                | kind::SCRIPT_META => return Some(n),
                _ => current = n.parent(),
            }
        }

        None
    }

    /// Find the enclosing statement.
    fn find_enclosing_statement(node: Node<'_>) -> Option<Node<'_>> {
        let mut current = Some(node);

        while let Some(n) = current {
            match n.kind() {
                kind::LET_STATEMENT
                | kind::LET_ELSE_STATEMENT
                | kind::ASSIGNMENT_STATEMENT
                | kind::IF_STATEMENT
                | kind::IF_LET_STATEMENT
                | kind::FOR_STATEMENT
                | kind::RETURN_STATEMENT
                | kind::BREAK_STATEMENT
                | kind::CONTINUE_STATEMENT
                | kind::LOG_STATEMENT
                | kind::EXPRESSION_STATEMENT => return Some(n),
                _ => current = n.parent(),
            }
        }

        None
    }

    /// Find the start of the line containing the given offset.
    fn find_line_start(&self, offset: usize) -> usize {
        let bytes = self.content.as_bytes();
        let mut pos = offset;

        while pos > 0 && bytes[pos - 1] != b'\n' {
            pos -= 1;
        }

        pos
    }

    /// Count leading whitespace characters from a position.
    fn count_leading_whitespace(&self, start: usize) -> usize {
        let bytes = self.content.as_bytes();
        let mut count = 0;
        let mut pos = start;

        while pos < bytes.len() {
            match bytes[pos] {
                b' ' => count += 1,
                b'\t' => count += self.config.tab_size as usize,
                _ => break,
            }
            pos += 1;
        }

        count
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn format(code: &str) -> String {
        let doc = Document::new(code.to_string(), None);
        let config = FormattingConfig::default();
        let edits = format_document(&doc, &config);

        // Apply edits (there's only one that replaces the whole document)
        if let Some(edit) = edits.first() {
            edit.new_text.clone()
        } else {
            code.to_string()
        }
    }

    #[test]
    fn test_format_script_meta() {
        let code = "script meta{lang:nimbyscript.v1,api:nimbyrails.v1,}";
        let formatted = format(code);
        eprintln!("Formatted:\n{formatted}");

        assert!(formatted.contains("script meta {"));
        assert!(formatted.contains("    lang: nimbyscript.v1,"));
        assert!(formatted.contains("    api: nimbyrails.v1,"));
    }

    #[test]
    fn test_format_struct() {
        let code = "pub struct Foo extend Bar{field1:i64,field2:f64,}";
        let formatted = format(code);

        assert!(formatted.contains("pub struct Foo extend Bar {"));
        assert!(formatted.contains("    field1: i64,"));
        assert!(formatted.contains("    field2: f64,"));
    }

    #[test]
    fn test_format_enum() {
        let code = "enum MyEnum{Variant1,Variant2,}";
        let formatted = format(code);

        assert!(formatted.contains("enum MyEnum {"));
        assert!(formatted.contains("    Variant1,"));
        assert!(formatted.contains("    Variant2,"));
    }

    #[test]
    fn test_format_function() {
        let code = "fn test(x:i64):bool{return true;}";
        let formatted = format(code);
        eprintln!("Formatted function:\n{formatted}");

        assert!(formatted.contains("fn test(x: i64): bool {"));
        assert!(formatted.contains("    return true;"));
    }

    #[test]
    fn test_format_preserves_comments() {
        let code = "// This is a comment\npub struct Foo {}";
        let formatted = format(code);

        assert!(formatted.contains("// This is a comment"));
    }

    #[test]
    fn test_format_trailing_commas() {
        let code = "pub struct Foo{field1:i64}";
        let formatted = format(code);

        // Should add trailing comma
        assert!(formatted.contains("field1: i64,"));
    }

    #[test]
    fn test_format_binary_expression() {
        let code = "fn test(){let x=1+2;}";
        let formatted = format(code);

        assert!(formatted.contains("let x = 1 + 2;"));
    }

    #[test]
    fn test_format_if_statement() {
        let code = "fn test(){if x>0{return true;}}";
        let formatted = format(code);

        assert!(formatted.contains("if x > 0 {"));
        assert!(formatted.contains("        return true;"));
    }

    #[test]
    fn test_format_for_statement() {
        let code = "fn test(){for item in items{log(item);}}";
        let formatted = format(code);

        assert!(formatted.contains("for item in items {"));
    }

    #[test]
    fn test_final_newline() {
        let code = "pub struct Foo{}";
        let formatted = format(code);

        assert!(formatted.ends_with('\n'));
    }

    #[test]
    fn test_format_method_multiline_params() {
        let code =
            "pub fn Foo::method(self:&Foo,ctx:&Context,train:&Train,motion:&Motion):bool{return true;}";
        let formatted = format(code);

        // Long parameter lists should be multi-line
        assert!(formatted.contains("pub fn Foo::method("));
    }

    #[test]
    fn test_format_inline_meta() {
        let code = "pub struct Foo{field:i64 meta{label:\"Test\",},}";
        let formatted = format(code);

        // Short inline meta stays on one line
        assert!(
            formatted.contains("meta { label: \"Test\", }")
                || formatted.contains("meta {\n        label: \"Test\",\n    }")
        );
    }

    #[test]
    fn test_format_let_else() {
        let code = "fn test(){let x &= foo.bar() else{return;}}";
        let formatted = format(code);

        assert!(formatted.contains("let x &="));
        assert!(formatted.contains("else {"));
    }

    #[test]
    fn test_format_path_expression() {
        let code = "fn test(){return Foo::Bar::Baz;}";
        let formatted = format(code);

        assert!(formatted.contains("Foo::Bar::Baz"));
    }

    #[test]
    fn test_format_call_expression() {
        let code = "fn test(){foo.bar(a,b,c);}";
        let formatted = format(code);

        assert!(formatted.contains("foo.bar(a, b, c);"));
    }
}
