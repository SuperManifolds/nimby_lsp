//! Shared test utilities for nimbyscript-lsp tests.

use crate::document::Document;
use nimbyscript_analyzer::ApiDefinitions;
use tower_lsp::lsp_types::{Position, Url};

/// Create a Document from source code.
pub fn make_doc(source: &str) -> Document {
    Document::new(source.to_string(), None)
}

/// Load the API definitions from the embedded TOML file.
pub fn load_api() -> ApiDefinitions {
    let toml = include_str!("../../../api-definitions/nimbyrails.v1.toml");
    ApiDefinitions::load_from_str(toml).expect("should parse API definitions")
}

/// Create a file:// URL for testing.
pub fn make_uri(name: &str) -> Url {
    Url::parse(&format!("file:///{name}.nimbyscript")).expect("valid url")
}

/// Create a Position from line and character.
pub fn pos(line: u32, character: u32) -> Position {
    Position { line, character }
}
