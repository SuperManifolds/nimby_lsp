//! Struct and enum validation pass (E05xx).
//!
//! Validates struct and enum definitions including:
//! - E0500: Struct must extend a valid game type
//! - E0501: Invalid field type for pub struct
//! - E0502: Duplicate struct definition
//! - E0503: Duplicate enum definition
//! - E0504: Duplicate field in struct
//! - E0505: Duplicate variant in enum
//! - E0506: Duplicate function definition
//! - E0507: Invalid field type for private struct
//! - E0508: Method defined on private struct

use std::collections::HashSet;

use nimbyscript_parser::ast::Span;
use nimbyscript_parser::{kind, Node, NodeExt};

use crate::semantic::{SemanticContext, SemanticPass};
use crate::types::parse_type_string;
use crate::Diagnostic;

/// Pass that validates struct and enum definitions
pub struct StructValidationPass;

impl SemanticPass for StructValidationPass {
    fn name(&self) -> &'static str {
        "struct_validation"
    }

    fn run(&self, ctx: &mut SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
        let root = ctx.tree.root_node();
        validate_definitions(root, ctx, diagnostics);
    }
}

fn validate_definitions(node: Node, ctx: &SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    match node.kind() {
        kind::STRUCT_DEFINITION => validate_struct(node, ctx, diagnostics),
        kind::ENUM_DEFINITION => validate_enum(node, ctx, diagnostics),
        kind::FUNCTION_DEFINITION => validate_function(node, ctx, diagnostics),
        _ => {
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                validate_definitions(child, ctx, diagnostics);
            }
        }
    }
}

fn validate_struct(node: Node, ctx: &SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    let Some(name_node) = node.child_by_field("name") else {
        return;
    };
    let name = name_node.text(ctx.source);

    // Check if this is a pub struct
    let is_pub = node.child_by_kind(kind::VISIBILITY_MODIFIER).is_some();

    // E0500: Validate extends clause for pub structs
    if is_pub {
        if let Some(extends_clause) = node.child_by_kind(kind::EXTENDS_CLAUSE) {
            if let Some(type_node) = extends_clause.child_by_field("type") {
                let extends_type = type_node.text(ctx.source);
                if !ctx.is_extendable_game_type(extends_type) {
                    diagnostics.push(
                        Diagnostic::error(
                            format!(
                                "Struct '{name}' extends '{extends_type}', which is not a valid extendable game type. \
                                Valid types: Signal, Train, Script, Station, Line, Schedule, Tag"
                            ),
                            Span::new(type_node.start_byte(), type_node.end_byte()),
                        )
                        .with_code("E0500"),
                    );
                }
            }
        } else {
            // Pub struct without extends clause
            diagnostics.push(
                Diagnostic::error(
                    format!(
                        "Public struct '{name}' must extend a game type (Signal, Train, Script, etc.)"
                    ),
                    Span::new(name_node.start_byte(), name_node.end_byte()),
                )
                .with_code("E0500"),
            );
        }
    }

    // Check for duplicate fields (E0504)
    let mut seen_fields = HashSet::new();
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == kind::STRUCT_FIELD {
            if let Some(field_name_node) = child.child_by_field("name") {
                let field_name = field_name_node.text(ctx.source);
                if !seen_fields.insert(field_name.to_string()) {
                    diagnostics.push(
                        Diagnostic::error(
                            format!("Duplicate field '{field_name}' in struct '{name}'"),
                            Span::new(field_name_node.start_byte(), field_name_node.end_byte()),
                        )
                        .with_code("E0504"),
                    );
                }

                // E0501: Validate field type for pub structs
                // E0507: Validate field type for private structs
                if is_pub {
                    validate_pub_field_type(child, ctx, diagnostics);
                } else {
                    validate_private_field_type(child, ctx, diagnostics);
                }
            }
        }
    }
}

fn validate_pub_field_type(field: Node, ctx: &SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    let Some(type_node) = field.child_by_field("type") else {
        return;
    };
    let type_str = type_node.text(ctx.source);
    let type_info = parse_type_string(type_str);

    if !ctx.is_valid_pub_struct_field_type(&type_info) {
        diagnostics.push(
            Diagnostic::error(
                format!(
                    "Invalid field type '{type_str}' for pub struct. \
                    Allowed types: bool, i64, f64, script enums, ID<Line/Train/Schedule/Signal/Tag>, \
                    &Vec<ID<Line/Train/Schedule/Signal>>"
                ),
                Span::new(type_node.start_byte(), type_node.end_byte()),
            )
            .with_code("E0501"),
        );
    }
}

fn validate_private_field_type(
    field: Node,
    ctx: &SemanticContext,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let Some(type_node) = field.child_by_field("type") else {
        return;
    };
    let type_str = type_node.text(ctx.source);
    let type_info = parse_type_string(type_str);

    if !ctx.is_valid_private_struct_field_type(&type_info) {
        diagnostics.push(
            Diagnostic::error(
                format!(
                    "Invalid field type '{type_str}' for private struct. \
                    Allowed types: bool, i64, f64, script enums, \
                    ID<Line/Train/Schedule/Signal/Tag/Track/Building/Station>"
                ),
                Span::new(type_node.start_byte(), type_node.end_byte()),
            )
            .with_code("E0507"),
        );
    }
}

fn validate_function(node: Node, ctx: &SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    let Some(name_node) = node.child_by_field("name") else {
        return;
    };
    let name = name_node.text(ctx.source);

    // Check if this is a method (contains ::)
    if let Some(sep_pos) = name.find("::") {
        let struct_name = &name[..sep_pos];

        // Check if the function is public
        let is_pub = node.child_by_kind(kind::VISIBILITY_MODIFIER).is_some();

        // E0508: Private functions on private structs are not allowed
        // (pub fn on private structs is allowed for task callbacks)
        if !is_pub && ctx.is_user_struct(struct_name) && !ctx.is_pub_struct(struct_name) {
            diagnostics.push(
                Diagnostic::error(
                    format!(
                        "Cannot define private method on private struct '{struct_name}'. \
                         Use 'pub fn' for task callbacks, or define methods on pub structs."
                    ),
                    Span::new(name_node.start_byte(), name_node.end_byte()),
                )
                .with_code("E0508"),
            );
        }
    }
}

fn validate_enum(node: Node, ctx: &SemanticContext, diagnostics: &mut Vec<Diagnostic>) {
    let Some(name_node) = node.child_by_field("name") else {
        return;
    };
    let name = name_node.text(ctx.source);

    // Check for duplicate variants (E0505)
    let mut seen_variants = HashSet::new();
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == kind::ENUM_VARIANT {
            if let Some(variant_name_node) = child.child_by_field("name") {
                let variant_name = variant_name_node.text(ctx.source);
                if !seen_variants.insert(variant_name.to_string()) {
                    diagnostics.push(
                        Diagnostic::error(
                            format!("Duplicate variant '{variant_name}' in enum '{name}'"),
                            Span::new(variant_name_node.start_byte(), variant_name_node.end_byte()),
                        )
                        .with_code("E0505"),
                    );
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::api::ApiDefinitions;
    use crate::diagnostics::Severity;
    use crate::semantic::context::collect_declarations;
    use nimbyscript_parser::parse;

    fn load_api() -> ApiDefinitions {
        let toml = include_str!("../../../../../api-definitions/nimbyrails.v1.toml");
        ApiDefinitions::load_from_str(toml).expect("should parse")
    }

    fn check(source: &str) -> Vec<Diagnostic> {
        let tree = parse(source);
        let api = load_api();
        let mut ctx = SemanticContext::new(source, &tree, &api);
        collect_declarations(&mut ctx);
        let mut diagnostics = Vec::new();
        StructValidationPass.run(&mut ctx, &mut diagnostics);
        diagnostics
    }

    fn errors(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
        diags
            .iter()
            .filter(|d| matches!(d.severity, Severity::Error))
            .collect()
    }

    // E0500 tests - invalid extends

    #[test]
    fn test_valid_extends_signal() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal { }
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0500")),
            "Valid extends should not error: {errs:?}"
        );
    }

    #[test]
    fn test_valid_extends_train() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Train { }
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0500")),
            "Valid extends should not error: {errs:?}"
        );
    }

    #[test]
    fn test_invalid_extends_type() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Motion { }
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(errs.iter().any(|d| d.code.as_deref() == Some("E0500")));
    }

    #[test]
    fn test_pub_struct_without_extends() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test { }
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(errs.iter().any(|d| d.code.as_deref() == Some("E0500")));
    }

    #[test]
    fn test_private_struct_without_extends_ok() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
struct PrivateData { x: i64, }
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0500")),
            "Private struct without extends should be ok: {errs:?}"
        );
    }

    // E0501 tests - invalid field types

    #[test]
    fn test_valid_field_types() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal {
    a: bool,
    b: i64,
    c: f64,
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0501")),
            "Valid field types should not error: {errs:?}"
        );
    }

    #[test]
    fn test_invalid_string_field() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal {
    name: String,
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(errs.iter().any(|d| d.code.as_deref() == Some("E0501")));
    }

    #[test]
    fn test_valid_id_field() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal {
    sig: ID<Signal>,
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0501")),
            "ID<Signal> should be valid: {errs:?}"
        );
    }

    #[test]
    fn test_valid_vec_id_field() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal {
    lines: &Vec<ID<Line>>,
    trains: &Vec<ID<Train>>,
    schedules: &Vec<ID<Schedule>>,
    signals: &Vec<ID<Signal>>,
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0501")),
            "&Vec<ID<T>> should be valid for Line, Train, Schedule, Signal: {errs:?}"
        );
    }

    #[test]
    fn test_invalid_vec_id_tag_field() {
        // Tag is valid for ID<Tag> but NOT for &Vec<ID<Tag>>
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal {
    tags: &Vec<ID<Tag>>,
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0501")),
            "&Vec<ID<Tag>> should be invalid"
        );
    }

    #[test]
    fn test_invalid_vec_without_ref() {
        // Vec<ID<T>> without & is not valid
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal {
    lines: Vec<ID<Line>>,
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0501")),
            "Vec<ID<T>> without & should be invalid"
        );
    }

    // E0504 tests - duplicate fields

    #[test]
    fn test_duplicate_field() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal {
    count: i64,
    count: f64,
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(errs.iter().any(|d| d.code.as_deref() == Some("E0504")));
    }

    #[test]
    fn test_unique_fields_ok() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct Test extend Signal {
    count: i64,
    value: f64,
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0504")),
            "Unique fields should not error: {errs:?}"
        );
    }

    // E0505 tests - duplicate enum variants

    #[test]
    fn test_duplicate_enum_variant() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub enum Status { Active, Active, }
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(errs.iter().any(|d| d.code.as_deref() == Some("E0505")));
    }

    #[test]
    fn test_unique_enum_variants_ok() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub enum Status { Active, Inactive, }
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0505")),
            "Unique variants should not error: {errs:?}"
        );
    }

    // E0507 tests - invalid field types for private structs

    #[test]
    fn test_private_struct_direct_game_type_invalid() {
        // Direct game types like Train are not allowed - must use ID<Train>
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
struct HitchInfo {
    hitched_train: Train,
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0507")),
            "Direct Train type should be invalid for private struct"
        );
    }

    #[test]
    fn test_private_struct_id_train_valid() {
        // ID<Train> is valid for private structs (same as pub structs)
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
struct HitchInfo {
    train_id: ID<Train>,
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0507")),
            "ID<Train> should be valid for private struct: {errs:?}"
        );
    }

    #[test]
    fn test_private_struct_id_track_valid() {
        // ID<Track> is allowed in private structs
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
struct TrackInfo {
    track: ID<Track>,
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0507")),
            "ID<Track> should be valid for private struct: {errs:?}"
        );
    }

    #[test]
    fn test_private_struct_id_building_valid() {
        // ID<Building> is allowed in private structs
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
struct BuildingInfo {
    building: ID<Building>,
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0507")),
            "ID<Building> should be valid for private struct: {errs:?}"
        );
    }

    #[test]
    fn test_private_struct_id_station_valid() {
        // ID<Station> is allowed in private structs
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
struct StationInfo {
    station: ID<Station>,
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0507")),
            "ID<Station> should be valid for private struct: {errs:?}"
        );
    }

    #[test]
    fn test_private_struct_primitives_valid() {
        // Primitives are allowed in private structs
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
struct Data {
    flag: bool,
    count: i64,
    value: f64,
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0507")),
            "Primitives should be valid for private struct: {errs:?}"
        );
    }

    #[test]
    fn test_private_struct_user_enum_valid() {
        // User-defined enums are allowed in private structs
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
enum Status { Active, Inactive, }
struct Data {
    status: Status,
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0507")),
            "User enum should be valid for private struct: {errs:?}"
        );
    }

    #[test]
    fn test_private_struct_vec_invalid() {
        // &Vec<ID<T>> is pub-only
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
struct Data {
    tracks: &Vec<ID<Track>>,
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0507")),
            "&Vec<ID<T>> should be invalid for private struct"
        );
    }

    #[test]
    fn test_private_struct_string_invalid() {
        // String is not a valid field type
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
struct Data {
    name: String,
}
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0507")),
            "String should be invalid for private struct"
        );
    }

    // E0508 tests - private method on private struct

    #[test]
    fn test_private_method_on_private_struct_error() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
struct MyData { x: i64, }
fn MyData::foobar() { }
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().any(|d| d.code.as_deref() == Some("E0508")),
            "Private method on private struct should error: {errs:?}"
        );
    }

    #[test]
    fn test_pub_method_on_private_struct_ok() {
        // pub fn on private struct is allowed for task callbacks
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
struct MyTask { x: i64, }
pub fn MyTask::task_run(self: &MyTask, ctx: &ControlCtx, sc: &mut SimController) { }
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0508")),
            "Pub method on private struct should be ok (task callback): {errs:?}"
        );
    }

    #[test]
    fn test_method_on_pub_struct_ok() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
pub struct MySignal extend Signal { }
fn MySignal::foobar() { }
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0508")),
            "Method on pub struct should be ok: {errs:?}"
        );
    }

    #[test]
    fn test_plain_function_ok() {
        let source = r"
script meta { lang: nimbyscript.v1, api: nimbyrails.v1, }
fn helper() { }
";
        let diags = check(source);
        let errs = errors(&diags);
        assert!(
            errs.iter().all(|d| d.code.as_deref() != Some("E0508")),
            "Plain function should not get E0508: {errs:?}"
        );
    }
}
