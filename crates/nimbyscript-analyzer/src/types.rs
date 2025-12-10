//! Type system for NimbyScript semantic analysis.
//!
//! Provides type representation and operations for semantic passes.

use std::fmt;

/// Represents a type in NimbyScript's type system.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeInfo {
    // Primitives
    Bool,
    I64,
    F64,
    String,

    // User-defined types
    Struct {
        name: String,
        /// The game type this struct extends (e.g., "Signal", "Train")
        extends: Option<String>,
    },
    Enum {
        name: String,
    },

    // Compound types
    Reference {
        inner: Box<TypeInfo>,
        is_mut: bool,
    },
    Pointer {
        inner: Box<TypeInfo>,
        is_mut: bool,
    },
    /// Generic type like `ID<Signal>` or `std::optional<i64>`
    Generic {
        name: String,
        args: Vec<TypeInfo>,
    },
    /// Array type like `[f64; 3]`
    Array {
        element: Box<TypeInfo>,
        size: usize,
    },

    // Special types
    Void,
    /// Unknown type (for error recovery)
    Unknown,
    /// Never type (for unreachable code paths)
    Never,
    /// Self type in method contexts
    SelfType,
}

impl TypeInfo {
    /// Create a simple struct type
    pub fn struct_type(name: impl Into<String>) -> Self {
        TypeInfo::Struct {
            name: name.into(),
            extends: None,
        }
    }

    /// Create a struct type that extends a game type
    pub fn struct_extending(name: impl Into<String>, extends: impl Into<String>) -> Self {
        TypeInfo::Struct {
            name: name.into(),
            extends: Some(extends.into()),
        }
    }

    /// Create an enum type
    pub fn enum_type(name: impl Into<String>) -> Self {
        TypeInfo::Enum { name: name.into() }
    }

    /// Create a reference type
    pub fn reference(inner: TypeInfo, is_mut: bool) -> Self {
        TypeInfo::Reference {
            inner: Box::new(inner),
            is_mut,
        }
    }

    /// Create a pointer type
    pub fn pointer(inner: TypeInfo, is_mut: bool) -> Self {
        TypeInfo::Pointer {
            inner: Box::new(inner),
            is_mut,
        }
    }

    /// Create a generic type like `ID<Signal>`
    pub fn generic(name: impl Into<String>, args: Vec<TypeInfo>) -> Self {
        TypeInfo::Generic {
            name: name.into(),
            args,
        }
    }

    /// Create an optional type `std::optional<T>`
    pub fn optional(inner: TypeInfo) -> Self {
        TypeInfo::Generic {
            name: "std::optional".to_string(),
            args: vec![inner],
        }
    }

    /// Create an array type `[T; N]`
    pub fn array(element: TypeInfo, size: usize) -> Self {
        TypeInfo::Array {
            element: Box::new(element),
            size,
        }
    }

    /// Check if this type is numeric (i64 or f64)
    pub fn is_numeric(&self) -> bool {
        matches!(self, TypeInfo::I64 | TypeInfo::F64)
    }

    /// Check if this type is an integer
    pub fn is_integer(&self) -> bool {
        matches!(self, TypeInfo::I64)
    }

    /// Check if this type is a float
    pub fn is_float(&self) -> bool {
        matches!(self, TypeInfo::F64)
    }

    /// Check if this type is boolean
    pub fn is_bool(&self) -> bool {
        matches!(self, TypeInfo::Bool)
    }

    /// Check if this type is a reference type
    pub fn is_reference(&self) -> bool {
        matches!(self, TypeInfo::Reference { .. })
    }

    /// Check if this type is a pointer type
    pub fn is_pointer(&self) -> bool {
        matches!(self, TypeInfo::Pointer { .. })
    }

    /// Check if this type is unknown (used for error recovery)
    pub fn is_unknown(&self) -> bool {
        matches!(self, TypeInfo::Unknown)
    }

    /// Get the inner type for reference/pointer types
    pub fn inner_type(&self) -> Option<&TypeInfo> {
        match self {
            TypeInfo::Reference { inner, .. } | TypeInfo::Pointer { inner, .. } => Some(inner),
            _ => None,
        }
    }

    /// Check if this type can be compared with another (using ==, !=, <, >, etc.)
    /// NimbyScript allows comparing i64 and f64, but not most other cross-type comparisons
    pub fn can_compare_with(&self, other: &TypeInfo) -> bool {
        // Unknown types are permissive for error recovery
        if self.is_unknown() || other.is_unknown() {
            return true;
        }

        // Same types can always be compared
        if self == other {
            return true;
        }

        // Numeric types can be compared with each other
        if self.is_numeric() && other.is_numeric() {
            return true;
        }

        // References to comparable types
        match (self, other) {
            (TypeInfo::Reference { inner: a, .. }, TypeInfo::Reference { inner: b, .. }) => {
                a.can_compare_with(b)
            }
            _ => false,
        }
    }

    /// Check if this type can participate in arithmetic with another type.
    /// NimbyScript is strict: i64 + f64 is NOT allowed without explicit conversion.
    pub fn can_arithmetic_with(&self, other: &TypeInfo) -> bool {
        // Unknown types are permissive for error recovery
        if self.is_unknown() || other.is_unknown() {
            return true;
        }

        // Must be the same numeric type
        match (self, other) {
            (TypeInfo::I64, TypeInfo::I64) => true,
            (TypeInfo::F64, TypeInfo::F64) => true,
            _ => false,
        }
    }

    /// Check if this type is assignable to a target type.
    /// This is the core type compatibility check.
    pub fn is_assignable_to(&self, target: &TypeInfo) -> bool {
        // Unknown types are permissive for error recovery
        if self.is_unknown() || target.is_unknown() {
            return true;
        }

        // Never can be assigned to anything (unreachable code)
        if matches!(self, TypeInfo::Never) {
            return true;
        }

        // Exact match
        if self == target {
            return true;
        }

        // SelfType matches any struct type in method context
        if matches!(target, TypeInfo::SelfType) {
            return matches!(self, TypeInfo::Struct { .. } | TypeInfo::Reference { .. });
        }

        // Reference compatibility: &T is compatible with &T
        // Mutable reference is compatible with immutable reference target
        match (self, target) {
            (
                TypeInfo::Reference {
                    inner: self_inner,
                    is_mut: self_mut,
                },
                TypeInfo::Reference {
                    inner: target_inner,
                    is_mut: target_mut,
                },
            ) => {
                // Can't assign immutable ref to mutable ref target
                if *target_mut && !*self_mut {
                    return false;
                }
                self_inner.is_assignable_to(target_inner)
            }

            // Generic type compatibility (e.g., ID<Signal>)
            (
                TypeInfo::Generic {
                    name: self_name,
                    args: self_args,
                },
                TypeInfo::Generic {
                    name: target_name,
                    args: target_args,
                },
            ) => {
                if self_name != target_name || self_args.len() != target_args.len() {
                    return false;
                }
                self_args
                    .iter()
                    .zip(target_args.iter())
                    .all(|(s, t)| s.is_assignable_to(t))
            }

            // Array compatibility
            (
                TypeInfo::Array {
                    element: self_elem,
                    size: self_size,
                },
                TypeInfo::Array {
                    element: target_elem,
                    size: target_size,
                },
            ) => self_size == target_size && self_elem.is_assignable_to(target_elem),

            _ => false,
        }
    }

    /// Get the type name as it would appear in source code
    pub fn type_name(&self) -> String {
        match self {
            TypeInfo::Bool => "bool".to_string(),
            TypeInfo::I64 => "i64".to_string(),
            TypeInfo::F64 => "f64".to_string(),
            TypeInfo::String => "String".to_string(),
            TypeInfo::Struct { name, .. } => name.clone(),
            TypeInfo::Enum { name } => name.clone(),
            TypeInfo::Reference { inner, is_mut } => {
                let mut_str = if *is_mut { "mut " } else { "" };
                format!("&{}{}", mut_str, inner.type_name())
            }
            TypeInfo::Pointer { inner, is_mut } => {
                let mut_str = if *is_mut { "mut " } else { "" };
                format!("*{}{}", mut_str, inner.type_name())
            }
            TypeInfo::Generic { name, args } => {
                if args.is_empty() {
                    name.clone()
                } else {
                    let args_str: Vec<_> = args.iter().map(|a| a.type_name()).collect();
                    format!("{}<{}>", name, args_str.join(", "))
                }
            }
            TypeInfo::Array { element, size } => {
                format!("[{}; {}]", element.type_name(), size)
            }
            TypeInfo::Void => "void".to_string(),
            TypeInfo::Unknown => "?".to_string(),
            TypeInfo::Never => "!".to_string(),
            TypeInfo::SelfType => "Self".to_string(),
        }
    }
}

impl fmt::Display for TypeInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.type_name())
    }
}

/// Parse a type string from source code into a TypeInfo.
/// This handles the common type syntax patterns in NimbyScript.
pub fn parse_type_string(s: &str) -> TypeInfo {
    let s = s.trim();

    // Handle reference types: &T, &mut T
    if let Some(inner) = s.strip_prefix('&') {
        let inner = inner.trim();
        if let Some(inner) = inner.strip_prefix("mut ") {
            return TypeInfo::reference(parse_type_string(inner.trim()), true);
        }
        return TypeInfo::reference(parse_type_string(inner), false);
    }

    // Handle pointer types: *T, *mut T
    if let Some(inner) = s.strip_prefix('*') {
        let inner = inner.trim();
        if let Some(inner) = inner.strip_prefix("mut ") {
            return TypeInfo::pointer(parse_type_string(inner.trim()), true);
        }
        return TypeInfo::pointer(parse_type_string(inner), false);
    }

    // Handle array types: [T; N]
    if s.starts_with('[') && s.ends_with(']') {
        let inner = &s[1..s.len() - 1];
        if let Some(semi_pos) = inner.rfind(';') {
            let elem_str = inner[..semi_pos].trim();
            let size_str = inner[semi_pos + 1..].trim();
            if let Ok(size) = size_str.parse::<usize>() {
                return TypeInfo::array(parse_type_string(elem_str), size);
            }
        }
    }

    // Handle generic types: Name<T, U, ...>
    if let Some(angle_pos) = s.find('<') {
        if s.ends_with('>') {
            let name = s[..angle_pos].trim();
            let args_str = &s[angle_pos + 1..s.len() - 1];
            let args = parse_generic_args(args_str);
            return TypeInfo::generic(name, args);
        }
    }

    // Primitive types
    match s {
        "bool" => TypeInfo::Bool,
        "i64" | "i32" | "i16" | "i8" | "u64" | "u32" | "u16" | "u8" => TypeInfo::I64, // Simplify to i64
        "f64" | "f32" => TypeInfo::F64,
        "String" | "string" => TypeInfo::String,
        "void" | "()" => TypeInfo::Void,
        "Self" => TypeInfo::SelfType,
        "" => TypeInfo::Unknown,
        _ => {
            // Assume it's a struct or enum type
            // The actual distinction will be made during semantic analysis
            TypeInfo::struct_type(s)
        }
    }
}

/// Parse comma-separated generic arguments, handling nested generics
fn parse_generic_args(s: &str) -> Vec<TypeInfo> {
    let mut args = Vec::new();
    let mut current = String::new();
    let mut depth = 0;

    for c in s.chars() {
        match c {
            '<' => {
                depth += 1;
                current.push(c);
            }
            '>' => {
                depth -= 1;
                current.push(c);
            }
            ',' if depth == 0 => {
                let trimmed = current.trim();
                if !trimmed.is_empty() {
                    args.push(parse_type_string(trimmed));
                }
                current.clear();
            }
            _ => current.push(c),
        }
    }

    let trimmed = current.trim();
    if !trimmed.is_empty() {
        args.push(parse_type_string(trimmed));
    }

    args
}

#[cfg(test)]
mod tests {
    use super::*;

    // Type creation tests

    #[test]
    fn test_primitive_types() {
        assert!(TypeInfo::Bool.is_bool());
        assert!(TypeInfo::I64.is_integer());
        assert!(TypeInfo::F64.is_float());
        assert!(TypeInfo::I64.is_numeric());
        assert!(TypeInfo::F64.is_numeric());
        assert!(!TypeInfo::Bool.is_numeric());
    }

    #[test]
    fn test_struct_type() {
        let s = TypeInfo::struct_type("MyStruct");
        assert_eq!(
            s,
            TypeInfo::Struct {
                name: "MyStruct".to_string(),
                extends: None
            }
        );
    }

    #[test]
    fn test_struct_extending() {
        let s = TypeInfo::struct_extending("MyHandler", "Signal");
        assert_eq!(
            s,
            TypeInfo::Struct {
                name: "MyHandler".to_string(),
                extends: Some("Signal".to_string())
            }
        );
    }

    #[test]
    fn test_reference_type() {
        let r = TypeInfo::reference(TypeInfo::I64, false);
        assert!(r.is_reference());
        assert_eq!(r.inner_type(), Some(&TypeInfo::I64));

        let r_mut = TypeInfo::reference(TypeInfo::I64, true);
        assert!(r_mut.is_reference());
    }

    #[test]
    fn test_pointer_type() {
        let p = TypeInfo::pointer(TypeInfo::I64, false);
        assert!(p.is_pointer());
        assert_eq!(p.inner_type(), Some(&TypeInfo::I64));
    }

    #[test]
    fn test_generic_type() {
        let g = TypeInfo::generic("ID", vec![TypeInfo::struct_type("Signal")]);
        assert_eq!(g.type_name(), "ID<Signal>");
    }

    #[test]
    fn test_optional_type() {
        let o = TypeInfo::optional(TypeInfo::I64);
        assert_eq!(o.type_name(), "std::optional<i64>");
    }

    #[test]
    fn test_array_type() {
        let a = TypeInfo::array(TypeInfo::F64, 3);
        assert_eq!(a.type_name(), "[f64; 3]");
    }

    // Type comparison tests

    #[test]
    fn test_can_compare_same_types() {
        assert!(TypeInfo::I64.can_compare_with(&TypeInfo::I64));
        assert!(TypeInfo::F64.can_compare_with(&TypeInfo::F64));
        assert!(TypeInfo::Bool.can_compare_with(&TypeInfo::Bool));
    }

    #[test]
    fn test_can_compare_numerics() {
        // i64 and f64 CAN be compared in NimbyScript
        assert!(TypeInfo::I64.can_compare_with(&TypeInfo::F64));
        assert!(TypeInfo::F64.can_compare_with(&TypeInfo::I64));
    }

    #[test]
    fn test_cannot_compare_different_types() {
        assert!(!TypeInfo::Bool.can_compare_with(&TypeInfo::I64));
        assert!(!TypeInfo::String.can_compare_with(&TypeInfo::I64));
    }

    // Arithmetic tests

    #[test]
    fn test_can_arithmetic_same_numeric() {
        assert!(TypeInfo::I64.can_arithmetic_with(&TypeInfo::I64));
        assert!(TypeInfo::F64.can_arithmetic_with(&TypeInfo::F64));
    }

    #[test]
    fn test_cannot_arithmetic_mixed_numeric() {
        // NimbyScript is strict: i64 + f64 is NOT allowed
        assert!(!TypeInfo::I64.can_arithmetic_with(&TypeInfo::F64));
        assert!(!TypeInfo::F64.can_arithmetic_with(&TypeInfo::I64));
    }

    #[test]
    fn test_cannot_arithmetic_non_numeric() {
        assert!(!TypeInfo::Bool.can_arithmetic_with(&TypeInfo::Bool));
        assert!(!TypeInfo::String.can_arithmetic_with(&TypeInfo::String));
    }

    // Assignment tests

    #[test]
    fn test_assignable_same_type() {
        assert!(TypeInfo::I64.is_assignable_to(&TypeInfo::I64));
        assert!(TypeInfo::Bool.is_assignable_to(&TypeInfo::Bool));
    }

    #[test]
    fn test_not_assignable_different_type() {
        assert!(!TypeInfo::I64.is_assignable_to(&TypeInfo::F64));
        assert!(!TypeInfo::Bool.is_assignable_to(&TypeInfo::I64));
    }

    #[test]
    fn test_never_assignable_to_anything() {
        assert!(TypeInfo::Never.is_assignable_to(&TypeInfo::I64));
        assert!(TypeInfo::Never.is_assignable_to(&TypeInfo::Bool));
    }

    #[test]
    fn test_unknown_permissive() {
        assert!(TypeInfo::Unknown.is_assignable_to(&TypeInfo::I64));
        assert!(TypeInfo::I64.is_assignable_to(&TypeInfo::Unknown));
        assert!(TypeInfo::Unknown.can_compare_with(&TypeInfo::Bool));
        assert!(TypeInfo::Unknown.can_arithmetic_with(&TypeInfo::I64));
    }

    #[test]
    fn test_reference_assignability() {
        let ref_i64 = TypeInfo::reference(TypeInfo::I64, false);
        let ref_i64_mut = TypeInfo::reference(TypeInfo::I64, true);

        // Exact match
        assert!(ref_i64.is_assignable_to(&ref_i64));

        // Mutable ref can be assigned to immutable ref
        assert!(ref_i64_mut.is_assignable_to(&ref_i64));

        // Immutable ref CANNOT be assigned to mutable ref
        assert!(!ref_i64.is_assignable_to(&ref_i64_mut));
    }

    #[test]
    fn test_generic_assignability() {
        let id_signal = TypeInfo::generic("ID", vec![TypeInfo::struct_type("Signal")]);
        let id_signal2 = TypeInfo::generic("ID", vec![TypeInfo::struct_type("Signal")]);
        let id_train = TypeInfo::generic("ID", vec![TypeInfo::struct_type("Train")]);

        assert!(id_signal.is_assignable_to(&id_signal2));
        assert!(!id_signal.is_assignable_to(&id_train));
    }

    // Parse type string tests

    #[test]
    fn test_parse_primitives() {
        assert_eq!(parse_type_string("bool"), TypeInfo::Bool);
        assert_eq!(parse_type_string("i64"), TypeInfo::I64);
        assert_eq!(parse_type_string("f64"), TypeInfo::F64);
        assert_eq!(parse_type_string("String"), TypeInfo::String);
    }

    #[test]
    fn test_parse_reference() {
        assert_eq!(
            parse_type_string("&i64"),
            TypeInfo::reference(TypeInfo::I64, false)
        );
        assert_eq!(
            parse_type_string("&mut i64"),
            TypeInfo::reference(TypeInfo::I64, true)
        );
    }

    #[test]
    fn test_parse_pointer() {
        assert_eq!(
            parse_type_string("*i64"),
            TypeInfo::pointer(TypeInfo::I64, false)
        );
        assert_eq!(
            parse_type_string("*mut i64"),
            TypeInfo::pointer(TypeInfo::I64, true)
        );
    }

    #[test]
    fn test_parse_array() {
        assert_eq!(
            parse_type_string("[f64; 3]"),
            TypeInfo::array(TypeInfo::F64, 3)
        );
    }

    #[test]
    fn test_parse_generic() {
        let expected = TypeInfo::generic("ID", vec![TypeInfo::struct_type("Signal")]);
        assert_eq!(parse_type_string("ID<Signal>"), expected);
    }

    #[test]
    fn test_parse_nested_generic() {
        let expected = TypeInfo::generic("std::optional", vec![TypeInfo::I64]);
        assert_eq!(parse_type_string("std::optional<i64>"), expected);
    }

    #[test]
    fn test_parse_struct_type() {
        let expected = TypeInfo::struct_type("MyStruct");
        assert_eq!(parse_type_string("MyStruct"), expected);
    }

    #[test]
    fn test_parse_complex_reference() {
        // &Signal
        let expected = TypeInfo::reference(TypeInfo::struct_type("Signal"), false);
        assert_eq!(parse_type_string("&Signal"), expected);
    }

    // Display tests

    #[test]
    fn test_type_name_primitives() {
        assert_eq!(TypeInfo::Bool.type_name(), "bool");
        assert_eq!(TypeInfo::I64.type_name(), "i64");
        assert_eq!(TypeInfo::F64.type_name(), "f64");
        assert_eq!(TypeInfo::String.type_name(), "String");
    }

    #[test]
    fn test_type_name_reference() {
        let r = TypeInfo::reference(TypeInfo::I64, false);
        assert_eq!(r.type_name(), "&i64");

        let r_mut = TypeInfo::reference(TypeInfo::I64, true);
        assert_eq!(r_mut.type_name(), "&mut i64");
    }

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", TypeInfo::I64), "i64");
        assert_eq!(
            format!("{}", TypeInfo::reference(TypeInfo::Bool, false)),
            "&bool"
        );
    }
}
