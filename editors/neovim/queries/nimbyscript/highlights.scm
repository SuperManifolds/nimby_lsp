; NimbyScript syntax highlighting queries
; Based on LSP semantic token categories

; =============================================================================
; Keywords
; =============================================================================

[
  "script"
  "meta"
  "struct"
  "enum"
  "fn"
  "extend"
  "const"
  "let"
  "if"
  "else"
  "for"
  "in"
  "return"
  "break"
  "continue"
  "log"
] @keyword

(visibility_modifier) @keyword
(storage_modifier) @keyword
(mutability_modifier) @keyword

; Booleans are keywords in LSP semantic tokens
(boolean) @keyword

; =============================================================================
; Comments
; =============================================================================

(comment) @comment

; =============================================================================
; Literals
; =============================================================================

(number) @number
(time_literal) @number
(string_literal) @string

; =============================================================================
; Type Definitions
; =============================================================================

; Struct definition name
(struct_definition
  name: (identifier) @type.definition)

; Enum definition name
(enum_definition
  name: (identifier) @type.definition)

; Enum variant names
(enum_variant
  name: (identifier) @constant)

; Extends clause type
(extends_clause
  type: (identifier) @type)

; =============================================================================
; Struct Fields
; =============================================================================

; Field definition
(struct_field
  name: (identifier) @property)

; Field access
(field_access
  field: (identifier) @property)

; =============================================================================
; Function Definitions
; =============================================================================

; Simple function name
(function_definition
  name: (function_name
    (identifier) @function
    .))

; Method definition: Type::method - first identifier is type
(function_definition
  name: (function_name
    (identifier) @type
    (identifier) @function))

; =============================================================================
; Type References (defined here but applied after fallback for precedence)
; =============================================================================

; =============================================================================
; Parameters
; =============================================================================

(parameter
  name: (identifier) @variable.parameter)

; =============================================================================
; Variables
; =============================================================================

; Let binding names
(binding
  name: (identifier) @variable)

; For loop variable
(for_statement
  variable: (identifier) @variable)

; Const declaration
(const_declaration
  name: (identifier) @constant)

; =============================================================================
; Function Calls
; =============================================================================

; Simple function call
(call_expression
  function: (path_expression
    (path_segment
      (identifier) @function)))

; Method call: obj.method()
(call_expression
  function: (field_access
    field: (identifier) @function))

; =============================================================================
; Path Expressions (Enum variants, static methods)
; =============================================================================

; Path with :: - first part is type/enum, last part is variant/method
; e.g., SignalCheck::Pass or ID<Train>::new

; First segment in path (before ::) is a type
(path_expression
  (path_segment
    (identifier) @type)
  (path_segment))

; Last segment in path (after ::) is typically an enum member
(path_expression
  (path_segment)
  .
  (path_segment
    (identifier) @constant))

; =============================================================================
; Meta blocks
; =============================================================================

(meta_entry
  key: (meta_name) @property)

; =============================================================================
; Punctuation
; =============================================================================

["(" ")" "{" "}" "[" "]" "<" ">"] @punctuation.bracket
["," ":" "::" ";" "."] @punctuation.delimiter

; =============================================================================
; Operators
; =============================================================================

[
  "="
  "=="
  "!="
  "<="
  ">="
  "+"
  "-"
  "*"
  "/"
  "%"
  "&&"
  "||"
  "!"
  "&"
  "&="
  "&mut="
  "mut="
] @operator

; < and > as comparison operators (in binary expressions)
(binary_expression
  "<" @operator)
(binary_expression
  ">" @operator)

; =============================================================================
; Fallback
; =============================================================================

(identifier) @variable

; =============================================================================
; Type References (MUST be after fallback - later patterns take precedence)
; =============================================================================

; Type identifiers (in type annotations)
; This captures both the main type (e.g., ID) and generic args (e.g., Signal in ID<Signal>)
(type_identifier
  (identifier) @type)

; Ensure nested type identifiers in generics are also captured
(type_identifier
  (generic_arguments
    (type_identifier
      (identifier) @type)))

; Path segments with generics (e.g., ID<Train>::empty())
; The identifier before generic_arguments is a type
(path_segment
  (path_segment
    (identifier) @type
    (generic_arguments)))
