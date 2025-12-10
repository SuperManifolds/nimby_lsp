; NimbyScript syntax highlighting queries

; Keywords
[
  "script"
  "meta"
  "struct"
  "enum"
  "fn"
  "pub"
  "extend"
  "const"
  "let"
  "mut"
  "if"
  "else"
  "for"
  "in"
  "return"
  "break"
  "continue"
  "log"
] @keyword

; Operators
[
  "="
  "=="
  "!="
  "<"
  ">"
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

; Punctuation
["(" ")" "{" "}" "[" "]" "<" ">"] @punctuation.bracket
["," ":" "::" ";" "."] @punctuation.delimiter

; Literals
(number) @number
(boolean) @boolean
(string_literal) @string
(time_literal) @number

; Comments
(comment) @comment

; Types
(type_identifier) @type
(storage_modifier) @keyword.modifier
(mutability_modifier) @keyword.modifier

; Functions
(function_definition
  name: (function_name) @function)

(call_expression
  function: (path_expression) @function.call)

; Struct/Enum definitions
(struct_definition
  name: (identifier) @type.definition)

(enum_definition
  name: (identifier) @type.definition)

(enum_variant
  name: (identifier) @constant)

; Extends clause
(extends_clause
  type: (identifier) @type)

; Fields
(struct_field
  name: (identifier) @property)

; Parameters
(parameter
  name: (identifier) @variable.parameter)

; Constants
(const_declaration
  name: (identifier) @constant)

; Meta
(meta_entry
  key: (meta_name) @property)

; Identifiers (fallback)
(identifier) @variable
