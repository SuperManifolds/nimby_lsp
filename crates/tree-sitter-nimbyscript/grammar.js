/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

// Operator precedence levels (higher = binds tighter)
const PREC = {
  OR: 1,
  AND: 2,
  EQUALITY: 3,
  COMPARISON: 4,
  ADDITIVE: 5,
  MULTIPLICATIVE: 6,
  UNARY: 7,
  POSTFIX: 8,
};

module.exports = grammar({
  name: 'nimbyscript',

  extras: $ => [
    /\s/,
    $.comment,
  ],

  word: $ => $.identifier,

  conflicts: $ => [
    [$._assignable, $.path_segment],
    [$.path_segment, $._generic_path_segment],
    // GLR: try both interpretations when we see obj.method< (could be generic args or comparison)
    [$.field_access],
  ],

  rules: {
    // =========================================================================
    // Top-level structure
    // =========================================================================
    source_file: $ => repeat($._item),

    _item: $ => choice(
      $.script_meta,
      $.const_declaration,
      $.struct_definition,
      $.enum_definition,
      $.function_definition,
    ),

    // =========================================================================
    // Comments
    // =========================================================================
    comment: $ => token(seq('//', /.*/)),

    // =========================================================================
    // Identifiers and Keywords
    // =========================================================================
    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,

    // =========================================================================
    // Script metadata: script meta { ... }
    // =========================================================================
    script_meta: $ => seq('script', $.meta_block),

    meta_block: $ => seq('meta', $.meta_map),

    meta_map: $ => seq(
      '{',
      optional(seq(
        $.meta_entry,
        repeat(seq(',', $.meta_entry)),
        optional(','),
      )),
      '}',
    ),

    meta_entry: $ => seq(
      field('key', $.meta_name),
      ':',
      field('value', $._meta_value),
    ),

    _meta_value: $ => choice(
      $.meta_map,
      $.meta_array,
      $.string_literal,
      $.meta_name,
      $.number,
    ),

    meta_array: $ => seq(
      '[',
      optional(seq(
        $._meta_value,
        repeat(seq(',', $._meta_value)),
        optional(','),
      )),
      ']',
    ),

    meta_name: $ => /[a-zA-Z_][a-zA-Z0-9_.]*/,

    // =========================================================================
    // Constant declaration: const NAME: Type = value;
    // =========================================================================
    const_declaration: $ => seq(
      'const',
      field('name', $.identifier),
      ':',
      field('type', $.type),
      '=',
      field('value', $._constant),
      ';',
    ),

    // =========================================================================
    // Struct definition
    // =========================================================================
    struct_definition: $ => seq(
      optional($.visibility_modifier),
      'struct',
      field('name', $.identifier),
      optional($.extends_clause),
      '{',
      repeat($._struct_item),
      '}',
    ),

    extends_clause: $ => seq('extend', field('type', $.identifier)),

    _struct_item: $ => choice(
      $.struct_field,
      $.meta_field,
    ),

    struct_field: $ => seq(
      field('name', $.identifier),
      ':',
      field('type', $.type),
      optional($.meta_block),
      ',',
    ),

    meta_field: $ => seq($.meta_block, ','),

    // =========================================================================
    // Enum definition
    // =========================================================================
    enum_definition: $ => seq(
      optional($.visibility_modifier),
      'enum',
      field('name', $.identifier),
      '{',
      repeat($.enum_variant),
      '}',
    ),

    enum_variant: $ => seq(
      field('name', $.identifier),
      optional($.meta_block),
      ',',
    ),

    // =========================================================================
    // Function definition
    // =========================================================================
    function_definition: $ => seq(
      optional($.visibility_modifier),
      'fn',
      field('name', $.function_name),
      '(',
      optional($.parameters),
      ')',
      optional(seq(':', field('return_type', $.type))),
      field('body', $.block),
    ),

    function_name: $ => seq(
      $.identifier,
      optional(seq('::', $.identifier)),
    ),

    parameters: $ => seq(
      $.parameter,
      repeat(seq(',', $.parameter)),
    ),

    parameter: $ => seq(
      field('name', $.identifier),
      ':',
      field('type', $.type),
    ),

    visibility_modifier: $ => 'pub',

    // =========================================================================
    // Types
    // =========================================================================
    type: $ => seq(
      optional($.storage_modifier),
      optional($.mutability_modifier),
      $.type_identifier,
    ),

    type_identifier: $ => seq(
      $.identifier,
      optional($.generic_arguments),
      optional(seq('::', $.type_identifier)),
    ),

    generic_arguments: $ => prec.dynamic(10, seq(
      '<',
      $.type_identifier,
      repeat(seq(',', $.type_identifier)),
      '>',
    )),

    storage_modifier: $ => choice('&', '*'),
    mutability_modifier: $ => 'mut',

    // =========================================================================
    // Statements
    // =========================================================================
    block: $ => seq('{', repeat($._statement), '}'),

    _statement: $ => choice(
      $.let_statement,
      $.let_else_statement,
      $.assignment_statement,
      $.if_statement,
      $.if_let_statement,
      $.for_statement,
      $.return_statement,
      $.break_statement,
      $.continue_statement,
      $.log_statement,
      $.expression_statement,
    ),

    let_statement: $ => seq(
      'let',
      $.binding,
      ';',
    ),

    let_else_statement: $ => seq(
      'let',
      $.binding,
      'else',
      $.block,
    ),

    binding: $ => seq(
      field('name', $.identifier),
      optional(seq(':', optional($.type_pattern))),
      field('operator', $.binding_operator),
      field('value', $._expression),
    ),

    // Binding operators need to be single tokens
    binding_operator: $ => choice(
      token('&mut='),
      token('mut='),
      token('&='),
      '=',
    ),

    type_pattern: $ => choice(
      seq($.storage_modifier, optional($.mutability_modifier), optional($.type_identifier)),
      seq($.mutability_modifier, optional($.type_identifier)),
      $.type_identifier,
    ),

    assignment_statement: $ => prec.right(seq(
      field('left', $._assignable),
      '=',
      field('right', $._expression),
      ';',
    )),

    // Assignable expressions: simple identifier or field access chain
    _assignable: $ => choice(
      $.identifier,
      alias($.assignment_field_access, $.field_access),
    ),

    // Field access specifically for assignment LHS (to avoid conflicts)
    assignment_field_access: $ => prec.left(PREC.POSTFIX + 1, seq(
      field('object', $._assignable),
      '.',
      field('field', $.identifier),
    )),

    if_statement: $ => seq(
      'if',
      field('condition', $._expression),
      field('consequence', $.block),
      optional($.else_clause),
    ),

    if_let_statement: $ => seq(
      'if',
      'let',
      $.binding,
      field('consequence', $.block),
      optional($.else_clause),
    ),

    else_clause: $ => seq(
      'else',
      choice(
        $.if_statement,
        $.if_let_statement,
        $.block,
      ),
    ),

    for_statement: $ => seq(
      'for',
      field('variable', $.identifier),
      'in',
      field('iterator', $._expression),
      field('body', $.block),
    ),

    return_statement: $ => seq('return', optional($._expression), ';'),
    break_statement: $ => seq('break', ';'),
    continue_statement: $ => seq('continue', ';'),

    log_statement: $ => seq(
      'log',
      '(',
      $.string_literal,
      optional(seq(',', $.arguments)),
      ')',
      ';',
    ),

    expression_statement: $ => seq($._expression, ';'),

    // =========================================================================
    // Expressions
    // =========================================================================
    _expression: $ => choice(
      $.binary_expression,
      $.unary_expression,
      $._postfix_expression,
    ),

    binary_expression: $ => choice(
      prec.left(PREC.OR, seq($._expression, '||', $._expression)),
      prec.left(PREC.AND, seq($._expression, '&&', $._expression)),
      prec.left(PREC.EQUALITY, seq($._expression, choice('==', '!='), $._expression)),
      // Use explicit tokens for comparison to avoid conflict with generic arguments
      prec.left(PREC.COMPARISON, seq($._expression, '<=', $._expression)),
      prec.left(PREC.COMPARISON, seq($._expression, '>=', $._expression)),
      prec.left(PREC.COMPARISON, seq($._expression, $._less_than, $._expression)),
      prec.left(PREC.COMPARISON, seq($._expression, $._greater_than, $._expression)),
      prec.left(PREC.ADDITIVE, seq($._expression, choice('+', '-'), $._expression)),
      prec.left(PREC.MULTIPLICATIVE, seq($._expression, choice('*', '/', '%'), $._expression)),
    ),

    // These are used to disambiguate < and > from generic arguments
    // The external scanner will handle the context
    _less_than: $ => '<',
    _greater_than: $ => '>',

    unary_expression: $ => prec(PREC.UNARY, seq(
      choice('!', '-'),
      $._expression,
    )),

    // Postfix expressions: atom followed by method calls or field access
    _postfix_expression: $ => choice(
      $.call_expression,
      $.field_access,
      $._atom_expression,
    ),

    call_expression: $ => prec(PREC.POSTFIX, seq(
      field('function', $._postfix_expression),
      '(',
      optional($.arguments),
      ')',
    )),

    field_access: $ => prec(PREC.POSTFIX, seq(
      field('object', $._postfix_expression),
      '.',
      field('field', $.identifier),
      optional(field('type_arguments', $.generic_arguments)),
    )),

    _atom_expression: $ => choice(
      $.parenthesized_expression,
      $.path_expression,
      $._constant,
    ),

    path_expression: $ => prec.left(seq(
      $.path_segment,
      repeat(seq('::', $.path_segment)),
    )),

    path_segment: $ => choice(
      alias($._generic_path_segment, $.path_segment),
      $.identifier,
    ),

    _generic_path_segment: $ => seq(
      $.identifier,
      $.generic_arguments,
    ),

    parenthesized_expression: $ => seq('(', $._expression, ')'),

    arguments: $ => seq(
      $._expression,
      repeat(seq(',', $._expression)),
    ),

    // =========================================================================
    // Literals / Constants
    // =========================================================================
    _constant: $ => choice(
      $.time_literal,
      $.number,
      $.boolean,
      $.string_literal,
    ),

    number: $ => token(seq(
      optional('-'),
      /[0-9]+/,
      optional(seq('.', /[0-9]+/)),
      optional(choice('i64', 'u64', 'i32', 'u32', 'i16', 'u16', 'i8', 'u8', 'f64', 'f32')),
    )),

    time_literal: $ => token(seq(
      /[0-9]+/,
      ':',
      /[0-9]+/,
      optional(seq(':', /[0-9]+/)),
    )),

    boolean: $ => choice('true', 'false'),

    string_literal: $ => seq(
      '"',
      repeat(choice(
        /[^"\\]/,
        $.escape_sequence,
      )),
      '"',
    ),

    escape_sequence: $ => token.immediate(seq('\\', /./)),
  },
});
