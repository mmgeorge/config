(field_identifier) @chain

(field_expression) @jump

(call_expression) @outer_only

(call_expression
  function: (identifier) @jump)

(macro_invocation
  macro: (identifier) @jump)

(parenthesized_expression) @jump

(expression_statement) @list ; functionally we need the skipping logic

(field_initializer_list) @list

(field_declaration_list) @list

(token_tree) @list

(match_block) @list

(block) @list

(tuple_expression) @list

(type_arguments) @list

(type_parameters) @list

(arguments) @list

(parameters) @list

(array_expression) @list

(else_clause) @else

(else_clause
  (if_expression
    consequence: (block) @end)) @elseif

(else_clause
  (if_expression) @jump)

(if_expression
  consequence: (block) @end) @if

(expression_statement
  (if_expression)) @if_container
