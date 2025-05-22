(export_statement
  (_) @jump)

(lexical_declaration
  (_) @jump)

(type_annotation) @jump

(type_parameter
  name: (type_identifier) @jump)

(statement_block) @list

(array_pattern) @list

(arguments) @list

(for_statement) @list

(type_arguments) @list

(object_pattern) @list

(object) @list

(array) @list

(interface_body) @list

(formal_parameters) @list

(type_parameters) @list

(statement_block) @list

(call_expression) @outer_only

(call_expression
  function: (identifier) @jump)

(new_expression
  constructor: (identifier) @jump)

(property_identifier) @chain

(member_expression) @chain

(call_expression
  (member_expression)) @jump ; chain selection rules interfere with this

(else_clause) @else

(else_clause
  (if_statement
    consequence: (statement_block) @end)) @elseif

(else_clause
  (if_statement) @jump)

(if_statement
  consequence: (statement_block) @end) @if
