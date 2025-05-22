(call_expression) @outer_only

(field_identifier) @chain

(field_expression) @jump

(call_expression
  function: (identifier) @jump)

(macro_invocation
  macro: (identifier) @jump)

(field_initializer_list) @list

(field_declaration_list) @list

(token_tree) @list

(tuple_expression) @list

(type_arguments) @list

(type_parameters) @list

(arguments) @list

(parameters) @list

(array_expression) @list
