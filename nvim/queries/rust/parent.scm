(([
   (use_declaration)
   (self_parameter)
   (parameter)
   (dynamic_type)
   (line_comment)
   (attribute_item)
   (field_declaration)
   (enum_variant)
   (expression_statement)
   (match_arm)
   (match_pattern)
   (generic_function)
   (macro_definition)
   (function_item)
   (function_signature_item)
   (struct_item)
   (enum_item)
   (impl_item)

   (let_declaration)
   (call_expression)
   (macro_invocation)

   (scoped_identifier)
   (reference_type)
   (reference_expression)
   (lifetime)
   (trait_bounds)
   (abstract_type) ; impl Iterator<..>
   ; (generic_type)
   ; (type_identifier)
   (string_literal)
   (string_content)

   (field_initializer_list)   

   ; Select inner -- all list types?
   (array_expression)

   ] @parent))

(field_initializer_list) @list
(token_tree) @list
(type_arguments) @list
(type_parameters) @list
(arguments) @list
(parameters) @list
(array_expression) @list
