(([
   (comment)
   (export_statement)
   (lexical_declaration)
   (if_statement)
   (else_clause)
   (switch_statement)
   (switch_case)
   (switch_default)
   (array)
   (for_statement)
   (function_declaration)
   (class_declaration)
   (method_definition)
   (type_alias_declaration)
   (interface_declaration)
   (property_signature)
   (public_field_definition)
   (pair)
   (object)
   (template_string)
   (string)
   (string_fragment)
   (call_expression)

   (array)
   (object)
  
   ;; change padding capture for theses:
   (array_pattern)
   (generic_type)
   (union_type)
   (object_type)
   (identifier)
   (array_type)
   (type_identifier)
   ] @parent))

(else_clause) @jump  
(export_statement) @jump 
; (identifier) @list_arg 
; (required_parameter) @list_arg 
; (optional_parameter) @list_arg 

(for_statement) @list 
(type_arguments) @list 
(array) @list 
(formal_parameters) @list 

