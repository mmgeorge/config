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
   (jsx_element)
   (jsx_self_closing_element)
   (function_declaration)
   (class_declaration)
   (method_definition)
   (type_alias_declaration)
   (interface_declaration)
   (property_signature)
   (public_field_definition)
   (pair)
   (object)
   ] @parent))
   
(else_clause) @jump  
(export_statement) @jump 
; (identifier) @list_arg 
; (required_parameter) @list_arg 
; (optional_parameter) @list_arg 

(type_arguments) @list 
(array) @list 
(formal_parameters) @list 

