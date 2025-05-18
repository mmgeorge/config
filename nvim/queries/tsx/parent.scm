(([
   (comment)
   (export_statement)
   (lexical_declaration)
   (if_statement)
   (else_clause)
   (switch_statement)
   (switch_case)
   (switch_default)

   ; (lexical_declaration
   ;   (variable_declarator
   ;     value: (arrow_function)))
   (jsx_element)
   (jsx_self_closing_element)
   (function_declaration)
   (class_declaration)
   (method_definition)
   ; typescript
   (type_alias_declaration)
   (interface_declaration)
   ] @parent))
   
(else_clause) @jump  
(export_statement) @jump 

(identifier) @list_arg 
(required_parameter) @list_arg 
(optional_parameter) @list_arg 

(array) @list 
(formal_parameters) @list 

