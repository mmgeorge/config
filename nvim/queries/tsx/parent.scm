(([
   (comment)
   (export_statement)
   (lexical_declaration)

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


(identifier) @list_arg 
(required_parameter) @list_arg 
(optional_parameter) @list_arg 

(array) @list 
(formal_parameters) @list 

