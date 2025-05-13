; extends

(variable_declarator
  name: (identifier) @declaration)  

(function_declaration
  name: (identifier) @declaration) 

(generator_function_declaration
  name: (identifier) @declaration) 

(method_definition
  (override_modifier) @builtin
  name: (property_identifier) @declaration) 
 
(method_definition
  name: (property_identifier) @declaration) 
(method_signature
  name: (property_identifier) @declaration)

(decorator
  (call_expression 
    function: (identifier) @decorator)) 

(accessibility_modifier) @builtin 

(implements_clause) @builtin

(type_identifier) @type 

(this_type) @primitive
(predefined_type) @primitive 

(public_field_definition) @builtin 
(import_statement) @builtin
