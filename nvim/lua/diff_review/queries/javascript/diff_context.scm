(function_declaration
  name: (identifier) @scope.name) @scope

(class_declaration
  name: (identifier) @scope.name) @scope

(method_definition
  name: (property_identifier) @scope.name) @scope

; Arrow function assigned to variable: const foo = () => {}
(lexical_declaration
  (variable_declarator
    name: (identifier) @scope.name
    value: (arrow_function))) @scope
