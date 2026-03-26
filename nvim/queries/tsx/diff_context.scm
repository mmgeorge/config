(function_declaration
  name: (identifier) @scope.name) @scope

(generator_function_declaration
  name: (identifier) @scope.name) @scope

(class_declaration
  name: (type_identifier) @scope.name) @scope

(abstract_class_declaration
  name: (type_identifier) @scope.name) @scope

(method_definition
  name: (property_identifier) @scope.name) @scope

(interface_declaration
  name: (type_identifier) @scope.name) @scope

(enum_declaration
  name: (identifier) @scope.name) @scope

; Arrow function assigned to variable: const foo = () => {}
(lexical_declaration
  (variable_declarator
    name: (identifier) @scope.name
    value: (arrow_function))) @scope

; Module/namespace
(internal_module
  name: (_) @scope.name) @scope
