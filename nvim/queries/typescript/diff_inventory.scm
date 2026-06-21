(function_declaration
  name: (identifier) @inventory.function.name) @inventory.function

(generator_function_declaration
  name: (identifier) @inventory.function.name) @inventory.function

(lexical_declaration
  (variable_declarator
    name: (identifier) @inventory.function.name
    value: (arrow_function))) @inventory.function

(class_declaration
  name: (type_identifier) @inventory.class.name) @inventory.class

(abstract_class_declaration
  name: (type_identifier) @inventory.class.name) @inventory.class

(method_definition
  name: (property_identifier) @inventory.function.name) @inventory.function

(interface_declaration
  name: (type_identifier) @inventory.interface.name) @inventory.interface

(enum_declaration
  name: (identifier) @inventory.enum.name) @inventory.enum

(type_alias_declaration
  name: (type_identifier) @inventory.type.name) @inventory.type

(internal_module
  name: (_) @inventory.module.name) @inventory.module
