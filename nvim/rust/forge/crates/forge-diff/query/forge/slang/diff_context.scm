; Slang/HLSL function names are in declarator chain:
; function_definition -> declarator (function_declarator) -> declarator (identifier)
(function_definition
  declarator: (function_declarator
    declarator: (identifier) @scope.name)) @scope

(struct_specifier
  name: (type_identifier) @scope.name) @scope

(class_specifier
  name: (type_identifier) @scope.name) @scope

(namespace_definition
  name: (_) @scope.name) @scope

; Slang-specific
(interface_specifier
  name: (_) @scope.name) @scope

(extension_specifier
  name: (_) @scope.name) @scope
