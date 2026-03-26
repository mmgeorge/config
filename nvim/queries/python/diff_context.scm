(function_definition
  name: (identifier) @scope.name) @scope

(class_definition
  name: (identifier) @scope.name) @scope

(decorated_definition
  definition: (function_definition
    name: (identifier) @scope.name)) @scope

(decorated_definition
  definition: (class_definition
    name: (identifier) @scope.name)) @scope
