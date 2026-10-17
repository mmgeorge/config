((function_definition
  name: (identifier) @scope.name) @scope
  (#not-has-parent? @scope decorated_definition))

((class_definition
  name: (identifier) @scope.name) @scope
  (#not-has-parent? @scope decorated_definition))

(decorated_definition
  definition: (function_definition
    name: (identifier) @scope.name)) @scope

(decorated_definition
  definition: (class_definition
    name: (identifier) @scope.name)) @scope
