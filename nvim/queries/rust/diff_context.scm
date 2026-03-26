(function_item
  name: (identifier) @scope.name) @scope

(struct_item
  name: (type_identifier) @scope.name) @scope

(enum_item
  name: (type_identifier) @scope.name) @scope

(trait_item
  name: (type_identifier) @scope.name) @scope

(impl_item
  type: (type_identifier) @scope.name) @scope

(impl_item
  type: (generic_type
    type: (type_identifier) @scope.name)) @scope

(mod_item
  name: (identifier) @scope.name) @scope
