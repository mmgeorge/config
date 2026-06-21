(impl_item
  type: (type_identifier) @inventory.container.name) @inventory.container

(impl_item
  type: (generic_type
    type: (type_identifier) @inventory.container.name)) @inventory.container

(function_item
  name: (identifier) @inventory.function.name) @inventory.function

(struct_item
  name: (type_identifier) @inventory.struct.name) @inventory.struct

(enum_item
  name: (type_identifier) @inventory.enum.name) @inventory.enum

(trait_item
  name: (type_identifier) @inventory.trait.name) @inventory.trait

(type_item
  name: (type_identifier) @inventory.type.name) @inventory.type

(mod_item
  name: (identifier) @inventory.module.name) @inventory.module
