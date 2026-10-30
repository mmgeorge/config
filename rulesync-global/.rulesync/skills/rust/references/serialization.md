# Serialization

Read before adding serialization derives or changing persisted or wire representations.

## Follow the library's serialization stack

Inspect the library's existing serialized types, derives, and manifest before adding a serialized type. Choose the serialization stack in this order:

1. Use Serde when any existing serialized type in the library uses it.
2. Use Facet when no serialized type uses Serde and the library already uses Facet for serialization or errors.
3. Use Facet when the library has no serialized types yet.

Facet errors do not override existing Serde serialization. A library that serializes with Serde keeps Serde even when its errors use Facet. Migrate existing types only when the requested change includes migration and its compatibility work.

## Keep serialization contracts explicit

Serialize durable domain facts or explicit transport values. Keep locks, live handles, task state, caches, callbacks, and process-local addresses out of persisted data.

Inspect the existing format, field names, enum tags, defaults, optional fields, and versioning policy before changing a derive or data type. A Rust refactor must not silently alter the wire or storage representation. Use a versioned boundary type when compatibility requires a shape different from the runtime owner.

## Replace values only after validation

A derive must not bypass a validated type's constructor. Deserialize such a type through a raw type and `TryFrom`, as the type-modeling reference shows for `Extent`.

Deserialize and validate a complete replacement before publishing it. Preserve the previous accepted value when decoding or validation fails. Report I/O and decoding failures with the input context that the caller needs to recover.

Test compatibility and recovery behavior owned by the library, such as reading an older saved value, rejecting malformed input, or retaining accepted state after a failed decode.
