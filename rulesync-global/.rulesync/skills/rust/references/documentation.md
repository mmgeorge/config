# Rust documentation

Read before writing doc comments, safety comments, or Facet error messages. Apply the technical-writing skill's API Documentation profile first.

## Describe the caller's contract

Use the project's domain vocabulary. State what callers may rely on during creation, use, replacement, and destruction. Mention another type only when callers interact with it as part of that contract. Do not restate the type signature.

A get-or-create operation states what the first call starts and what later calls share:

```rust
/// Load the asset at `path`, or share the load already in progress.
///
/// The first call starts decoding. Later calls for the same path receive the same handle.
/// Removing the asset from the store does not invalidate existing handles.
pub fn load(&self, path: &AssetPath) -> AssetHandle {
```

Documentation that restates the signature or opens with `Returns` tells callers nothing:

```rust
/// Returns an asset handle for the given path.
pub fn load(&self, path: &AssetPath) -> AssetHandle {
```

Do not substitute internal registry or publication mechanics for caller-visible guarantees. For registration, state the installed capability and collision behavior. Describe blocking, failure, panic, cancellation, and thread-safety behavior when those conditions affect callers. Later sentences can describe return variants.

## Place doc comments above attributes

Put every doc comment above all attributes on its item, including `#[derive]`, `#[serde]`, `#[facet]`, `#[cfg]`, lint attributes, and attribute macros such as `#[tokio::test]`. The rule applies equally to structs, enums, functions, fields, and variants:

```rust
/// Streamed provider update normalized for the interaction reducer.
#[derive(Clone, Debug, Serialize)]
pub struct ProviderUpdate {
  /// First adapter receipt time, retained through queueing and forwarding.
  #[serde(default, skip_serializing_if = "Option::is_none")]
  pub received_at_ms: Option<i64>,
}
```

Never place a doc comment between attributes and the item it documents:

```rust
#[derive(Clone, Debug, Serialize)]
/// Streamed provider update normalized for the interaction reducer.
pub struct ProviderUpdate {
  #[serde(default, skip_serializing_if = "Option::is_none")]
  /// First adapter receipt time, retained through queueing and forwarding.
  pub received_at_ms: Option<i64>,
}
```

## Document each definition once

Document the definition itself. Do not copy its prose onto re-exports or onto trait implementations that add no constraints.

## State safety assumptions

In a `// SAFETY:` comment directly above the unsafe operation, name the assumptions that make it valid, including lifetimes, aliasing, initialization, and ABI ownership where relevant:

```rust
// SAFETY: `buffer` holds `len` initialized bytes and outlives this borrow.
let bytes = unsafe { std::slice::from_raw_parts(buffer.as_ptr(), len) };
```

## Preserve executable error text

Facet's `#[facet(derive(Error))]` uses each variant's doc comment as its `Display` format string, interpolating named fields as `{field}` and tuple fields as `{0}`. Keep the concrete failure wording and every placeholder. Treat an edit to that text as a runtime message change, not prose cleanup. Check other derives the same way before editing a doc comment on an error type.
