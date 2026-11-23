# Rust documentation

Apply the technical-writing skill's API profile first.

## Caller contract over signature restatement

State what callers can rely on across creation, sharing, failure, and drop without restating the signature. Document each definition once, never copying prose onto re-exports or trivial trait impls.

```rust
// Good: states first-call effect, sharing, and handle lifetime.
/// Load the asset at `path`, or share the load already in progress.
///
/// The first call starts decoding. Later calls for the same path receive the same handle.
/// Removing the asset from the store does not invalidate existing handles.
pub fn load(&self, path: &AssetPath) -> AssetHandle {

// Bad: restates the signature.
/// Returns an asset handle for the given path.
pub fn load(&self, path: &AssetPath) -> AssetHandle {
```

## Safety comments

Place `// SAFETY:` directly above each unsafe operation naming the soundness preconditions (lifetimes, aliasing, initialization, ABI ownership):

```rust
// SAFETY: `buffer` holds `len` initialized bytes and outlives this borrow.
let bytes = unsafe { std::slice::from_raw_parts(buffer.as_ptr(), len) };
```

For `facet-error` variant doc comments (which double as runtime `Display` text), see [errors.md](errors.md).
