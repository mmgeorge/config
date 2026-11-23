# Errors

Match the library's existing error stack (`thiserror` if any existing error uses it, otherwise `Facet`). Facet serialization does not override existing `thiserror` errors.

## Stack shapes

In `thiserror`, messages live in `#[error(...)]` and `#[source]` marks the cause:

```rust
#[derive(Debug, thiserror::Error)]
pub enum ManifestError {
  #[error("failed to read manifest {path}")]
  Read {
    path: String,
    #[source]
    source: std::io::Error,
  },
  #[error("manifest {path} is missing field {field}")]
  MissingField { path: String, field: String },
}
```

In `Facet`, variant doc comments are the runtime `Display` messages, non-`Facet` causes take `#[facet(opaque)]`, and `facet-error` requires `#[repr(u8)]`:

```rust
#[derive(Debug, Facet)]
#[facet(derive(Error))]
#[repr(u8)]
pub enum ManifestError {
  /// failed to read manifest {path}
  Read {
    path: String,
    #[facet(opaque)]
    source: std::io::Error,
  },
  /// manifest {path} is missing field {field}
  MissingField { path: String, field: String },
}
```

## Sources and boundaries

- Define errors at the boundary that owns recovery context (paths, IDs, operation names) and avoid wrapper variants that merely rename another error. Convert once at domain boundaries.
- In `Facet`, `#[facet(opaque)]` only controls reflection, not `Error::source()`. Check the installed crate's docs via `docs-mcp`, write `From` impls manually where needed, and test that callers observe preserved causes.
