# Error handling

Read before adding or reshaping error types, error derives, or error conversions.

## Follow the library's error stack

Inspect the library's existing error types, derives, and manifest before adding an error type. Choose the error stack in this order:

1. Use thiserror when any existing error type in the library uses it.
2. Use Facet when no error type uses thiserror and the library already uses Facet for errors or serialization.
3. Use Facet when the library has no error types yet.

Facet serialization does not override existing thiserror errors. A library that serializes with Facet but defines errors with thiserror keeps thiserror. Migrate existing errors only when the requested change includes migration and its compatibility work.

## Keep recoverable failures typed

Define an error at the boundary that can name the failure and its required context. Preserve the underlying cause as a typed source. Carry relevant paths, identifiers, operation names, or retry context on the variant that owns them.

Use a domain enum when callers need to distinguish recovery actions. Reuse an existing error type when it already expresses the failure. Do not add a wrapper error that only renames another error without adding context or a distinct recovery decision.

Convert an error once when it crosses into another domain. Treat a violated private construction invariant as a bug, not an input error. Keep machine-readable failure identity and context when the transport exposes a structured error contract.

## Write the error in the library's stack

The same error takes a different shape in each stack. In thiserror, the message lives in an `#[error(...)]` attribute and `#[source]` marks the cause explicitly:

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

In Facet, the variant doc comment is the message. A cause that does not implement `Facet` needs `#[facet(opaque)]`. The facet-error README declares `#[repr(u8)]` on the enum:

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

## Preserve error sources

In thiserror-based code, preserve the meaning of `#[error(...)]`, source fields, and `#[from]` conversions. Do not remove a source to satisfy a derive.

In Facet-based code, verify that the generated `source()` returns each wrapped cause, such as the `io::Error` in `ManifestError::Read`. `#[facet(opaque)]` controls reflection, not causal chaining. The facet-error 0.46.5 README lists `#[facet(error::source)]` and `#[facet(error::from)]` as future work, and the 0.50.0-rc.7 README does not document how `source()` selects a field. Check the installed version before relying on either. Write `From` conversions by hand when Facet does not generate them.

Test that a library-owned error preserves its cause and context when callers depend on them.
