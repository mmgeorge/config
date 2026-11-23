# Modules

## Directory promotion

Start one owned concept in one file, promoting to a directory only when distinct child responsibilities emerge. Never split a file solely for length or wrap a single file in a directory. Name the directory after the shared feature area (avoid `inspector/inspector.rs`, which triggers `clippy::module_inception`), and keep `mod.rs` to `mod` declarations and public re-exports:

```text
inspection/
  mod.rs
  inspector.rs
  panel.rs
```

## Avoid `#[path]`

Place modules where `mod name;` resolves naturally (`name.rs` or `name/mod.rs`), reserving `#[path]` strictly for mutually exclusive `#[cfg]` platform modules:

- **Unit tests:** Inline `#[cfg(test)] mod tests { ... }`, or `#[cfg(test)] mod tests;` loading `foo/tests.rs` when large.
- **Integration helpers:** `tests/support/mod.rs` (`mod support;` keeps Cargo from building it as a test target).
- **Shared test fixtures:** Library module gated by `#[cfg(any(test, feature = "test-fixtures"))]` enabled via dev-dependencies (and self dev-dependency for own integration tests).
- **Code shared with `build.rs`:** Small crate listed under both `[dependencies]` and `[build-dependencies]`.

## Item ordering and visibility

- Order top-level items after imports: constants, type aliases, enums, struct clusters, free functions, then `#[cfg(test)]`, placing exported items (`pub`, `pub(crate)`, `pub(super)`) before private ones in each group.
- Follow each struct with its inherent `impl` (`pub`, restricted, then private, callers before helpers) and its trait `impl`s (primary architectural contracts, then adapters, conversions, formatting) in its defining module. Put supporting private types after the owner cluster.
- Keep visibility as narrow as real consumers permit, and never widen visibility for tests.
