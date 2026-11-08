# Module organization

Read before splitting, moving, or promoting modules, placing test helpers or `#[path]` declarations, ordering trait implementations, or changing re-exports.

## Promote a file to a directory

Promote a file to a directory when independently understandable child responsibilities emerge. Name the directory after the feature area its children share, never after one child. Keep the parent implementation beside its children:

```text
inspection/
  mod.rs
  inspector.rs
  panel.rs
  property_row.rs
```

`mod.rs` declares the children and re-exports the public owner:

```rust
mod inspector;
mod panel;
mod property_row;

pub use inspector::Inspector;
```

Do not name a child after its directory. `inspector/inspector.rs` triggers `clippy::module_inception`.

Do not create a directory that only wraps one implementation file.

## Let the file layout resolve modules

Place each module's file where `mod name;` finds it by default: `name.rs` beside the declaring file, or `name/mod.rs`, following the crate's existing convention. Readers navigate by that layout, so never use `#[path]` to rename or relocate a module. A `#[path]` attribute also makes the loaded file resolve its children like a `mod.rs` file, which forces every child declaration to repeat a path of its own.

Put code where its consumers can reach it without `#[path]`:

- **Unit tests:** `#[cfg(test)] mod tests;` in `foo.rs` loads `foo/tests.rs`.
- **Integration test helpers:** `tests/support/mod.rs`, declared as `mod support;` in each integration test. The `mod.rs` form keeps Cargo from building the helper as its own test target.
- **Fixtures shared beyond one crate's integration tests:** a library module gated by `#[cfg(any(test, feature = "test-fixtures"))]`. Dependent crates enable the feature in `[dev-dependencies]`. The crate's own integration tests enable it through a self dev-dependency, `name = { path = ".", features = ["test-fixtures"] }`.
- **Code shared with `build.rs`:** a small crate listed under both `[dependencies]` and `[build-dependencies]`, which the main crate may re-export.

Use `#[path]` only to select one of several platform implementations of the same logical module, paired with `#[cfg]` or `#[cfg_attr(..., path = "...")]` conditions that cover every supported target exactly once.

## Split at ownership boundaries

Split when public owners govern different lifecycles or serve independent consumers. Keep caller-facing values and supporting contracts with their owner. Keep storage, allocation, and orchestration details private to the owner that enforces them.

An asset handle can own its status, revision, and shared result while a store owns cache keys and loader selection. Separate those owners without scattering each supporting type into its own file.

## Order within a cluster

Exported means visible outside the defining module, including `pub(crate)` and `pub(super)`.

Within each inherent method visibility group, place callers before their helpers when dependencies permit.

Order trait implementations from the primary architectural contract to adapters, conversions, and formatting. Place supporting private types after the complete owner cluster, never between a public struct and its implementation. Keep an enum's methods with its declaration.

## Keep behavior and visibility with the owner

When an operation belongs to another owner, move it to that owner. Introduce a narrow integration trait only when the boundary needs one. Keep each trait implementation beside the type or integration it describes.

Re-export contracts that a real consumer needs, never storage details.
