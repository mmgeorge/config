---
name: rust
description: >-
  Design, implement, refactor, review, and test Rust code. Use for .rs files,
  Cargo.toml, type and invariant design, synchronization, Serde, Facet,
  thiserror, rustdoc, FFI, dynamic libraries, hot reload, and Criterion or
  Samply performance profiling.
---

# Rust

## Required conventions

- **Validate once:** Keep validated fields private and enforce invariants in fallible constructors or `TryFrom` (routing deserialization through `#[serde(try_from = "...")]` or Facet's proxy). Use newtypes or standard types (`Duration`, `PathBuf`, `NonZero*`) for confusable primitives, `Option` instead of sentinels, and `From`/`TryFrom` instead of lossy `as` casts.
- **Enums over strings:** Treat `String` or `&str` for a finite set of cases as a code smell. Use exhaustive enums for closed cases, choices, states, and boundary contracts instead of `kind: String`, `bool` parameters, `serde_json::Value`, or co-dependent `Option`s, boxing large variants when `clippy::large_enum_variant` fires. Parse external protocols inside the receiving adapter, separating provider observations, internal state, and wire notifications, and modeling open external vocabularies with `Missing` and `Unknown(raw)`.
- **Exhaustive matching and conversions:** Match crate-owned enums without `_`, and destructure structs completely instead of using `..Default::default()`.
- **Ownership and lifetimes:** Consume `self` to enforce call order, tie acquire/release pairs to `Drop` guards, prefer generics or associated types over `dyn Trait` downcasting, and reserve `expect` for invariants types cannot express.
- **Protect mutable state:** Keep mutable fields and interior mutability (`RefMut`, lock guards) private, expose `&[T]`, `&str`, or iterators for reads, combine check-then-act, and finish every fallible step before mutating `self`. Never add `Deref<Target = str>`, `PartialEq<&str>`, or `AsRef<str>` escape hatches on typed values.
- **Match the existing stack:** Use Serde if any serialized type uses it and `thiserror` if any error type uses it, otherwise default to Facet. Ask the user before adding a parallel stack. Serialize only durable domain facts or explicit transport values (never locks, handles, caches, or callbacks), keep wire and persisted shapes stable across refactors, and validate complete replacements before publishing.
- **Flat flow and single-owner async:** Return early, drop `else` after a return, and skip single-line forwarding or error-wrapping helpers. Centralize async mutations behind one owner (see [synchronization.md](references/synchronization.md) before adding locks, atomics, or channels).

## Naming

- **Methods:** Use `noun_verb` (`source_get`, `source_remove`), omitting the noun when the receiver makes the target clear (`get`, `get_mut`). Put context parameters first after the receiver.
- **Types and fields:** Name fields by contents (`sources`, `assets`, never `data`), name monotonic counts `<domain>_count`, and skip single-caller `with_*` constructors or `Typed` prefixes.

## Crate Research

- Research new crates and their APIs through the `docs` MCP server (`docs-mcp`) before falling back to GitHub source.

## Code Comments

- **Doc comments (`///`):** Document every function and every non-function item visible outside its module using the `technical-writing` API profile. Open function and method docs with `Get`, `Create`, or an imperative verb (never `Return`/`Returns`), and place doc comments above all attributes (`#[derive]`, `#[serde]`, `#[facet]`, `#[cfg]`, attribute macros).
- **Safety and inline comments (`//`):** Place `// SAFETY:` directly above unsafe operations. Add other `//` comments only when correctness depends on non-obvious context (domain rules, lock or atomic ordering, platform quirks, non-obvious optimizations), directly above the statement, naming the invariant and failure prevented.
- **Phase labels:** In 40+ line functions spanning multiple phases (validation, mutation/publication, cleanup), mark each phase with a short imperative state-boundary comment.

```rust
// Flush before releasing the lock, or workers read the stale buffer.
writer.flush()?;
drop(guard);
```

## References

| Reference | Read before |
| --- | --- |
| [invariants.md](references/invariants.md) | Designing public mutation methods, scoped edits, `Drop` guards, borrowing transactions, or interior mutability |
| [modules.md](references/modules.md) | Splitting or promoting modules, placing test helpers or `#[path]`, ordering trait impls, or changing re-exports |
| [errors.md](references/errors.md) | Defining error types, derives, source preservation, or error conversions |
| [documentation.md](references/documentation.md) | Writing rustdoc contracts, safety comments, or Facet error messages |
| [synchronization.md](references/synchronization.md) | Adding or restructuring locks, atomics, channels, completion queues, cancellation, or shared mutable state |
| [dynamic-libraries.md](references/dynamic-libraries.md) | Changing Rust dylibs, FFI, native loading, host/plugin contracts, or hot-reload unload order |
| [testing.md](references/testing.md) | Adding, moving, or removing tests, or bounding test runtime |
| [performance.md](references/performance.md) | Setting up Criterion/Samply benchmarks, reading profiles without a GUI, or measuring optimizations |

## Verification

- **Nextest only:** Run tests with `cargo nextest run`, never `cargo test`. If `cargo nextest` is missing, report that instead of installing it or falling back.
- **No broad autofix:** Never run `cargo clippy --fix` or `cargo fix`.
- **Match dev flags:** Pass `cargo check`, `cargo clippy`, `cargo nextest run`, and `cargo build` the project's exact development flags so the build cache stays valid. Do not add `--all-targets`, `--tests`, `--all-features`, `--release`, or a custom `--target-dir` unless requested.
- **Narrowest scope:** Run the smallest relevant test selection (`cargo nextest run -p <package> <filter>`, or `-E 'test(...)'` when substrings are too broad), batching related tests in one command.
